(ns cmsnew.publisher.plugins.base
  (:require
   [cmsnew.datastore.core :refer [fetch-files
                                  source-file-list]]
   [cmsnew.transformer.underscore-template :refer [render-template]]
   [cmsnew.publisher.source-file :as sf]
   [cmsnew.publisher.paths :as paths]
   [cmsnew.publisher.logger :refer [logger]]
   [cmsnew.publisher.rendering.base :refer [render-raw-page-without-context]]
   [cmsnew.publisher.rendering.edn-page :refer [render-edn-section]]   
   [cmsnew.util.core :refer [self-assoc map-to-key]]
   [cljs.core.async :as async])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go]]
   [cmsnew.util.macros :refer [chan->>]]))

(defn source-extention->source-type
  "Map an extention to a source type"
  [source-ext source-type]
  (fn [[_ site]]
    (when-not (get-in site [:source-ext-types (keyword source-ext)])
      (assoc-in site [:source-ext-types (keyword source-ext)] source-type))))

(defn register-page-renderer 
  "Map source type to renderer"
  [source-type render-function]
  (fn [[_ site]]
    (assoc-in site [:source-type-renderer source-type] render-function)))

(defn register-template-helper [helper-name helper-function]
  (fn [[_ site]]
    (assoc-in site [:template-helpers (keyword helper-name)] helper-function)))

;; source-file-list plugin

(defn get-source-file-list 
  "We get the source files for the system. Creates a :source-file-list entry
   with the shape [{:path \"\" :etag \"\" } ...] in the site"
  [[old-s site]]
  (go
   (logger site "Fetching site source file listing." :fetching)
   (assoc site :source-file-list
          (filter (comp paths/good-file-path? :path)
                  (<! (source-file-list (:store site)))))))

;; changed-source-files

(defn path-etag-map [maps]
  (into {} (map (juxt :path :etag) maps)))

(defn changed-map-keys [[old-map new-map :as maps]]
  (let [key-list (set (apply concat (map keys maps)))]
    (keep (fn [k] (if (not (= (old-map k) (new-map k))) k)) key-list)))

(defn changed-source-files
  "We fetch changed files. Depends on the :source-file-list and a :source-files key.
   Creates or updates a :changed-files entry in the site that is a list of the paths
   of the files that have changed"
  [[old-s site]]
  (let [changed (changed-map-keys
                 [(path-etag-map (:source-file-list site))
                  (path-etag-map (:source-files old-s))])]
    (assoc site :changed-source-files changed)))

;; fetch-changed-source-files

(defn fetch-changed-source-files
  "We fetch changed files. Depends on the :changed-source-files and a :source-files key.
   Fteches files in the :changed-source-files and merges them into the :source-files map."
  [[old-s site]]
  (if (zero? (count (:changed-source-files site)))
    site
    (go
     (logger site "Source files have changed: " :source-files-changed (:changed-source-files site))
     (logger site "Fetching changed files ..." :downloading)     
     (let [files (<! (chan->> (fetch-files site (:changed-source-files site))
                              (async/into [])))]
       (logger site "Received changed files ..." :notice)
       (update-in site [:source-files] merge (map-to-key :path files))))))


;; parse-pages

(defn- pages* [site]
  (->> (paths/filter-for-prefix (:source-files site) (:page-path site))
       (map (partial sf/parse-front-matter site))
       (map #(assoc % :page-type :page))
       (map #(self-assoc % :target-path sf/make-page-target-path))
       (map #(self-assoc % :name (fn [page] (paths/remove-prefix (:page-path site) (:path page)))))))

(defn parse-pages
  "Parses the front matter out of the pages and installs a list of
   pages into the :pages.

   Expects: { :page-path String
              :source-files [{ :body String
                               :path String } ...]
   Adds: {:pages { String {:front-matter Map
                           :page-type :page
                           :target-path String
                           :name String
                           :body etc... } ...}}"
  [[old-s site]]
  (assoc site :pages (map-to-key :name (pages* site))))

;; parse-posts

(defn- posts* [site]
  (->> (paths/filter-for-prefix (:source-files site) (:post-path site))
       (map (partial sf/parse-front-matter site))
       (map #(assoc % :page-type :post))
       (map #(self-assoc % :date paths/parse-file-date))
       (map #(self-assoc % :target-path sf/make-post-target-path))
       (map #(self-assoc % :name (fn [page] (paths/remove-prefix (:post-path site) (:path page)))))))

(defn parse-posts
  [[old-s site]]
  (assoc site :posts (map-to-key :name (posts* site))))

;; parse-layouts

(defn- layouts* [site]
  (->> (paths/filter-for-prefix (:source-files site) (:layout-path site))
       (map (partial sf/parse-front-matter site))
       (map #(self-assoc % :name (fn [page] (paths/remove-prefix (:layout-path site) (:path page)))))))

(defn parse-layouts
  "Parses the front matter out of the layouts and installs them into the :layouts key.
   The layouts are stored in a map keyd to their name.  The name for _layouts/fun/index.html
   would be fun/index.html.  
   Depends on the :source-files key and the :layout-path key."
  [[old-s site]]
  (assoc site :layouts (map-to-key :name (layouts* site))))

;; parse partials

(defn- partials* [site]
  (->> (paths/filter-for-prefix (:source-files site) (:partial-path site))
       (map (partial sf/parse-front-matter site))
       (map #(self-assoc % :name (fn [page] (paths/remove-prefix (:partial-path site) (:path page)))))))

(defn parse-partials
  [[old-s site]]
  (assoc site :partials (map-to-key :name (partials* site))))

;; parse data-files

(defn- data-files* [site]
  (->> (paths/filter-for-prefix (:source-files site) (:data-path site))
       (map (partial sf/parse-front-matter site))
       (map #(self-assoc % :name (fn [page]
                                   (paths/remove-extention
                                    (paths/remove-prefix (:data-path site) (:path page))))))))

(defn parse-data-files
  [[_ site]]
  (assoc site :data (map-to-key :name (data-files* site))))

;; template environment processing

(defn data-into-templ-env
  "Templates have the data from the files in the data directory directly available by
   using the name of the file to access them"
  [[_ site]]
  (assoc site :template-env
         (zipmap (keys (:data site))
                 (map :front-matter (vals (:data site))))))


;; bringing in base page data

(defn pages-into-templ-env
  [[_ site]]
  (assoc-in site [:template-env :site :pages]
            (map
             (fn [f]
               (merge
                (select-keys f [:name :path :body])
                { :url (str "/" (sf/make-target-path f))
                 :id (str "/" (-> f sf/make-target-path (paths/replace-extention "")))}))
             (vals (:pages site)))))

;; bringing in base post data

(defn posts-into-templ-env
  [[_ site]]
  (assoc-in site [:template-env :site :posts]
            (->> (vals (:posts site))
                 (map 
                  (fn [f]
                    (assoc
                        (select-keys f [:name :path :body :date])
                      :url (str "/" (sf/make-target-path f))
                      :id (str "/" (-> f sf/make-target-path (paths/replace-extention ""))))))
                 (sort-by #(paths/date-to-int (:date %)))
                 reverse)))

(defn map-in [col key f]
  (update-in col key (fn [c] (map f c))))


(defn- merge-front-matter-to-temple-env* [file-list-key site]
  (map-in site [:template-env :site file-list-key]
          (fn [{:keys [name] :as file}]
            (merge file
              (:front-matter (get-in site [file-list-key name]))))))

(defn merge-front-matter-into-page-templ-env [[_ site]]
  (merge-front-matter-to-temple-env* :pages site))

(defn merge-front-matter-into-post-templ-env [[_ site]]
  (merge-front-matter-to-temple-env* :posts site))

;; add rendered content to templ post and page data

(defn- add-rendered-content-into-templ-env* [file-list-key site]
  (map-in site [:template-env :site file-list-key]
          (fn [{:keys [name] :as file}]
            (assoc file
              :content
              (render-raw-page-without-context site (get-in site [file-list-key name]))))))

(defn add-rendered-content-to-pages-templ-env [[_ site]]
  (add-rendered-content-into-templ-env* :pages site))

(defn add-rendered-content-to-posts-templ-env [[_ site]]
  (add-rendered-content-into-templ-env* :posts site))

;; add sections to page templ data


(defn sections-from-items [system-data items]
  (let [temp-items (drop-while #(not= (:type %) :section) items)
        section-header (first temp-items)
        section-items (take-while #(not= (:type %) :section) (rest temp-items))]
    (if section-header
      (cons { :name    (:content section-header)
              :items   section-items 
              :content (render-edn-section system-data section-items) }
            (sections-from-items system-data (rest temp-items)))
      [])))

(defn get-sections [system-data source-file]
  (if (sf/edn-page? source-file)
    (sections-from-items system-data (sf/items source-file))
    []))

(defn page-sections [[_ site]]
  (map-in site [:template-env :site :pages]
          (fn [file]
            (let [sections (get-sections site (get-in site [:pages (:name file)]))
                  sections-map (into {} (map (juxt :name :content) sections))]
              (assoc file
                :sections sections
                :sectionsMap sections-map
                :getSection (fn [k] (get sections-map k)))))))

;; render partials

;; this is not a good idea as the env is not handled well
(defn render-partials [[_ site]]
  (assoc-in site [:template-env :renderPartial]
         (fn rendPartial [partial-path partial-data]
           (if-let [template ((:partials site) partial-path)]
             (let [env (merge (:template-env site)
                              {:renderPartial rendPartial}
                              (or (js->clj partial-data) 
                                  {}))]
               (render-template (:body template) env))
             "null partial"))))
