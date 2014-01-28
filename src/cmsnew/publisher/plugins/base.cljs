(ns cmsnew.publisher.plugins.base
  (:require
   [cmsnew.publisher.datastore.core :refer [fetch-files
                                            source-file-list
                                            store-files]]
   [cmsnew.publisher.transformer.underscore-template :refer [render-template]]
   [cmsnew.publisher.source-file :as sf]
   [cmsnew.publisher.paths :as paths]
   [cmsnew.publisher.logger :refer [logger]]
   [cmsnew.publisher.rendering.base :refer [render-raw-page-without-context]]
   [cmsnew.edn-page.rendering :refer [render-edn-section]]   
   [cmsnew.publisher.util.core :refer [self-assoc map-to-key map-in]]
   [cljs.core.async :as async :refer [chan put! close!]]
   [jayq.util :refer [log]])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go]]
   [cmsnew.publisher.util.macros :refer [chan->>]]))

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
  [[old-s site]]
  (let [changed (changed-map-keys
                 [(path-etag-map (:source-file-list site))
                  (path-etag-map (:source-files old-s))])]
    (assoc site :changed-source-files changed)))

(defn changed-rendered-files
  [[old-s site]]
  (let [changed (changed-map-keys
                 [(or (:rendered-files site) {})
                  (or (:rendered-files old-s) {})])
        changed-rendered (->> changed
                              (select-keys (:rendered-files site))
                              vals)]
    (assoc site :changed-rendered-files changed-rendered)))

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
     (let [files (<! (chan->> (fetch-files (:store site) (:changed-source-files site))
                              (async/into [])))]
       (logger site "Received changed files ..." :notice)
       (update-in site [:source-files] merge (map-to-key :path files))))))


;; store-changed-rendered-files pluging

(defn store-changed-rendered-files [[_ site]]
  (let [changed-files (:changed-rendered-files site)]
    (if (zero? (count changed-files))
      site
      (let [out (chan)]
        (go
         (let [res (<! (chan->>
                        (store-files (:store site) changed-files)
                        (async/into [])))]
           (log (clj->js res)))
         (put! out site)
         (close! out))
        out))))

;; parse-pages

(defn- pages* [site]
  (->> (paths/filter-for-prefix (:source-files site) (:page-path site))
       ;; published should be separate layer of functionality
       (map #(assoc % :published true)) 
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
