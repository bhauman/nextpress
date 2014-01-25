(ns cmsnew.publisher.core
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [cmsnew.util.core :refer [self-assoc map-to-key find-first]]
   [cmsnew.datastore.s3 :as store]
   [cmsnew.transformer.markdown :refer [markdown-to-html]]
   [cmsnew.util.async-utils :as async-util]
   [cmsnew.publisher.site :as st]   
   [cmsnew.publisher.paths :as paths]
   [cmsnew.publisher.source-file :as sf]

   [cmsnew.publisher.item-templates :as templ]

   ;; importing edn-items
   [cmsnew.edn-page.item :refer [deleted? render-item]]
   [cmsnew.edn-page.items.heading]
   [cmsnew.edn-page.items.markdown]
   [cmsnew.edn-page.items.section]
   [cmsnew.edn-page.items.image]
   
   [crate.core :as crate]
   [clojure.string :as string]
   [cljs.reader :refer [push-back-reader read-string]]   
   [jayq.util :refer [log]]
   [goog.crypt.Md5])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]
                   [cmsnew.util.macros :refer [chan->>]]))

(def system-defaults {
                      :store { :type :s3
                               :bucket "immubucket"
                               :signing-service "http://localhost:4567"}         
                      
                      :layout-path "_layouts"
                      :partial-path "_partials"
                      :post-path "_posts"
                      :page-path "_site_src"             
                      :data-path "_data" })

;; storage-system

(defn starts-with? [s prefix]
  (zero? (.indexOf s prefix)))

(let [md (goog.crypt.Md5.)]
  (defn md5 [s]
    (.update md s)
    (apply
     str
     (map (fn [x]
            (str
             (if (> 16 x) "0" "")
             (.toString x 16))) (.digest md)))))

(defprotocol FileLister
  (list-files [this callback])  
  (list-files-with-prefix [this prefix callback]))

(defprotocol PutFile
  (-store! [this path data options callback])
  (-store-response-success? [this response])
  (-store-response-version [this response])) 

(defprotocol GetFile
  (-get-file [this path callback]))

(defprotocol ToSourceFile
  (->source-file [this path file-response]))

(defprotocol ResourcePath
  (resource-path [this path]))

(defrecord LocalStore [path-prefix]
  FileLister
  (list-files [this callback]
    (callback
     (map (fn [x] {:path x
                  :etag (md5
                         (.getItem js/localStorage
                                   (str (:path-prefix this) "::" x)))})
          (map (fn [x] (string/replace-first x (str path-prefix "::") ""))
               (filter (fn [x] (starts-with? x (str path-prefix "::")))
                       (map #(.key js/localStorage %)
                            (range (.-length js/localStorage))))))))
  (list-files-with-prefix [this prefix callback]
    (list-files this
     (fn [files]
       (callback
        (filter (fn [x] (starts-with? (:path x) prefix))
              files)))))
  PutFile
  (-store! [this path data options callback]
    (.setItem js/localStorage
              (resource-path this path)
              data)
    (callback data))
  (-store-response-version [this resp] nil)
  (-store-response-success? [this resp] true)
  GetFile
  (-get-file [this path callback]
    (callback
     (.getItem js/localStorage
               (resource-path this path))))
  ToSourceFile
  (->source-file [this path file-response]
    {:path path
     :etag (md5 file-response)
     :body file-response})
  ResourcePath
  (resource-path [this path]
    (str (:path-prefix this) "::" path)))

(defrecord S3Store [bucket signing-service]
  FileLister
  (list-files [this callback]
    (cmsnew.datastore.s3/list-files this "" callback))
  (list-files-with-prefix [this prefix callback]
    (cmsnew.datastore.s3/list-files this prefix callback))
  PutFile
  (-store! [this path data options callback]
    (store/save-data-to-file this
                             path
                             data
                             (or (:mime-type options) "text/plain")
                             callback))
  (-store-response-version [this resp]
    (.getResponseHeader resp "x-amz-version-id"))
  (-store-response-success? [this resp]
    (log resp)
    true)
  GetFile
  (-get-file [this path callback]
    (store/get-text (resource-path this path) callback))
  ToSourceFile
  (->source-file [this path file-response]
    {:path path
     :etag (get-in file-response [:headers :etag])
     :version (get-in file-response [:headers :version])
     :body (:body file-response)})
  ResourcePath
  (resource-path [this path]
    (str "http://s3.amazonaws.com/" bucket "/" path)))

(defn get-source-file [st path callback]
  (-get-file st path (fn [f] (callback (->source-file st path f)))))

(defn store-source-file! [st source-file callback]
  (-store! st
           (:path source-file)
           (str (prn-str (:front-matter source-file)) (:body source-file))
           {:mime-type "text/plain"}
           (fn [store-resp]
             (if (-store-response-success? st store-resp)
               (callback (assoc source-file :version
                                (-store-response-version st store-resp))) 
               (callback :failed)))))

(defn store-rendered-file! [st source-file callback]
  (-store! st
           (:target-path source-file)
           (:rendered-body file-map)           
           {:mime-type "text/html"}
           (fn [store-resp]
             (if (-store-response-success? st store-resp)
               (callback (assoc source-file :target-version
                                (-store-response-version st store-resp))) 
               (callback :failed)))))


(defmulti create-store #(:type %))

(defmethod create-store :local [{:keys [path-prefix]}]
  (LocalStore. path-prefix))

(defmethod create-store :s3 [{:keys [bucket signing-service]}]
  (S3Store. bucket signing-service))


(defn render-template [template-string data]
  (.template js/_ template-string (clj->js data)))

;; fetching pipeline helpers

(defn fetch-file [system file]
  (let [out (chan)]
    (store/get-text (item-path (:store system) file)
                    (fn [resp] (put! out {:path file
                                         :etag    (get-in resp [:headers :etag])
                                         :version (get-in resp [:headers :version])
                                         :body (:body resp)}) (close! out)))
    out))

(defn fetch-files [system paths]
  (async/merge (map (partial fetch-file system) paths)))

(defn store-rendered-file [system file-map]
  (let [out (chan)]
    (store/save-data-to-file (:s3-store system)
                             (:target-path file-map)
                             (:rendered-body file-map)
                             "text/html"
                             (fn [e] (let [version
                                          (.getResponseHeader e "x-amz-version-id")]
                                      (put! out (assoc file-map :rendered { :version version }))
                                      (close! out)
                                      )))  
    out))

#_(defn store-source-file [system file-map]
  (let [out (chan)]
    (store/save-data-to-file (:s3-store system)
                             (:path file-map)
                             (str (prn-str (:front-matter file-map)) (:body file-map))
                             "text/plain"
                             (fn [e] (let [version
                                          (.getResponseHeader e "x-amz-version-id")]
                                      (put! out (assoc file-map :current-version { :version version }))
                                      (close! out)
                                      )))  
    out))

(defn store-files [system file-maps-list]
  (async/merge (map (partial store-rendered-file system) file-maps-list)))

(defn get-config [site-url]
  (let [out (chan)]
    (store/get-text (str site-url "/_config.edn" )
                    (fn [e] (put! out
                                 (merge system-defaults
                                        (-> e :body read-string)))
                      (close! out)))
    out))

;; rendering pages

(defn template-for-item-type [system-data item]
  ;; this is getting pretty arbitrary we should probably have full
  ;; paths to everything and use those as keys
  ((:partials system-data)
   (or (and (not (string/blank? (:partial item))) (:partial item))
       (str "items/" (name (:type item))))))

(defn render-item-with-template-overide [system-data item]
  (if-let [template (template-for-item-type system-data item)]
    (crate/raw (render-template (:body template) item))    
    (render-item item)))

(defn render-edn-page [system-data page-file-map]
  (.-outerHTML
   (crate/html
    (templ/item-list "list-1" "list-1"
                     (map (partial render-item-with-template-overide system-data)
                          (get-in page-file-map
                                              [:front-matter :items]))))))

(defn render-edn-section [system-data items]
  (.-outerHTML
   (crate/html
    [:div
     (map (partial render-item-with-template-overide system-data) items)])))

(defn render-raw-page [system-data page-file-map data-for-page]
  (condp = (-> page-file-map :path paths/extention-from-path)
    "md"    (markdown-to-html (:body page-file-map))
    "html"  (render-template (:body page-file-map)
                             data-for-page)
    "edn"   (render-edn-page system-data page-file-map)
    (:body page-file-map)))

(defn render-raw-page-without-context [system-data page-file-map]
  (condp = (-> page-file-map :path paths/extention-from-path)
    "md"    (render-raw-page system-data page-file-map {})
    "edn"   (render-raw-page system-data page-file-map {})
    (:body page-file-map)))

(defn sections-from-items [system-data items]
  (let [temp-items (drop-while #(not= (:type %) :section) items)
        section-header (first temp-items)
        section-items (take-while #(not= (:type %) :section) (rest temp-items))]
    (if section-header
      (cons { :name    (:content section-header)
              :items   section-items 
              :content (render-edn-section system-data section-items) }
            (sections-from-items system-data (rest temp-items)))
      nil)))

(defn get-sections [system-data source-file]
  (if (sf/edn-page? source-file)
    (sections-from-items system-data (sf/items source-file))
    (list)))

(defn file-to-page-data [system-data {:keys [body front-matter date] :as fm}]
  (let [{:keys [title]} front-matter
        sections (get-sections system-data fm)
        sections-map (into {} (map (juxt :name :content) sections))
        sections-items-map (into {} (map (juxt :name :items) sections))]
    (merge { :content (render-raw-page-without-context system-data fm)
             :sections sections
             :sectionsMap sections-map
             :getSection (fn [name] (get sections-map name))
             :getSectionItems (fn [name] (clj->js (get sections-items-map name)))            
             :body (:body fm)
             :url (str "/" (sf/make-target-path fm))
             :path (:path fm)
             :date date
             :id   (str "/" (-> fm sf/make-target-path (paths/replace-extention "")) )}
           front-matter)))

(defn template-data [system-data]
  (let [data-files (system-data :data)
        posts (->> (:posts system-data)
                   (map (partial file-to-page-data system-data))
                   (sort-by #(paths/date-to-int (:date %)))
                   reverse)
        pages (map (partial file-to-page-data system-data) (:pages system-data))
        data-file-map (zipmap (keys data-files) (map :data (vals data-files)))
        site (assoc (get-in system-data [:site :config-file-data])
               :posts posts
               :pages pages)
        include-page (fn [page-path] (->> pages
                                         (find-first #(= (:path %) page-path))
                                         :content))
        get-page (fn [page-path] (clj->js (->> pages
                                              (find-first #(= (:path %) page-path)))))
        all-data (merge
                  data-file-map
                  { :site site
                    :includePage include-page
                    :getPage get-page})
        render-partial (fn rendPartial [partial-path partial-data]
                         (if-let [template ((:partials system-data) partial-path)]
                           (let [env (merge all-data
                                            {:renderPartial rendPartial}
                                            (or (js->clj partial-data) 
                                                {}))]
                             (render-template (:body template) env))
                           "null partial"))]
    (assoc all-data
      :renderPartial render-partial)
    ))

(defn render-page-with-templates [system-data data-for-templates page-file-map]
  (let [start-template (get-in page-file-map [:front-matter :layout])
        data-for-page (merge {:page (file-to-page-data system-data page-file-map)}
                             data-for-templates)]
    (loop [template start-template
           content (render-raw-page system-data page-file-map data-for-page)]
      (let [template-file-map (get (system-data :templates) template)]
        (if (nil? template-file-map)
          content
          (recur
           (get-in template-file-map [:front-matter :layout])
           (render-template (:body template-file-map)
                            (assoc data-for-page :content content)))
          )))))

;; processing system

(defn process [site files]
  (let [system-data
        { :templates  (map-to-key :name (st/templates site))
          :partials   (map-to-key :name (st/partials site))
          :data       (map-to-key :name (st/data-files site))
          :posts      (st/posts site)
          :pages      (st/pages site)
          :site     site }
        data-for-templates (template-data system-data)
        files-to-publish (filter sf/publish?
                                 (concat (:posts system-data)
                                         (:pages system-data)))]
    #_(log "SYSTEM DATA")
    #_(log (clj->js system-data))
     (->> files-to-publish
          (map #(self-assoc % :rendered-body
                           (partial render-page-with-templates
                                    system-data
                                    data-for-templates)))
          (map (juxt :path identity))
          (into {}))))

; working on detecting file changes

(let [dir-path-rx #"/$"
      hash-rx #"\#$"]
  (defn good-file-path? [path]
    (not (or (.test dir-path-rx path)
             (.test hash-rx path)))))

(defn fetch-file-list [system input]
  (let [out (chan)]
    (go-loop [file-list (<! input)]
             (put! out (<! (chan->> (fetch-files system file-list)
                                    (async/into []))))
             (recur (<! input)))
    out))

;; local storage

(defn local-storage-set [key clj-val]
  (.setItem (.-localStorage js/window) (name key) (prn-str clj-val)))

(defn local-storage-get [key]
  (let [x (.getItem (.-localStorage js/window) (name key))]
    (when x (read-string x))))

(defn local-storage-remove [key]
  (.removeItem (.-localStorage js/window) (name key)))

;; setting up reactive system

(defn log-chan [input]
  (map< #(do (log (prn-str %)) %) input))

(defn path-etag-map [maps]
  (into {} (map (juxt :path :etag) maps)))

(defn changed-map-keys [[old-map new-map :as maps]]
  (let [key-list (set (apply concat (map keys maps)))]
    (keep (fn [k] (if (not (= (old-map k) (new-map k))) k)) key-list)))

(defn log-it [system func in-chan]
  (map< (fn [x] (put! (:log-chan system) (func x)) x)
        in-chan))

;; beginings

(defrecord LogMsg [msg type data])

(defn logger
  ([system msg typ data]
     (when-let [log-chan (:log-chan system)]
       (put! log-chan (LogMsg. msg typ data))))
  ([system msg typ]
     (logger system msg typ nil))
  ([system msg]
     (logger system msg nil nil)))



;; source-file-list plugin

(defn source-file-list [system]
  (let [out (chan)]
    (store/list-files (system :store) "_" #(do (put! out %) (close! out)))
    out))

(defn get-source-file-list 
  "We get the source files for the system. Creates a :source-file-list entry
   with the shape [{:path \"\" :etag \"\" } ...] in the site"
  [[old-s site]]
  (go
   (logger site "Fetching site source file listing." :fetching)
   (assoc site :source-file-list
          (filter (comp good-file-path? :path)
                  (<! (source-file-list site))))))

;; changed-source-files

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
       (logger site "Recieved changed files ..." :notice)
       (update-in site [:source-files] merge (map-to-key :path files))))))

#_{ :templates  (map-to-key :name (st/templates site))
    :partials   (map-to-key :name (st/partials site))
    :data       (map-to-key :name (st/data-files site))
    :posts      (st/posts site)
    :pages      (st/pages site)
    :site     site }

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

;; parse-layouts

(defn- layouts* [site]
  (->> (paths/filter-for-prefix (:source-files site) (:layout-path site))
       (map (partial sf/parse-front-matter site))
       (map #(self-assoc % :name sf/full-filename-without-ext))))

(defn parse-layouts
  "Parses the front matter out of the layouts and installs them into the :layouts key.
   The layouts are stored in a map keyd to their name.  The name for _layouts/fun/index.html
   would be fun/index.html.  
   Depends on the :source-files key and the :layout-path key."
  [[old-s site]]
  (assoc site :layouts (map-to-key :name (layouts* site))))

(defn piper< [f input]
  (let [out (chan)]
    (go-loop []
             (let [v (<! input)]
               (if v
                 (let [[o n] v
                       res (f [o n])]
                   (put! out [o (if (.-takes res) (<! res) res)])
                   (recur))
                 (close! out))))
    out))

(defn render-pipeline [system-chan]
  (chan->> system-chan
           (map< (juxt identity identity))
           (piper< get-source-file-list)
           (piper< changed-source-files)
           (piper< fetch-changed-source-files)
           #_(piper< parse-pages)           
           
           ))

(defn system-flow [system]
  (chan->> (:touch-chan system)
       (log-it system (fn [x] {:msg (str "Publising site yehaw to bucket: " (:bucket system))}))
       (map< (fn [x] (source-file-list system)))
       async-util/flatten-chans
       (log-it system (fn [x] {:msg (str "Finished fetching source file list")}))
       (map< (fn [new-file-list] [(path-etag-map (vals @(:source-files system)))
                                 (path-etag-map new-file-list)]))
       (map< changed-map-keys)
       (map< (fn [file-list] (filter good-file-path? file-list)))

       (map< #(do
                (when (zero? (count %))
                  (swap! (:finished-publishing system) inc)) 
                %))
       
       (filter< #(pos? (count %)))
       (log-it system (fn [x] {:msg (str "Source files have changed: ") :list-data x :type :source-files-changed}))
       
       (log-it system (fn [x] {:msg (str "Fetching changed files ...") :type :downloading}))
       (fetch-file-list system)
       (log-it system (fn [x] {:msg (str "Received changed files ...") :type :notice}))
       (map< #(map-to-key :path %))
       (map< (fn [file-map]
               (swap! (:source-files system) merge file-map)
               @(:source-files system)))
       (log-it system (fn [x] {:msg (str "Rendering site ...") :type :processing}))
       (map< (partial process system))

              
       (async-util/map-to-atom (:rendered-files system))

       ;; this atom contains the rendered files
       async-util/atom-chan
       (map< (fn [[ov nv]] (let [res (->> (changed-map-keys [ov nv])
                               (select-keys nv)
                               vals)]
                            (or res []))))
       (log-it system (fn [x] (if (pos? (count x))
                               {:msg (str "Rendered pages have changed") :type :changes-detected}
                               {:msg "No rendered pages have changed" :type :published})))

       (map< #(do
                (when (zero? (count %))
                  (swap! (:finished-publishing system) inc)) 
                %))
       
       (filter< #(pos? (count %)))
       (log-it system (fn [x] {:msg (str "Uploading rendered pages to site: ")
                              :list-data (map :target-path x)
                              :type :uploading }))
       (map< (fn [files-to-store]
               (chan->> (store-files system files-to-store)
                        (async/into []))))
       (log-it system (fn [x]
                        {:msg (str "Site changes published") :type :published}))
       (map< #(do (swap! (:finished-publishing system) inc)  %))
       (async/into [])))

(defn localstorage-source-files-key [site]
  (keyword (str (:bucket site) "-next-press-source-files")))

(defn localstorage-rendered-files-key [site]
  (keyword (str (:bucket site) "-next-press-rendered-files")))

(defn- obtain-source-files [site]
  (atom (or (local-storage-get (localstorage-source-files-key site))
            {})))

(defn- obtain-rendered-files [site]
  (atom (or (local-storage-get (localstorage-rendered-files-key site))
            {})))

(defn publish [{:keys [touch-chan]}]
  (put! touch-chan 1))

(defn publish-it [{:keys [touch-chan finished-publishing]} callback]
  (let [key (keyword (gensym "site-published"))]
    (add-watch finished-publishing key (fn [_ _ _ _]
                                         (callback)
                                         (remove-watch finished-publishing key)))
    (put! touch-chan 1)))

(defn blocking-publish [site]
  (let [out (chan)]
    (publish-it site #(do (put! out 1) (close! out)))
    out))

(defn clear-cache [site]
  (local-storage-remove (localstorage-source-files-key site))
  (reset! (:source-files site) {}))

(defn force-publish [site]
  (go
   (clear-cache site)
   (<! (timeout 500))
   (publish site)))

(defn create-site-for-url [url]
  (go
   (let [config (<! (get-config url))
         site (assoc config
                       :config-file-data config
                       :site-url url
                       :s3-store (store/create-s3-store (:signing-service config) (:bucket config))
                       :touch-chan (chan)
                       :log-chan (chan)
                       :finished-publishing (atom 0)
                       :source-files (obtain-source-files config)
                       :rendered-files (obtain-rendered-files config))]
     (add-watch (:source-files site) :files-changed
                (fn [_ _ o n] (local-storage-set (localstorage-source-files-key site) n)))
     (add-watch (:rendered-files site) :fields-changed
                (fn [_ _ o n] (local-storage-set (localstorage-rendered-files-key site)  n)))
     (system-flow site)
     (go-loop []
              (let [msg (<! (:log-chan site))]
                (log (:msg msg))
                (recur)))
     site)))

#_(let [lc (chan)
      in (to-chan [{ :log-chan lc
                     :page-path "_site_src"
                     :layout-path "_layouts"
                     :store { :type :s3
                              :bucket "nextpress-demo"
                              :signing-service "http://localhost:4567"
                              :store-root "http://s3.amazonaws.com"}
                    }])
      p (render-pipeline in)]
  (go-loop []
        (let [msg (<! lc)]
          (when msg
            (log (:msg msg))
            (recur))))
    
  (go (log (clj->js (:source-files (last (<! p)))))))




(let [st (S3Store. "nextpress-demo"
                   "http://localhost:4567")]
  #_(store! st "_data1.txt" "1" identity)
  #_(store! st "data2" 2 identity)
  #_(store! st "data3" 3 identity)

  #_(store! st "rata1" 1 identity)
  #_(store! st "rata2" 2 identity)
  #_(store! st "rata3" 3 identity)
  
  (get-source-file st "_config.edn" (fn [x] (log x)
                                      (store-source-file! st x (fn [result] (log result) ))
                                      ))
  (list-files st (fn [list] (log (clj->js list))))
  (list-files-with-prefix st "_" (fn [list] (log (clj->js list))))) 

