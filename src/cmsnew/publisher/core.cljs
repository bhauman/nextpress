(ns cmsnew.publisher.core
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [cmsnew.util.core :refer [self-assoc map-to-key find-first]]
   [cmsnew.datastore.s3 :as store]
   [cmsnew.transformer.markdown :refer [markdown-to-html]]
   [cmsnew.publisher.item-templates :as templ]
   [cmsnew.util.async-utils :as async-util]
   [cmsnew.publisher.site :as st]   
   [cmsnew.publisher.paths :as paths]
   [cmsnew.publisher.source-file :as sf]   
   [crate.core :as crate]
   [clojure.string :as string]
   [cljs.reader :refer [push-back-reader read-string]]   
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(def system-defaults {
             :bucket "immubucket"
             :store-root "http://s3.amazonaws.com"
             :template-path "_layouts"
             :post-path "_posts"
             :page-path "_site_src"             
             :data-path "_data" })

(defn bucket-path [{:keys [store-root bucket]}]
  (str store-root "/" bucket))

(defn item-path [system path]
  (str (bucket-path system) "/" path))

(defn template-path [system path]
  (item-path system (str (:template-path system) "/" path)))

(defn post-path [system path]
  (item-path system (str (system :post-path) "/" path)))

(defn page-path [system path]
  (item-path system (str (system :page-path) "/" path)))

(defn render-template [template-string data]
  (.template js/_ template-string (clj->js data)))

;; fetching pipeline helpers

(defn file-list [bucket path-prefix]
  (let [out (chan)
        not-prefix? (fn [x] (not (or (= (str path-prefix "/") x)
                                    (= path-prefix x))))]
    (store/get-bucket-list bucket path-prefix
                           (fn [path-etags]
                             (async/onto-chan out (filter not-prefix?
                                                          (map :path path-etags)))))
    out))

(defn fetch-file [system file]
  (let [out (chan)]
    (store/get-text (item-path system file)
                    (fn [resp] (put! out {:path file
                                         :etag    (get-in resp [:headers :etag])
                                         :version (get-in resp [:headers :version])                                         
                                         :body (:body resp)}) (close! out)))
    out))

(defn fetch-files [system paths-chan]
  (let [out (chan)
        file-chans (map< (partial fetch-file system) paths-chan)]
    (go (async/pipe (async/merge (<! (async/into [] file-chans))) out))
    out))

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

(defn store-source-file [system file-map]
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

(defn store-files [system file-maps-chan]
  (let [out (chan)
        store-chans (map< (partial store-rendered-file system) file-maps-chan)]
    (go (async/pipe (async/merge (<! (async/into [] store-chans))) out))
    out))

(defn get-config [site-url]
  (let [out (chan)]
    (store/get-text (str site-url "/_config.edn" )
                    (fn [e] (put! out
                                 (merge system-defaults
                                        (-> e :body read-string)))
                      (close! out)))
    out))

;; rendering pages

(defn render-edn-page [page-file-map]
  (.-outerHTML
   (crate/html
    (templ/item-list "list-1" "list-1"
                     (map templ/render-item (get-in page-file-map
                                                    [:front-matter :items]))))))

(defn render-edn-section [items]
  (.-outerHTML
   (crate/html
    [:div
     (map templ/render-item items)])))

(defn render-raw-page [page-file-map data-for-page]
  (condp = (-> page-file-map :path paths/extention-from-path)
    "md"    (markdown-to-html (:body page-file-map))
    "html"  (render-template (:body page-file-map)
                             data-for-page)
    "edn"   (render-edn-page page-file-map)
    (:body page-file-map)))

(defn render-raw-page-without-context [page-file-map]
  (condp = (-> page-file-map :path paths/extention-from-path)
    "md"    (render-raw-page page-file-map {})
    "edn"   (render-raw-page page-file-map {})
    (:body page-file-map)))

(defn sections-from-items [items]
  (let [temp-items (drop-while #(not= (:type %) :section) items)
        section-header (first temp-items)
        section-items (take-while #(not= (:type %) :section) (rest temp-items))]
    (if section-header
      (cons { :name    (:content section-header)
              :content (render-edn-section section-items) }
            (sections-from-items (rest temp-items)))
      nil)))

(defn get-sections [source-file]
  (if (sf/edn-page? source-file)
    (sections-from-items (sf/items source-file))
    (list)))

(defn file-to-page-data [{:keys [body front-matter date] :as fm}]
  (let [{:keys [title]} front-matter
        sections (get-sections fm)
        sections-map (into {} (map (juxt :name :content) sections))]
    (merge { :content (render-raw-page-without-context fm)
             :sections sections
             :sectionsMap sections-map
             :getSection (fn [name] (get sections-map name))
             :body (:body fm)
             :url (str "/" (sf/make-target-path fm))
             :path (:path fm)
             :date date
             :id   (str "/" (-> fm sf/make-target-path (paths/replace-extention "")) )}
           front-matter)))

(defn template-data [system-data]
  (let [data-files (system-data :data)
        posts (->> (:posts system-data)
                   (map file-to-page-data)
                   (sort-by #(paths/date-to-int (:date %)))
                   reverse)
        pages (map file-to-page-data (:pages system-data))
        data-file-map (zipmap (keys data-files) (map :data (vals data-files)))]
    (merge
       data-file-map
       { :site { :posts posts
                 :pages pages }
         :includePage (fn [page-path] (->> pages
                                          (find-first #(= (:path %) page-path))
                                          :content))
         :getPage (fn [page-path] (clj->js (->> pages
                                               (find-first #(= (:path %) page-path)))))
        })))

(defn render-page-with-templates [system-data data-for-templates page-file-map]
  (let [start-template (get-in page-file-map [:front-matter :layout])
        data-for-page (merge {:page (file-to-page-data page-file-map)}
                             data-for-templates)]
    (loop [template start-template
           content (render-raw-page page-file-map data-for-page)]
      (let [template-file-map (get (system-data :templates) template)]
        (if (nil? template-file-map)
          content
          (recur
           (get-in template-file-map [:front-matter :layout])
           (render-template (:body template-file-map)
                            (assoc data-for-page :content content)))
          )))))

;; data for environment





;; processing system

(defn process [system files]
  (let [system-data
        { :templates  (map-to-key :name (st/templates system))
         :data       (map-to-key :name (st/data-files system))
         :posts      (st/posts system)
         :pages      (st/pages system)
         :system     system }
        data-for-templates (template-data system-data)
        files-to-publish (filter sf/publish?
                                 (concat (:posts system-data)
                                         (:pages system-data)))]
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
             (put! out (<! (->> (to-chan file-list)
                                (fetch-files system)
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

(defn source-file-list [system]
  (let [out (chan)]
    (store/get-bucket-list (system :bucket) "_" #(do (put! out %) (close! out)))
    out))

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

(defn system-flow [system]
  (->> (:touch-chan system)
       (log-it system (fn [x] {:msg (str "Publising site to bucket: " (:bucket system))}))
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
               (->> (to-chan files-to-store)
                    (store-files system)
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
     #_(go-loop []
                (let [msg (<! (:log-chan site))]
                  (log (:msg msg))
                  (recur)))
     site)))

#_(go
 (let [site (<! (create-site-for-url
                 "http://immubucket.s3-website-us-east-1.amazonaws.com"))]
   (log site)
   (force-publish site)))


#_(go-loop []
         (publish)
         (<! (timeout 5000))
         (recur))

