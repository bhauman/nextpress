(ns cmsnew.publisher.core
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   
   [cmsnew.publisher.util.core :refer [self-assoc map-to-key find-first]]
   [cmsnew.publisher.util.async-utils :as async-util]

   [cmsnew.publisher.datastore.core :refer [
                                            fetch-files
                                            source-file-list
                                            store-source
                                            store-files
                                            fetch-file
                                            create-store]]
   
   [cmsnew.publisher.datastore.localstore :refer [LocalStore]]
   [cmsnew.publisher.datastore.s3-store :refer [S3tore]]  
   
   [cmsnew.publisher.datastore.s3 :as store]

   [cmsnew.edn-page.rendering :refer [render-edn-page]]
   
   
   [cmsnew.publisher.site :as st]   
   [cmsnew.publisher.paths :as paths]
   [cmsnew.publisher.source-file :as sf]

   [cmsnew.publisher.item-templates :as templ]

   
   [crate.core :as crate]
   [clojure.string :as string]
   [cljs.reader :refer [push-back-reader read-string]]   
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]
                   [cmsnew.publisher.util.macros :refer [chan->>]]))

;;; This file is deprecated keeping it around for the transition

(def system-defaults {
                      :store { :type :s3
                               :bucket "nextpress-demo"
                               :signing-service "http://localhost:4567"}         
                      
                      :layout-path "_layouts"
                      :partial-path "_partials"
                      :post-path "_posts"
                      :page-path "_site_src"             
                      :data-path "_data" })

;; fetching pipeline helpers

(defn get-config [site-url]
  (let [out (chan)]
    (store/get-text (str site-url "/_config.edn" )
                    (fn [e] (put! out
                                 (merge system-defaults
                                        (-> e :body read-string)))
                      (close! out)))
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

;; register extention source type



#_(defn system-flow [system]
  (chan->> (:touch-chan system)
       (log-it system (fn [x] {:msg (str "Publising site yehaw to bucket: " (:bucket system))}))
       (map< (fn [x] (source-file-list system)))
       async-util/flatten-chans
       (log-it system (fn [x] {:msg (str "Finished fetching source file list")}))
       (map< (fn [new-file-list] [(path-etag-map (vals @(:source-files system)))
                                 (path-etag-map new-file-list)]))
       (map< changed-map-keys)
       (map< (fn [file-list] (filter paths/good-file-path? file-list)))

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

#_(defn create-site-for-url [url]
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
