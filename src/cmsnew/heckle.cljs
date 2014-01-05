(ns cmsnew.heckle
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [cmsnew.datastore.s3 :as store]
   [cmsnew.markdown :refer [markdown-to-html]]
   [cmsnew.templates :as templ]
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

;; getting front matter

(defn get-front-matter [reader]
  (try
    (let [front-matter-map (cljs.reader/read reader true nil false)]
      (if (map? front-matter-map) front-matter-map false))
    (catch js/Object e
      (.log js/console e) ; consider using an error channel
      false)))

(defn parse-front-matter [file-map]
  (let [r (push-back-reader (:body file-map))]
    (if-let [front-matter (get-front-matter r)]
      (assoc file-map
        :front-matter front-matter
        :body (.substr (:body file-map) (inc (.-idx r))))
      file-map)))

;; date parsing

(def filename-parts #(string/split % #"-"))

(defn parse-date [filename]
  (if (.test #"^\d{4}-\d{2}-\d{2}-" filename)
    (zipmap [:year :month :day]
            (map js/parseInt (take 3 (filename-parts filename))))
    {}))

(defn date-to-int [{:keys [year month day]}]
  (+ (* 10000 year) (* 100 month) day))

(defn has-date? [fm] (and (:date fm) (-> fm :date :month)))

(def path-parts #(string/split % #"/"))

(defn parse-file-date [{:keys [path]}]
  (parse-date (last (path-parts path))))

;; handle path rewriting

(def filename-from-path (comp last path-parts))

(defn extention-from-path [path]
  (-> path
      (string/split #"\.")
      last))

(defn dated-post-target-filename [path]
  (->> path
       filename-from-path
       filename-parts
       (drop 3)
       (string/join "-" )))

(defn str-join [coll sep] (string/join sep coll))

(defn replace-extention [path new-ext]
  (-> path
      (string/split #"\.")
      butlast
      (str-join ".") 
      (str new-ext)))

(defn make-post-target-path [{:keys [date path] :as fm}]
  (replace-extention
   (if (has-date? fm)
     (string/join "/"
                  [(:year date) (:month date) (:day date)
                   (dated-post-target-filename path)])
     (filename-from-path path))
   ".html"))

(defn make-page-target-path [{:keys [path] :as fm}]
  (replace-extention
   (->> path
        path-parts
        rest
        (string/join "/"))   
   ".html"))

(defn make-target-path [fm]
  (condp = (fm :page-type)
    :post (make-post-target-path fm)
    (make-page-target-path fm)
    ))

;; parse data file

(defn parse-data-file [file-map]
  (try
    (assoc file-map
      :data (read-string (:body file-map))
      :name (-> (:path file-map)
                filename-from-path
                (replace-extention "")))
    (catch js/Object e
      (.log js/console e) ; consider using an error channel
      file-map)))

;; fetching pipeline helpers

(defn self-assoc [x key f]
  (assoc x key (f x)))

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
    (store/save-data-to-file (:target-path file-map)
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
    (store/save-data-to-file (:path file-map)
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

(defn filename-without-ext [{:keys [path]}]
  (-> path
      filename-from-path
      (replace-extention "")))

(defn edn-page? [fpm]
  (= "edn" (last (string/split (:path fpm) #"\."))))

(defn get-config [site-url]
  (let [out (chan)]
    (store/get-text (str site-url "/_config.edn" )
                    (fn [e] (put! out
                                 (merge system-defaults
                                        (-> e :body read-string)))
                      (close! out)))
    out))

;; rendering pages

(defn map-to-key [key x]
  (zipmap (map key x) x))

(defn file-to-page-data [{:keys [body front-matter date] :as fm}]
  (let [{:keys [title]} front-matter]
    (merge { :content body
             :url (str "/" (make-target-path fm))
             :date date
             :id   (str "/" (-> fm make-target-path (replace-extention "")) )}
           front-matter)))

(defn template-data [system-data]
  (let [data-files (system-data :data)]
    (merge
       (zipmap (keys data-files) (map :data (vals data-files))) 
       { :site { :posts (->> (:posts system-data)
                             (map file-to-page-data)
                             (sort-by #(date-to-int (:date %)))
                             reverse)
                 :pages (map file-to-page-data (:pages system-data))
                }} )))

(defn render-edn-page [page-file-map]
  (.-outerHTML
   (crate/html
    (templ/item-list "list-1" "list-1"
                     (map templ/render-item (get-in page-file-map
                                                    [:front-matter :items]))))))

(defn render-raw-page [page-file-map data-for-page]
  (condp = (-> page-file-map :path extention-from-path)
    "md"    (markdown-to-html (:body page-file-map))
    "html"  (render-template (:body page-file-map)
                             data-for-page)
    "edn"   (render-edn-page page-file-map)
    (:body page-file-map)))

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

;; processing system

(defn filter-for-prefix [files prefix]
  (let [rx (js/RegExp. (str "^" prefix))] 
    (filter #(.test rx (:path %)) (vals files))))

(defn get-templates [system files]
  (->> (filter-for-prefix files (:template-path system))
       (map parse-front-matter)
       (map #(self-assoc % :name filename-without-ext))))

(defn get-posts [system files]
  (->> (filter-for-prefix files (:post-path system))
       (map parse-front-matter)
       (map #(assoc % :page-type :post))
       (map #(self-assoc % :date parse-file-date))
       (map #(self-assoc % :target-path make-post-target-path))))

(defn get-pages [system files]
  (->> (filter-for-prefix files (:page-path system))
       (map parse-front-matter)
       (map #(assoc % :page-type :page))               
       (map #(self-assoc % :target-path make-page-target-path))))

(defn get-data [system files]
  (->> (filter-for-prefix files (:data-path system))
       (map parse-data-file)))

(defn process [system files]
  (let [system-data
         { :templates  (map-to-key :name (get-templates system files))
           :data       (map-to-key :name (get-data system files))
           :posts      (get-posts system files)
           :pages      (get-pages system files)
           :system     system }
         data-for-templates (template-data system-data)]
     (->> (concat (:posts system-data) (:pages system-data))
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

(defn atom-chan [a]
  (let [out (chan)]
    (add-watch a :atom-change
               (fn [_ _ ov nv] (put! out [ov nv])))
    out))

(defn map-to-atom
  ([atom input]
     (go-loop [v (<! input)]
              (reset! atom v)
              (recur (<! input))) 
     atom)
  ([input] (map-to-atom (atom {}) input)))

(defn async-flatten-chans [input]
  (let [out (chan)]
    (go-loop [chan-val (<! input)]
             (loop []
               (let [real-val (<! chan-val)]
                 (if (not (nil? real-val))
                   (do
                     (log "processing input")
                     (put! out real-val)
                     (recur)))))
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

(def source-files (atom (or (local-storage-get :next-press-source-files)
                            {})))

(def rendered-files (atom (or (local-storage-get :next-press-rendered-files)
                               {})))

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

(defn system-flow [system input]
  (add-watch source-files :files-changed
             (fn [_ _ o n] (local-storage-set :next-press-source-files n)))
  (add-watch rendered-files :fields-changed
             (fn [_ _ o n] (local-storage-set :next-press-rendered-files n)))
  
  (->> input
       (map< (fn [x] (source-file-list system)))
       async-flatten-chans
       (map< (fn [new-file-list] [(path-etag-map (vals @source-files))
                                 (path-etag-map new-file-list)]))
       (map< changed-map-keys)
       (map< (fn [file-list] (filter good-file-path? file-list)))
       (filter< #(pos? (count %)))
       (fetch-file-list system) 
       (map< #(map-to-key :path %))
       (map< (fn [file-map]
               (swap! source-files merge file-map)
               @source-files))
       (map< (partial process system))
       (map-to-atom rendered-files)
       ;; this atom contains the rendered files
       atom-chan 
       (map< (fn [[ov nv]] (->> (changed-map-keys [ov nv])
                               (select-keys nv)
                               vals)))
       (filter< #(pos? (count %)))
       log-chan
       (map< (fn [files-to-store]
               (->> (to-chan files-to-store)
                    (store-files system)
                    (async/into []))))
       (async/into [])))

(defn publish [{:keys [touch-chan]}]
  (put! touch-chan 1))

(defn clear-cache [system]
  (local-storage-remove :next-press-source-files)    
  (local-storage-remove :next-press-rendered-files))

(defn force-publish [site]
  (clear-cache site)
  (publish site))

(defn create-heckle-for-url [url]
  (go
   (let [config (<! (get-config url)) 
         input-chan (chan)
         flow (system-flow config input-chan)]
     (assoc config :touch-chan input-chan))))

#_(go
 (let [site (<! (create-heckle-for-url
                 "http://immubucket.s3-website-us-east-1.amazonaws.com"))]
   (log site)
   (force-publish site)))


#_(go-loop []
         (publish)
         (<! (timeout 5000))
         (recur))
