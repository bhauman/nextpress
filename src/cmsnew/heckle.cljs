(ns cmsnew.heckle
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan]]
   [cmsnew.datastore.s3 :as store]
   [cmsnew.markdown :refer [markdown-to-html]]
   [cmsnew.templates :as templ]
   [crate.core :as crate]
   [clojure.string :as string]
   [cljs.reader :refer [push-back-reader read-string]]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(def system {
             :bucket "immubucket"
             :store-root "http://s3.amazonaws.com"
             :template-path "_layouts"
             :post-path "_posts"
             :page-path "_site_src"             
             :data-path "_data"             
             })

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

(defn render-template-to-path [system template-url data path]
  (store/get-text template-url
                  (fn [template-body]
                    (store/save-data-to-file path
                                             (render-template template-body data)
                                             "text/html"
                                             (fn [e] (log (.getResponseHeader e
                                                                             "x-amz-version-id")))))))
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
                    (fn [body] (put! out {:path file :body body}) (close! out)))
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

;; processing pipelines

(defn get-templates [system]
  (go (<! (->> (file-list (:bucket system) (:template-path system))
               (fetch-files system)
               (map< parse-front-matter)
               (map< #(self-assoc % :name filename-without-ext))
               (async/into [])))))

(defn get-posts [system]
  (go (<! (->> (file-list (:bucket system) (:post-path system))
               (fetch-files system)
               (map< parse-front-matter)
               (map< #(assoc % :page-type :post))
               (map< #(self-assoc % :date parse-file-date))
               (map< #(self-assoc % :target-path make-post-target-path))
               (async/into [])))))

(defn get-pages [system]
  (go (<! (->> (file-list (:bucket system) (:page-path system))
               (fetch-files system)
               (map< parse-front-matter)
               (map< #(assoc % :page-type :page))               
               (map< #(self-assoc % :target-path make-page-target-path))
               (async/into [])))))

(defn get-data [system]
  (go (<! (->> (file-list (:bucket system) (:data-path system))
               (fetch-files system)
               (map< parse-data-file)
               (async/into [])))))

(defn process [system]
  (go
   (let [system-data
         { :templates  (map-to-key :name (<! (get-templates system)))
           :data       (map-to-key :name (<! (get-data system)))
           :posts      (<! (get-posts system))
           :pages      (<! (get-pages system))
           :system     system
          }
         data-for-templates (template-data system-data)]
     (log (clj->js data-for-templates))
     (log (clj->js system-data))
     (->> (async/merge [(to-chan (:posts system-data)) (to-chan (:pages system-data))])
          (map< #(self-assoc % :rendered-body
                             (partial render-page-with-templates
                                      system-data
                                      data-for-templates)))
          (store-files system)
          (async/into []))
     )))

(defn filter-for-prefix [files prefix]
  (let [rx (js/RegExp. (str "^" prefix))] 
    (filter #(.test rx (:path %)) (vals files))))

(defn new-get-templates [system files]
  (->> (filter-for-prefix files (:template-path system))
       (map parse-front-matter)
       (map #(self-assoc % :name filename-without-ext))))

(defn new-get-posts [system files]
  (->> (filter-for-prefix files (:post-path system))
       (map parse-front-matter)
       (map #(assoc % :page-type :post))
       (map #(self-assoc % :date parse-file-date))
       (map #(self-assoc % :target-path make-post-target-path))))

(defn new-get-pages [system files]
  (->> (filter-for-prefix files (:page-path system))
       (map parse-front-matter)
       (map #(assoc % :page-type :page))               
       (map #(self-assoc % :target-path make-page-target-path))))

(defn new-get-data [system files]
  (->> (filter-for-prefix files (:data-path system))
       (map parse-data-file)))

(defn new-process [system files]
  (let [system-data
         { :templates  (map-to-key :name (new-get-templates system files))
           :data       (map-to-key :name (new-get-data system files))
           :posts      (new-get-posts system files)
           :pages      (new-get-pages system files)
           :system     system }
         data-for-templates (template-data system-data)]
     (log (clj->js data-for-templates))
     (log (clj->js system-data))
     (->> (concat (:posts system-data) (:pages system-data))
          (map #(self-assoc % :rendered-body
                            (partial render-page-with-templates
                                     system-data
                                     data-for-templates)))
          (map (juxt :path identity))
          (into {}))))

; working on detecting file changes

(defn files-that-changed [old-etag-map new-etag-map]
  (let [paths (set (concat (keys old-etag-map) (keys new-etag-map)))]
    (keep (fn [p]
           (let [old-etag (get old-etag-map p)
                 new-etag (get new-etag-map p)]
             (cond
              (= old-etag new-etag) nil
              (and (nil? new-etag) (not (nil? old-etag))) [:deleted p]
              :else [:changed p])
             )) paths)
    ))

(let [dir-path-rx #"/$"
      hash-rx #"\#$"]
  (defn good-file-path? [path]
    (not (or (.test dir-path-rx path)
             (.test hash-rx path)))))

(defn files-changed-list [files-changed]
  (to-chan (map last files-changed)))

(defn get-changed-files [system changed-files]
  (->> (files-changed-list changed-files)
       (fetch-files system)
       (async/into [])))

(defn files-chan []
  (let [path-etag-map (atom {})
        files-atom (atom {})
        rendered-files (atom {})]
    (add-watch path-etag-map :watch-change (fn [k a oldv newv]
                                             (go
                                              (let [changes  (files-that-changed oldv newv)
                                                    fetched-files (<! (get-changed-files system
                                                                                         changes))]
                                                (if (pos? (count changes))
                                                  (swap! files-atom merge (map-to-key :path fetched-files)))))))

    (add-watch files-atom :files-change (fn [k a oldv newv]
                                          (let [rendered (new-process system newv)]
                                            (reset! rendered-files rendered))))

    (add-watch rendered-files :rendered-change (fn [k a oldv newv]
                                                 (log (clj->js newv))
                                                 (log "rendered change")
                                                 (let [changes (files-that-changed oldv newv)
                                                       files-to-store (vals (select-keys newv (map last changes)))]
                                                   (log "changed files")
                                                   (log (clj->js files-to-store))
                                                   (->> (to-chan files-to-store)
                                                        (store-files system)
                                                        (async/into []))
                                                   )
                                                 ))

    (go-loop []

             (store/get-bucket-list "immubucket" "_"
                                    (fn [res]
                                      (reset! path-etag-map (into {}
                                                                  (filter (fn [[path _]] (good-file-path? path))
                                                                          (map (juxt :path :etag) res))))))
             (<! (timeout 5000))
             (recur))
    
    )

  )



(files-chan)

(log (good-file-path? "_data/"))
(log (good-file-path? "_data/something.edn"))
(log (good-file-path? "_data/something.edn#"))

#_(store/get-bucket-list "immubucket" "_" (fn [filenames]  (log (clj->js filenames))))

