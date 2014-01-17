(ns cmsnew.publisher.source-file
  (:require
   [cmsnew.publisher.paths :as pth]
   [clojure.string :as string]
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]   
   [cljs.reader :refer [push-back-reader read-string]]))

(def path :path)

(def title #(get-in % [:front-matter :title]))

(def items #(get-in % [:front-matter :items]))

(defn has-date? [fm] (and (:date fm) (-> fm :date :month)))

(defn edn-page? [source-file]
  (= "edn" (last (string/split (path source-file) #"\."))))

(defn publish? [source-file]
  (get-in source-file [:front-matter :published]))

;; pulling out front-matter

(def default-front-matter { :published true })

(defn get-front-matter [log-chan path reader]
  (try
    (let [front-matter-map (cljs.reader/read reader true nil false)]
      (if (map? front-matter-map)
        (merge default-front-matter front-matter-map) 
        false))
    (catch js/Object e
      (put! log-chan {:msg (str "Error parsing file "
                                path ": "
                                (.-message e))})
      false)))

(defn parse-front-matter [site file-map]
  (let [r (push-back-reader (:body file-map))]
    (if-let [front-matter (get-front-matter (:log-chan site)
                                            (:path file-map) r)]
      (assoc file-map
        :front-matter front-matter
        :body (.substr (:body file-map) (inc (.-idx r))))
      file-map)))

;; parse data file

(defn parse-data-file [system file-map]
  (try
    (assoc file-map
      :data (read-string (:body file-map))
      :name (-> (:path file-map)
                pth/filename-from-path
                (pth/replace-extention "")))
    (catch js/Object e
      (put! (:log-chan system)
            {:msg (str "Error parsing file "
                       (:path file-map) ": "
                       (.-message e))})
      file-map)))

;; getting paths

(defn filename-without-ext [{:keys [path]}]
  (-> path
      pth/filename-from-path
      (pth/replace-extention "")))

(defn make-post-target-path [{:keys [date path] :as fm}]
  (pth/replace-extention
   (if (has-date? fm)
     (string/join "/"
                  [(:year date) (:month date) (:day date)
                   (pth/dated-post-target-filename path)])
     (pth/filename-from-path path))
   ".html"))

(defn make-page-target-path [{:keys [path] :as fm}]
  (pth/replace-extention
   (->> path
        pth/path-parts
        rest
        (string/join "/"))   
   ".html"))

(defn make-target-path [fm]
  (condp = (fm :page-type)
    :post (make-post-target-path fm)
    (make-page-target-path fm)))

;; 

(defn display-name [source-file]
  (or (title source-file)
      (filename-without-ext source-file)))
