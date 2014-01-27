(ns cmsnew.publisher.paths
  (:require
   [clojure.string :as string]))

(defn filter-for-prefix [files prefix]
  (if prefix
    (let [rx (js/RegExp. (str "^" prefix))] 
      (filter #(.test rx (:path %)) (vals files)))
    []))

(def filename-parts #(string/split % #"-"))

(defn parse-date [filename]
  (if (.test #"^\d{4}-\d{2}-\d{2}-" filename)
    (zipmap [:year :month :day]
            (map js/parseInt (take 3 (filename-parts filename))))
    {}))

(defn date-to-int [{:keys [year month day]}]
  (+ (* 10000 year) (* 100 month) day))

(def path-parts #(string/split % #"/"))

(defn parse-file-date [{:keys [path]}]
  (parse-date (last (path-parts path))))

(defn remove-prefix [prefix path]
  (->> (path-parts path)
       (drop (count (path-parts prefix)))
       (string/join "/")))

;; handle path rewriting

(def filename-from-path (comp last path-parts))

(def full-filename-from-path
  (comp
   (partial string/join "/")
   rest
   path-parts))

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

(defn remove-extention [path]
  (replace-extention path ""))
