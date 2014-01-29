(ns cmsnew.site
  (:require
   [cmsnew.publisher.source-file :as sf]))

(defn pages [st]
  (vals (:pages st)))

(defn edn-pages [st]
  (->> (pages st)
       (filter sf/edn-page?)))
