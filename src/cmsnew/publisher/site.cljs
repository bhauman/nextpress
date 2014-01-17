(ns cmsnew.publisher.site
  (:require
   [cmsnew.util.core :refer [self-assoc]]
   [cmsnew.publisher.paths :as p :refer [filter-for-prefix]]
   [cmsnew.publisher.source-file :as sf]   
   ))

(def page-path :page-path)
(def post-path :post-path)
(def data-path :post-path)
(def template-path :template-path)

(defn source-files [site] @(:source-files site))

(defn template-names [site]
  (->> (filter-for-prefix (source-files site) (template-path site))
       (map sf/filename-without-ext)))

(defn templates [site]
  (->> (filter-for-prefix (source-files site) (template-path site))
       (map (partial sf/parse-front-matter site))
       (map #(self-assoc % :name sf/filename-without-ext))))

(defn pages [site]
  (->> (filter-for-prefix (source-files site) (page-path site))
       (map (partial sf/parse-front-matter site))
       (map #(assoc % :page-type :page))               
       (map #(self-assoc % :target-path sf/make-page-target-path))))

(defn posts [site]
  (->> (filter-for-prefix (source-files site) (post-path site))
       (map (partial sf/parse-front-matter site))
       (map #(assoc % :page-type :post))
       (map #(self-assoc % :date p/parse-file-date))
       (map #(self-assoc % :target-path sf/make-post-target-path))))

(defn data-files [site]
  (->> (filter-for-prefix (source-files site) (data-path site))
       (map (partial sf/parse-data-file site))))

(defn edn-pages [site]
  (->> (pages site)
       (filter sf/edn-page?)))
