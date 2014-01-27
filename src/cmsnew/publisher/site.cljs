(ns cmsnew.publisher.site
  (:require
   [cmsnew.util.core :refer [self-assoc]]
   [cmsnew.publisher.paths :as p :refer [filter-for-prefix]]
   [cmsnew.publisher.source-file :as sf]   
   ))

(def page-path :page-path)
(def post-path :post-path)
(def data-path :data-path)
(def partial-path :partial-path)
(def template-path :template-path)

(defn source-files [site] (:source-files site))

(defn template-names [site]
  (->> (filter-for-prefix (source-files site) (template-path site))
       (map sf/full-filename-without-ext)))

#_(defn templates [site]
  (->> (filter-for-prefix (source-files site) (template-path site))
       (map (partial sf/parse-front-matter site))
       (map #(self-assoc % :name sf/full-filename-without-ext))))

#_(defn partials [site]
  (->> (filter-for-prefix (source-files site) (partial-path site))
       (map (partial sf/parse-front-matter site))
       (map #(self-assoc % :name sf/full-filename-without-ext))))

(defn partials-for-type [site item-type]
  (->> (filter-for-prefix (source-files site)
                          (str (partial-path site) "/items/" (name item-type)))
       (map (partial sf/parse-front-matter site))
       (map #(self-assoc % :name sf/full-filename-without-ext))))

#_(defn pages [site]
  (->> (filter-for-prefix (source-files site) (page-path site))
       (map (partial sf/parse-front-matter site))
       (map #(assoc % :page-type :page))               
       (map #(self-assoc % :target-path sf/make-page-target-path))))

#_(defn posts [site]
  (->> (filter-for-prefix (source-files site) (post-path site))
       (map (partial sf/parse-front-matter site))
       (map #(assoc % :page-type :post))
       (map #(self-assoc % :date p/parse-file-date))
       (map #(self-assoc % :target-path sf/make-post-target-path))))

#_(defn data-files [site]
  (->> (filter-for-prefix (source-files site) (data-path site))
       (map (partial sf/parse-data-file site))))

(defn edn-pages [site]
  (->> (pages site)
       (filter sf/edn-page?)))
