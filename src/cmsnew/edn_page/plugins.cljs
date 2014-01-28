(ns cmsnew.edn-page.plugins
  (:require
   [cmsnew.publisher.util.core :refer [map-in]]
   [cmsnew.publisher.source-file :as sf]
   [cmsnew.edn-page.rendering :refer [render-edn-section]]))

;; add sections to page templ data

(defn sections-from-items [system-data items]
  (let [temp-items (drop-while #(not= (:type %) :section) items)
        section-header (first temp-items)
        section-items (take-while #(not= (:type %) :section) (rest temp-items))]
    (if section-header
      (cons { :name    (:content section-header)
              :items   section-items 
              :content (render-edn-section system-data section-items) }
            (sections-from-items system-data (rest temp-items)))
      [])))

(defn get-sections [system-data source-file]
  (if (sf/edn-page? source-file)
    (sections-from-items system-data (sf/items source-file))
    []))

(defn page-sections [[_ site]]
  (map-in site [:template-env :site :pages]
          (fn [file]
            (let [sections (get-sections site (get-in site [:pages (:name file)]))
                  sections-map (into {} (map (juxt :name :content) sections))]
              (assoc file
                :sections sections
                :sectionsMap sections-map
                :getSection (fn [k] (get sections-map k)))))))

