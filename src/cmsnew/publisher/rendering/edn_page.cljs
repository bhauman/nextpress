(ns cmsnew.publisher.rendering.edn-page
  (:require
   [cmsnew.transformer.underscore-template :refer [render-template]]
   [cmsnew.edn-page.item :refer [render-item]]
   [cmsnew.edn-page.items.heading]
   [cmsnew.edn-page.items.markdown]
   [cmsnew.edn-page.items.section]
   [cmsnew.edn-page.items.image]
   [crate.core :as crate]
   [clojure.string :as string]
   ))

(defn item-list [id name items]
  [:div.edit-items-list {:id id :data-pagename name } items])

(defn item-container [id type content]
  [:div {:id id :data-pageitem (str type) :class "item"} content])

;; rendering pages

(defn template-for-item-type [system-data item]
  ;; this is getting pretty arbitrary we should probably have full
  ;; paths to everything and use those as keys
  ((:partials system-data)
   (or (and (not (string/blank? (:partial item))) (:partial item))
       (str "items/" (name (:type item))))))

(defn render-item-with-template-overide [system-data item]
  (if-let [template (template-for-item-type system-data item)]
    (crate/raw (render-template (:body template) item))    
    (render-item item)))

(defn render-edn-page [system-data page-file-map]
  (.-outerHTML
   (crate/html
    (item-list "list-1" "list-1"
               (map (partial render-item-with-template-overide system-data)
                    (get-in page-file-map
                            [:front-matter :items]))))))

(defn render-edn-section [system-data items]
  (.-outerHTML
   (crate/html
    [:div
     (map (partial render-item-with-template-overide system-data) items)])))


