(ns cmsnew.publisher.item-templates
  (:require
   [crate.core :as crate]
   [cmsnew.markdown :refer [markdown-to-html]]))

(defn item-list [id name items]
  [:div.edit-items-list {:id id :data-pagename name } items])

(defn item-container [id type content]
  [:div {:id id :data-pageitem (str type) :class "item"} content])

(defmulti render-item #(:type %))

(defmethod render-item :default [{:keys [id type] :as item}]
  (item-container id type [:div (prn-str item)]))

(defmethod render-item :image [{:keys [id type url] :as item}]
  (item-container id type [:p [:img.img-responsive {:src url}]]))

(defmethod render-item :heading [{:keys [id content type size]}]
  (item-container id type [(keyword (str "h" size)) content]))

(defmethod render-item :markdown [{:keys [id type content]}]
  (item-container id type (crate/raw (markdown-to-html content))))

