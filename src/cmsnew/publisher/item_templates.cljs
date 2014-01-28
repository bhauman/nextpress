(ns cmsnew.publisher.item-templates
  (:require
   [crate.core :as crate]
   [cmsnew.publisher.transformer.markdown :refer [markdown-to-html]]))

(defn item-list [id name items]
  [:div.edit-items-list {:id id :data-pagename name } items])

(defn item-container [id type content]
  [:div {:id id :data-pageitem (str type) :class "item"} content])

