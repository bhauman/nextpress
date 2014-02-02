(ns cmsnew.edn-page.item
  (:require
   [cljs-uuid-utils :refer [make-random-uuid uuid-string]]
   [cljs.core.async :as async :refer [put!]]
   [sablono.core :as sab :include-macros true]))

(defn random-uuid []
  (uuid-string (make-random-uuid)))

(defn add-id? [{:keys [id] :as item}]
  (if id item (assoc item :id (uuid-string (make-random-uuid)))))

(defmulti new-item identity)

(defmethod new-item :default [t]
  (add-id? {:type t}))

(defmulti deleted? #(:type %))

(defmethod deleted? :default [{:keys [deleted]}]
  deleted)

(defmulti render-item #(:type %))

(defmethod render-item :default [{:keys [id type] :as item}]
  [:div.item {:id id } (prn-str item)])


(defmulti render-editable-item #(:type %))

(defn editable-item-container [id type content event-chan]
  (sab/html [:div {:id id
                   :data-pageitem (str type)
                   :onDoubleClick #(put! event-chan [:edit-item {:id id}])
                   :className "item"}
             content]))

(defmethod render-editable-item :default [{:keys [id type] :as item} {:keys [event-chan]}]
  (editable-item-container id type
                           (sab/html [:pre (prn-str item)])
                           event-chan))

(defmulti item-form #(:type %))
