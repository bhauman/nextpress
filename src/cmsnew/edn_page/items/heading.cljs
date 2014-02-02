(ns cmsnew.edn-page.items.heading
  (:require
   [cmsnew.edn-page.item :refer [deleted?
                                 add-id?
                                 render-item
                                 item-form
                                 new-item
                                 render-editable-item] :as item]
   [cmsnew.ui.form-templates :as form :refer [control-group select-alternate-partial]]
   [cmsnew.publisher.site :as st]
   [sablono.core :as sab :include-macros true]
   [reactor.core :as react]   
   [cljs.core.async :as async :refer [put!]]   
   [clojure.string :as string]
   [jayq.util :refer [log]])
  (:require-macros [reactor.macros :as reactm] ))

(defmethod new-item :heading [_]
  (add-id? {:type :heading :size 2}))

(defmethod deleted? :heading [{:keys [content deleted]}]
  (or deleted (string/blank? content)))

(defmethod render-item :heading [{:keys [id content type size]}]
  [:div.item {:id id} [(keyword (str "h" size)) content]])

(defmethod render-editable-item :heading [{:keys [id content type size]} {:keys [event-chan]}]
  (item/editable-item-container id type
                           (sab/html
                            [(keyword (str "h" size)) content])
                           event-chan))

(defmethod item-form :heading [item errors {:keys [event-chan] :as state}]
  (reactm/owner-as
   owner
   (sab/html
    (sab/form-to {:onSubmit (react/form-submit owner event-chan :edit-item.form-submit [:content :size :partial])}
                 [:post (str "#heading-itemer-" (:id item))]
                 (control-group :content errors
                                (sab/text-field {:className "heading-input" :data-size (item :size)
                                                 :ref "content"
                                                 :defaultValue (item :content)
                                                 :placeholder "New Heading"}
                                                :content))
                 (sab/hidden-field {:ref "size"} :size (item :size))
                 (control-group :size errors
                                [:div.btn-group.heading-size
                                 (map (fn [x]
                                        [:button {:type "button"
                                                  :onClick #(put! event-chan [:edit-item.change-edited-item {:size x}])
                                                  :className (str "heading-size-btn btn btn-default h" x
                                                                  (if (= (str x) (str (item :size))) " active" ""))
                                                  :data-size x} (str "H" x)]
                                        ) (range 1 6))])
                 (select-alternate-partial item state :heading)
                 (sab/submit-button {:className "btn btn-primary"} "Save")
                 (sab/reset-button {:className "btn btn-default"
                                    :onClick #(put! event-chan [:edit-item.form-cancel])} "Cancel")))))
