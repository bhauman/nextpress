(ns cmsnew.edn-page.items.heading
  (:require
   [cmsnew.edn-page.item :refer [deleted?
                                 render-item
                                 item-form
                                 render-editable-item] :as item]
   [cmsnew.ui.form-templates :as form :refer [control-group]]
   [sablono.core :as sab :include-macros true]
   [reactor.core :as react]   
   [cljs.core.async :as async :refer [put!]]   
   [clojure.string :as string])
  (:require-macros [reactor.macros :as reactm] ))

(defmethod deleted? :heading [{:keys [content deleted]}]
  (or deleted (string/blank? content)))

(defmethod render-item :heading [{:keys [id content type size]}]
  [:div.item {:id id} [(keyword (str "h" size)) content]])

(defmethod render-editable-item :heading [{:keys [id content type size]} {:keys [event-chan]}]
  (item/editable-item-container id type
                           (sab/html
                            [(keyword (str "h" size)) content])
                           event-chan))

(defmethod item-form :heading [item errors event-chan _]
  (reactm/owner-as
   owner
   (sab/html
    (sab/form-to {:onSubmit (react/form-submit owner event-chan :form-submit [:content :size])}
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
                                                  :onClick #(put! event-chan [:change-edited-item {:size x}])
                                                  :className (str "heading-size-btn btn btn-default h" x
                                                                  (if (= (str x) (str (item :size))) " active" ""))
                                                  :data-size x} (str "H" x)]
                                        ) (range 1 6))])
                 (sab/submit-button {:className "btn btn-primary"} "Save")
                 (sab/reset-button {:className "btn btn-default"
                                    :onClick #(put! event-chan [:form-cancel])} "Cancel")))))

