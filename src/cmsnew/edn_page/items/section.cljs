(ns cmsnew.edn-page.items.section
  (:require
   [cmsnew.edn-page.item :refer [deleted?
                                 render-item
                                 item-form
                                 render-editable-item] :as item]
   [cmsnew.ui.form-templates :as form :refer [control-group select-alternate-partial]]
   [reactor.core :as react]   
   [sablono.core :as sab :include-macros true]
   [cljs.core.async :as async :refer [put!]]
   [clojure.string :as string])
  (:require-macros [reactor.macros :as reactm]))

(defmethod deleted? :section [{:keys [content deleted]}]
  (or deleted (string/blank? content)))

(defmethod render-item :section [{:keys [id content] :as item}]
  [:a.section { :name content }])

(defmethod render-editable-item :section [{:keys [id content]} {:keys [event-chan]}]
  (item/editable-item-container id type
                                (sab/html
                                 [:div [:pre "Section: " "\"" content "\""]])
                                event-chan))

(defmethod item-form :section [item errors {:keys [event-chan] :as state}]
  (reactm/owner-as
   owner
   (sab/html
    (sab/form-to {:onSubmit (react/form-submit owner event-chan :edit-item.form-submit [:content :partial])}
                 [:post (str "#section-item-" (:id item))]
                 (control-group :content errors
                                (sab/text-field {:className "form-control"
                                                 :ref "content"
                                                 :defaultValue (item :content)
                                                 :placeholder "Section name"}
                                                :content))
                 (select-alternate-partial item state :section)                 
                 (sab/submit-button {:className "btn btn-primary"} "Save")
                 (sab/reset-button {:className "btn btn-default"
                                    :onClick #(put! event-chan [:edit-item.form-cancel])} "Cancel")))))
