(ns cmsnew.edn-page.items.image
  (:require
   [cmsnew.edn-page.item :refer [deleted?
                                 render-item
                                 item-form
                                 render-editable-item] :as item]
   [cmsnew.ui.form-templates :as form :refer [control-group delete-button select-alternate-partial]]
   [reactor.core :as react]   
   [sablono.core :as sab :include-macros true]
   [cljs.core.async :as async :refer [put!]]   
   [clojure.string :as string])
  (:require-macros [reactor.macros :as reactm]))

;; uses default deleted? implementation

(defmethod render-item :image [{:keys [id type url] :as item}]
  [:div.item {:id id}
   [:p [:img.img-responsive {:src url}]]])

(defmethod render-editable-item :image [{:keys [id type url] :as item} {:keys [event-chan]}]
  (item/editable-item-container id type
                                (sab/html [:p [:img.img-responsive {:src url}]])
                                event-chan))

(defmethod item-form :image [item errors {:keys [event-chan] :as state}]
  (reactm/owner-as
   owner
   (sab/html
    (sab/form-to {:onSubmit (react/form-submit owner event-chan :form-submit [:description :partial])}
                 [:post (str "#image-item-" (:id item))]
                 [:p [:img.img-responsive {:src (:url item)}]]
                 (control-group :description errors
                                (sab/text-field {:className "heading-input"
                                                 :ref "description"
                                                 :defaultValue (item :description)
                                                 :placeholder "Description"}
                                                :description))
                 (select-alternate-partial item state :image)
                 (sab/submit-button {:className "btn btn-primary"} "Save")
                 (sab/reset-button {:className "btn btn-default"
                                    :onClick #(put! event-chan [:form-cancel])} "Cancel")
                 (delete-button event-chan)))))
