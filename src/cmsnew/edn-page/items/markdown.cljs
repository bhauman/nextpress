(ns cmsnew.edn-page.items.markdown
  (:require
   [cmsnew.edn-page.item :refer [deleted?
                                 render-item
                                 item-form
                                 render-editable-item] :as item]
   [cmsnew.ui.form-templates :as form :refer [control-group]]   
   [crate.core :as crate]
   [sablono.core :as sab :include-macros true]   
   [reactor.core :as react]   
   [cmsnew.transformer.markdown :refer [markdown-to-html]]
   [cljs.core.async :as async :refer [put!]]   
   [clojure.string :as string])
  (:require-macros [reactor.macros :as reactm]))

(defmethod deleted? :markdown [{:keys [content deleted]}]
  (or deleted (string/blank? content)))

(defmethod render-item :markdown [{:keys [id type content]}]
  [:div.item {:id id}
   (crate/raw (markdown-to-html content))])

(defmethod render-editable-item :markdown [{:keys [id type content]} {:keys [event-chan]}]
  (item/editable-item-container id type
                                (react/raw (markdown-to-html content))
                                event-chan))

(defmethod item-form :markdown [item errors event-chan owner]
  (reactm/owner-as
   owner
   (sab/html
    (sab/form-to
     {:onSubmit (react/form-submit owner event-chan :form-submit [:content])}
     [:post (str "#textblock-item-" (:id item))]
     (control-group :content errors
                    [:textarea {:className "big-text-area"
                                :defaultValue (:content item)
                                :name "content"
                                :ref "content"}])
     (sab/submit-button {:className "btn btn-primary"} "Save")
     (sab/reset-button {:className "btn btn-default"
                        :onClick #(put! event-chan [:form-cancel])} "Cancel"))
    )))
