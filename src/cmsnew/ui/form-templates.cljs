(ns cmsnew.ui.form-templates
  (:require
    [cljs.core.async :as async :refer [put!]]))

(defn delete-button [event-chan]
  [:button {:type "button"
            :onClick #(put! event-chan [:form-delete])
            :className "btn btn-danger form-delete pull-right"} "Delete"])

(defn control-group [field errors & content]
  (let [error-msg (first (errors field))]
    [:div {:className (str "form-group" (if error-msg " error"))}
     content
     (if error-msg [:span.help-inline error-msg])]))
