(ns cmsnew.ui.form-templates
  (:require
   [sablono.core :as sab]
   [cmsnew.publisher.site :as st]
   [clojure.string :as string]
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

(defn partial-options [site type]
  (let [partials (st/partials-for-type site type)]
    (cons ["::default::" ""]
          (sort-by first
                   (filter (comp not string/blank? first)
                           (map (fn [{:keys [name path]}]
                                  [(->> (string/split name #"/")
                                        (drop 2)
                                        (string/join "/"))
                                   name]
                                  ) partials))))))

(defn select-alternate-partial [item state type]
  (let [options (partial-options (:site state) type)]
    (if (= 1 (count options))
      (sab/hidden-field {:ref "partial"} :partial)
      [:div.form-group
       [:label "Apply Partial"]
       (control-group
        :partial
        (or (:errors item) {})
        (sab/drop-down {:ref "partial"
                        :className "form-control"                            
                        :defaultValue (item :partial)}
                       :partial
                       options))])))

