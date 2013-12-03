(ns cmsnew.templates
  (:require [crate.form :refer [form-to text-field text-area
                                submit-button reset-button drop-down label]]
            [crate.core :as crate]
            [cmsnew.heckle :refer [markdown-to-html]]
            [jayq.util :refer [log]]))

;; rendering items

(defn edit-page [page-data]
  [:div {:class "edit-page"}
   [:div {:class "navbar navbar-default"}
    [:a {:class "navbar-brand" :href "#"} (:title page-data)]
    ]
   [:div {:id "main-area" :class "container"}]
   ])

(defn item-list [id name items]
  [:div {:id id :data-pagename name :class "edit-items-list"} items])

(defn item-container [id type content]
  [:div {:id id :data-pageitem (str type) :class "item"} content])

(defmulti render-item #(:type %))

(defmethod render-item :heading [{:keys [id content type size]}]
  (item-container id type [(keyword (str "h" size)) content]))

(defmethod render-item :markdown [{:keys [id type content]}]
  (item-container id type (crate/raw (markdown-to-html content))))

;; forms

(defn control-group [field errors & content]
    (let [error-msg (first (errors field))]
      [:div {:class (str "form-group" (if error-msg " error"))}
       content
       (if error-msg [:span.help-inline error-msg])]))

(defmulti item-form #(:type %))

(defmethod item-form :heading [item errors]
  (form-to [:post (str "#heading-item-" (:id item))]
           (control-group :content errors
                          (text-field {:class "heading-input" :data-size (item :size)} :content (item :content)))
           (control-group :size errors
                          (label {:class "control-label"} :size "Size")
                          (drop-down {:class "form-control"} :size
                                     (map #(do [% %]) (range 1 6))
                                     (int (:size item))))
           (submit-button {:class "btn btn-primary"} "Save")
           (reset-button {:class "btn btn-default"} "Cancel")))


(defmethod item-form :markdown [item errors]
  (form-to [:post (str "#textblock-item-" (:id item))]
           (control-group :content errors
                          (text-area {:class "big-text-area"} :content (item :content)))
           (submit-button {:class "btn btn-primary"} "Save")
           (reset-button {:class "btn btn-default"} "Cancel")))

(defn edit-form-holder [contents]
  [:div.edit-form-holder
   contents])
