(ns cmsnew.templates
  (:require [crate.form :refer [form-to text-field text-area hidden-field
                                submit-button reset-button drop-down label]]
            [cljs.core.async :as async
             :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
            [crate.core :as crate]
            [reactor.core :as react]
            [sablono.core :as sab :include-macros true]
            [cmsnew.markdown :refer [markdown-to-html]]
            [jayq.util :refer [log]])
  (:require-macros [reactor.macros :as reactm] ))

;; helpers
(defn delete-button []
  [:button {:type "button" :class "btn btn-danger form-delete pull-right"} "Delete"])

;; rendering items

(defn tooltip-template []
  [:div {:class "btn-group"}
   [:button {:type "button" :class "btn btn-default add-heading-item"} "Heading"]
   [:button {:type "button" :class "btn btn-default add-text-item"} "Text"]
   [:button {:type "button" :class "btn btn-default add-image-item"} "Image"]])

(defn item-list [id name items]
  [:div {:id id :data-pagename name :class "edit-items-list"} items])

(defn item-container [id type content]
  [:div {:id id :data-pageitem (str type) :class "item"} content])

(defmulti render-item #(:type %))

(defmethod render-item :default [{:keys [id type] :as item}]
  (item-container id type [:div (prn-str item)]))

(defmethod render-item :image [{:keys [id type url] :as item}]
  (item-container id type [:p [:img.img-responsive {:src url}]]))

(defmethod render-item :heading [{:keys [id content type size]}]
  (item-container id type [(keyword (str "h" size)) content]))

(defmethod render-item :markdown [{:keys [id type content]}]
  (item-container id type (crate/raw (markdown-to-html content))))

;; moving to react

(defn editable-item-container [id type content event-chan]
  (sab/html [:div {:id id
                   :data-pageitem (str type)
                   :onClick #(put! event-chan [:edit-item {:id id}])
                   :className "item"} content]))

(defmulti render-editable-item #(:type %))

(defmethod render-editable-item :default [{:keys [id type] :as item} {:keys [event-chan]}]
  (editable-item-container id type
                           (sab/html [:div (prn-str item)])
                           event-chan))

(defmethod render-editable-item :image [{:keys [id type url] :as item} {:keys [event-chan]}]
  (editable-item-container id type
                           (sab/html [:p [:img.img-responsive {:src url}]])
                           event-chan))

(defmethod render-editable-item :heading [{:keys [id content type size]} {:keys [event-chan]}]
  (editable-item-container id type
                           (sab/html
                            [(keyword (str "h" size)) content])
                           event-chan))

(defmethod render-editable-item :markdown [{:keys [id type content]} {:keys [event-chan]}]
  (editable-item-container id type
                           (react/raw (markdown-to-html content))
                           event-chan))

;; forms

(defn control-group [field errors & content]
    (let [error-msg (first (errors field))]
      [:div {:className (str "form-group" (if error-msg " error"))}
       content
       (if error-msg [:span.help-inline error-msg])]))

(defmulti item-form #(:type %))

(defmethod item-form :heading [item errors event-chan _]
  (reactm/owner-as
   owner
   (sab/html
    (form-to {:onSubmit (react/form-submit owner event-chan :form-submit [:content :size])}
             [:post (str "#heading-item-" (:id item))]
             (control-group :content errors
                            (text-field {:className "heading-input" :data-size (item :size)
                                         :ref "content"
                                         :defaultValue (item :content)
                                         :placeholder "New Heading"}
                                        :content))
             (hidden-field {:ref "size"} :size (item :size))
             (control-group :size errors
                            [:div.btn-group.heading-size 
                             (map (fn [x]
                                    [:button {:type "button"
                                              :onClick #(put! event-chan [:change-edited-item {:size x}])
                                              :className (str "heading-size-btn btn btn-default h" x
                                                              (if (= (str x) (str (item :size))) " active" ""))
                                              :data-size x} (str "H" x)]
                                    ) (range 1 6))])
             (submit-button {:className "btn btn-primary"} "Save")
             (reset-button {:className "btn btn-default"
                            :onClick #(put! event-chan [:form-cancel])} "Cancel")))))

(defmethod item-form :image [item errors event-chan owner]
  (reactm/owner-as
   owner
   (sab/html
    (form-to {:onSubmit (react/form-submit owner event-chan :form-submit [:description])}
             [:post (str "#image-item-" (:id item))]
             (control-group :description errors
                            (text-field {:className "heading-input"
                                         :ref "description"
                                         :defaultValue (item :description)
                                         :placeholder "Description"}
                                        :description))
             (submit-button {:className "btn btn-primary"} "Save")
             (reset-button {:className "btn btn-default"
                            :onClick #(put! event-chan [:form-cancel])} "Cancel")
             (delete-button)))))

(defmethod item-form :markdown [item errors event-chan owner]
  (reactm/owner-as
   owner
   (sab/html
    (form-to
     {:onSubmit (react/form-submit owner event-chan :form-submit [:content])}
     [:post (str "#textblock-item-" (:id item))]
     (control-group :content errors
                    [:textarea {:className "big-text-area"
                                :defaultValue (:content item)
                                :name "content"
                                :ref "content"}])
     (submit-button {:className "btn btn-primary"} "Save")
     (reset-button {:className "btn btn-default"
                    :onClick #(put! event-chan [:form-cancel])} "Cancel"))
    )))

(defn edit-form-holder [contents]
  [:div.edit-form-holder
   contents])

(defn item-under-edit [{:keys [editing-item]}] editing-item)

(defn item-being-edited? [{:keys [editing-item]} item]
  (and editing-item (= (:id item) (:id editing-item))))

(defn item-renderer [item state]
  (if (item-being-edited? state item)
    (sab/html (item-container (:id item) (:type item)
                              (reactm/owner-as owner
                                               (sab/html (item-form (item-under-edit state) {}
                                                                    (:event-chan state) owner)))))
    (render-editable-item item state)))

(defn render-edn-page [{:keys [edn-page] :as state}]
  (sab/html
   (item-list "list-1" "list-1" (map item-renderer
                                     (get-in edn-page [:front-matter :items])
                                     (repeat state)))))

(defn edit-page [{:keys [edn-page] :as state}]
  (sab/html [:div.edit-page
   [:div.navbar.navbar-default
    [:a.navbar-brand { :href "#"} (-> edn-page :front-matter :title)]]
   [:div#main-area.container
    (render-edn-page state)
    ]
   [:div#tooltipper.tooltipper "+"]
   [:div.hidden {:id "image-upload"}
    [:input.image-upload {:type "file" :name "image-upload-file" }]]
   ]))
