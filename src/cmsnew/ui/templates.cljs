(ns cmsnew.ui.templates
  (:require [crate.form :refer [form-to text-field text-area hidden-field
                                submit-button reset-button drop-down label]]
            [cmsnew.ui.tooltipper :as tooltip]
            [cljs.core.async :as async
             :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
            [crate.core :as crate]
            [reactor.core :as react]
            [sablono.core :as sab :include-macros true]
            [cmsnew.transformer.markdown :refer [markdown-to-html]]
            [cmsnew.publisher.item-templates :refer [item-list item-container]]
            [jayq.core :as jq :refer [$]]
            [jayq.util :refer [log]])
  (:require-macros [reactor.macros :as reactm] ))


;; helpers
(defn delete-button [event-chan]
  [:button {:type "button"
            :onClick #(put! event-chan [:form-delete])
            :className "btn btn-danger form-delete pull-right"} "Delete"])

(defn loading [percent-complete]
  [:div.progress.progress-striped.active
   [:div.progress-bar {:role "progressbar"
                       :aria-valuenow percent-complete
                       :aria-valuemin 0
                       :aria-valuemax 100
                       :style { :width (str percent-complete "%")}}
    [:span.sr-only (str percent-complete "% Complete") ]]])

;; rendering items

(defn item-modify-control [id event-chan]
  [:div.item-modify-control.btn-group
   [:button.btn.btn-default {:type "button"
                             :onClick #(put! event-chan [:move-item-up {:id id}])
                             } [:span.glyphicon.glyphicon-chevron-up]]
   [:button.btn.btn-default {:type "button"
                             :onClick #(put! event-chan [:move-item-down {:id id}])
                             } [:span.glyphicon.glyphicon-chevron-down]]])

;; moving to react

(defn editable-item-container [id type content event-chan]
  (sab/html [:div {:id id
                   :data-pageitem (str type)
                   :onDoubleClick #(put! event-chan [:edit-item {:id id}])
                   :className "item"}
             #_(item-modify-control id event-chan)
             content]))

(defmulti render-editable-item #(:type %))

(defmethod render-editable-item :default [{:keys [id type] :as item} {:keys [event-chan]}]
  (log (str "rendering " (prn-str [type id])))
  (editable-item-container id type
                           (sab/html [:div (prn-str item)])
                           event-chan))

(defmethod render-editable-item :image [{:keys [id type url] :as item} {:keys [event-chan]}]
  (log (str "rendering " (prn-str [type id])))
  (editable-item-container id type
                           (sab/html [:p [:img.img-responsive {:src url}]])
                           event-chan))

(defmethod render-editable-item :heading [{:keys [id content type size]} {:keys [event-chan]}]
  (log (str "rendering " (prn-str [type id])))
  (editable-item-container id type
                           (sab/html
                            [(keyword (str "h" size)) content])
                           event-chan))

(defmethod render-editable-item :markdown [{:keys [id type content]} {:keys [event-chan]}]
  (log (str "rendering " (prn-str [type id])))
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
             [:p [:img.img-responsive {:src (:url item)}]]
             (control-group :description errors
                            (text-field {:className "heading-input"
                                         :ref "description"
                                         :defaultValue (item :description)
                                         :placeholder "Description"}
                                        :description))
             (submit-button {:className "btn btn-primary"} "Save")
             (reset-button {:className "btn btn-default"
                            :onClick #(put! event-chan [:form-cancel])} "Cancel")
             (delete-button event-chan)))))

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

(defn item-under-edit [{:keys [editing-item]}] editing-item)

(defn item-being-edited? [{:keys [editing-item]} item]
  (and editing-item (= (:id item) (:id editing-item))))

(defn item-renderer [item state]
  (if (item-being-edited? state item)
    (sab/html (item-container (:id item) (:type item)
                              (reactm/owner-as owner
                                               (sab/html (item-form (item-under-edit state) {}
                                                                    (:event-chan state) owner)))))
    (reactm/pure item (render-editable-item item state))))

(defn insert-at [items index item]
  (concat (take index items) [item] (drop index items)))

(defn render-edn-page [{:keys [edn-page] :as state}]
  (let [page-data (get-in edn-page [:front-matter :items])
        ;; this feels hacky
        page-data-inserted (if-let [insp (get-in state [:editing-item :insert-position])]
                             (insert-at page-data insp (:editing-item state))
                             page-data)]
    (sab/html
     (item-list "list-1" "list-1" (map item-renderer
                                       page-data-inserted
                                       (repeat state))))))

(defn edit-front-matter-form [{:keys [edn-page event-chan editing-front-matter] :as state}]
  (reactm/owner-as
   owner
   (sab/html
    (form-to {:onSubmit (react/form-submit owner event-chan :form-submit [:title :layout])}
             [:post (str "#pasting-front-matter")]
             [:div.form-group
              [:label "Title"]
              (control-group
               :title (or (:errors editing-front-matter) {})
               (text-field {:ref "title"
                            :className "form-control"
                            :defaultValue (editing-front-matter :title)
                            :placeholder "Enter a page title"}
                           :title))]
             [:div.form-group
              [:label "Layout"]
              (control-group
               :layout (or (:errors editing-front-matter) {})
               (sab/drop-down {:ref "layout"
                               :className "form-control"                            
                               :defaultValue (editing-front-matter :layout)}
                              :layout
                              [["default" "default"] ["post" "post"]]
                              ))]
             (submit-button {:className "btn btn-primary"} "Save")
             (reset-button {:className "btn btn-default"
                            :onClick #(put! event-chan [:form-cancel])} "Cancel")))))

(defn edit-page [{:keys [edn-page] :as state}]
  (sab/html
   [:div.edit-page
    [:div.navbar.navbar-default
     [:div.container
      [:a.pull-left.navbar-icon.navbar-icon-left
       {:href "#"
        :onClick #(do (put! (:event-chan state) [:edit-settings]) false)}
       [:span.glyphicon.glyphicon-cog]]
      [:a.navbar-brand { :href "#"} (-> edn-page :front-matter :title)]
      [:a.pull-right.navbar-icon
       {:href "#"
        :onClick #(do (put! (:close-chan state) [:close]) false)}
       [:span.glyphicon.glyphicon-remove-sign]]
      ]
     ]
    [:div#main-area.container
     (if (:editing-front-matter state)
       (edit-front-matter-form state)
       (render-edn-page state))]
    (if (and (not (:editing-item state)) (not (:editing-front-matter state)))
      (tooltip/Tooltipper. #js{ :watching ".edit-items-list"
                                :onPositionChange (fn [pos] (put! (:event-chan state) [:insert-position pos])) }
                           (sab/html
                            [:div.btn-group {:style {:width "200px" }}
                             [:button {:type "button"
                                       :onClick #(put! (:event-chan state) [:add-item {:type :heading}])
                                       :className "btn btn-default add-heading-item"} "Heading"]
                             [:button {:type "button"
                                       :onClick #(put! (:event-chan state) [:add-item {:type :markdown}])
                                       :className "btn btn-default add-text-item"} "Text"]
                             [:button {:type "button"
                                       :onClick (fn [_] (.click ($ "input.image-upload")))
                                       :className "btn btn-default add-image-item"} "Image"]]))
      [:span])
    [:div.hidden {:id "image-upload"}
     [:input.image-upload {:type "file"
                           ;; this isn't working, need to ask why
                           :onChange (fn [x]
                                       (let [event (.-nativeEvent x)]
                                         (put! (:event-chan state) [:image-selected event])))
                           :name "image-upload-file" }]]
    ]))



