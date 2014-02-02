(ns cmsnew.ui.templates
  (:require
   [crate.form :refer [form-to text-field text-area hidden-field
                       submit-button reset-button drop-down label]]
   [cmsnew.ui.tooltipper :as tooltip]
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [crate.core :as crate]
   [reactor.core :as react]
   [sablono.core :as sab :include-macros true]
   [cmsnew.publisher.transformer.markdown :refer [markdown-to-html]]
   [cmsnew.publisher.item-templates :refer [item-list item-container]]
   [cmsnew.ui.form-templates :as form :refer [control-group]]

    [cmsnew.publisher.site :as st]

   ;; importing edn-items
   [cmsnew.edn-page.item :refer [render-editable-item item-form]]
   [cmsnew.edn-page.items.heading]
   [cmsnew.edn-page.items.markdown]
   [cmsnew.edn-page.items.section]
   [cmsnew.edn-page.items.image]   

   [cmsnew.publisher.util.core :refer [insert-at]]
   [jayq.core :as jq :refer [$]]
   [jayq.util :refer [log]])
  (:require-macros
   [reactor.macros :as reactm]
   [cljs.core.async.macros :refer [go]]))

;; helpers

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

(defn item-under-edit [{:keys [editing-item]}] editing-item)

(defn item-being-edited? [{:keys [editing-item]} item]
  (and editing-item (= (:id item) (:id editing-item))))

(defn item-renderer [item state]
  (if (item-being-edited? state item)
    (sab/html (item-container (:id item) (:type item)
                              (sab/html (item-form (item-under-edit state) {} state))))
    (reactm/pure item (render-editable-item item state))))

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

(defn edit-front-matter-form [{:keys [event-chan editing-front-matter layout-list site] :as state}]
  (reactm/owner-as
   owner
   (sab/html
    (form-to {:onSubmit (react/form-submit owner event-chan :edit-settings.form-submit [:title :layout :published])}
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
                              layout-list
                              ))]
             [:div.checkbox
              [:label
               (sab/check-box {:ref "published"
                               :className "checkbox"
                               :defaultChecked (editing-front-matter :published)}
                              :published)
               "Published"]]
             (submit-button {:className "btn btn-primary"} "Save")
             (reset-button {:className "btn btn-default"
                            :onClick #(put! event-chan [:edit-settings.form-cancel])} "Cancel")))))

(defn self-reseting-file-input
  "This is a self toggling component. It rerenders a
   fresh file input after a file has been chosen.
   This is required to reset the filechooser so that it is ready
   for another file choice.
   Why? beacuse you are not allowed to set the input value of an
   input[type=file]"
  [state options]
  (reactm/owner-as
   owner
   (sab/html
    (if-not (react/get-state-val owner :removed)
      (let [on-change (:onChange options)
            new-on-change (fn [x]
                            (on-change x)
                            (go
                             (<! (timeout 100))
                             (react/set-state owner {:removed true})
                             (<! (timeout 1000))
                             (react/set-state owner {:removed false})))]
        [:input
         (assoc options :type "file" :onChange new-on-change)])
      [:span.resetting]))))

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
                            [:div.btn-group {:style {:width "271px" }}
                             [:button {:type "button"
                                       :onClick #(put! (:event-chan state) [:add-item {:type :heading}])
                                       :className "btn btn-default add-heading-item"} "Heading"]
                             [:button {:type "button"
                                       :onClick #(put! (:event-chan state) [:add-item {:type :markdown}])
                                       :className "btn btn-default add-text-item"} "Text"]
                             [:button {:type "button"
                                       :onClick #(put! (:event-chan state) [:add-item {:type :section}])
                                       :className "btn btn-default add-section-item"} "Section"]
                             [:button {:type "button"
                                       :onClick (fn [_]
                                                  (put! (:event-chan state) [:image-chooser-opened])
                                                  (.click ($ "input.image-upload")))
                                       :className "btn btn-default add-image-item"} "Image"]]))
      [:span])
    [:div.hidden
     (self-reseting-file-input state
                               {:className "image-upload"
                                :onChange (fn [x]
                                            (let [event (.-nativeEvent x)]
                                              (put! (:event-chan state) [:image-selected event])))
                                :name "image-upload-file" })]
    ]))
