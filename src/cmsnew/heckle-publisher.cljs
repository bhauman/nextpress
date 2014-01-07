(ns cmsnew.heckle-publisher
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [crate.core :as crate]
   [sablono.core :as sab :include-macros true]
   [cmsnew.authorization.persona :as session]
   [cmsnew.datastore.s3 :as store]
   [cmsnew.heckle :as heckle]
   [cmsnew.templates :as templ]
   [cmsnew.tooltipper :as tip]
   [cmsnew.log-utils :refer [ld lp log-chan]]
   [cmsnew.edn-page-editor :as page-edit]
   [cmsnew.async-utils :as async-util]

   [cljs-uuid-utils :refer [make-random-uuid uuid-string]]
   [clojure.string :as string]
   [cljs.reader :refer [push-back-reader read-string]]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(defn render-to
  ([react-dom html-node callback]
     (.renderComponent js/React react-dom html-node callback))
  ([react-dom html-node]
     (render-to react-dom html-node identity)))

(defn event-message [state message]
  (fn [] (put! (:event-chan state) message)))

(defn icon-for-log-message [log-message]
  (sab/html
   [:span.icon-holder
    (condp = (:type log-message)
      :source-files-changed [:span.glyphicon.glyphicon-file]
      :changes-detected [:span.glyphicon.glyphicon-file]    
      :downloading [:span.glyphicon.glyphicon-cloud-download]
      :uploading [:span.glyphicon.glyphicon-cloud-upload]    
      :notice [:span.glyphicon.glyphicon-circle-arrow-right]
      :processing [:span.glyphicon.glyphicon-cog]
      :published [:span.glyphicon.glyphicon-stop]
      [:span.glyphicon.glyphicon-circle-arrow-right]
      )]))

(defn render-log-item [log-message]
  (sab/html
   [:li.list-group-item 
    (icon-for-log-message log-message)
    (:msg log-message)
    (if (:list-data log-message)
     [:ul
      (map (fn [x] [:li (clj->js x)]) (:list-data log-message))]
     "")
    ]))

(defn publish-completed? [{:keys [messages]}]
  (-> messages last :type (= :published)))

(defn publish-area [index publish]
  (let [published (publish-completed? publish)]
    (sab/html
     [:div
      {:className (str "publish-area panel " (if (pos? index) "panel-default inactive" "panel-success"))} 
      [:div.panel-heading
       [:div.panel-title (str (if published "Published" "Publishing") " at ")
        [:span.date (.toLocaleTimeString (:time publish))]]]
      [:ul.list-group.logger-area
       (map render-log-item (:messages publish))
       ]])
    ))

(defn publishing-list [publishings]
  (sab/html [:div.publishing-list
            (map-indexed publish-area (take 6 publishings))]))

(defn publishing-page [state]
  (sab/html
   [:div {:className "publishing-page"}
    [:div {:className "navbar navbar-default"}
     [:div.container
      [:a {:className "navbar-brand" :href "#"} "Heckle Publisher"]
      [:ul.nav.navbar-nav
       [:li
        (if (:watching-files state)
          [:button.btn.btn-danger.navbar-btn
           {:type "button"
            :onClick #(put! (:event-chan state) [:watch-files-click])}
           [:span.glyphicon.glyphicon-refresh]
           " Watching Files"]
          [:button.btn.btn-default.navbar-btn
           {:type "button"
            :onClick #(put! (:event-chan state) [:watch-files-click])}
           "Watch Files"]
          )]
       [:li
        [:button.btn.btn-default.navbar-btn
         {:type "button"
          :onClick #(put! (:event-chan state) [:force-publish])}
         "Publish"]]
       ]]]
    [:div {:id "main-area" :className "container"}
     (publishing-list (:publishings state))
     ]]))

;; state transitions

(defn add-new-publish [state]
  (assoc state :publishings (vec (concat
                                  [{ :time (js/Date.)
                                    :messages [] }] 
                                  (:publishings state)))))

(defn add-publish-message [state message]
  (update-in state [:publishings 0 :messages]
             conj message))

(defn set-watching-files [state watching?]
  (assoc-in state [:watching-files] watching?))


;; application

(defn start-logger-loop [heckle-site]
  (let [publish-chan (async-util/partition-chan
                      #(= (:type %) :source-files-changed)
                      #(= (:type %) :published)                      
                      (:log-chan heckle-site))
        event-chan (chan)
        watching-chan (chan)
        state (atom { :publishings (list)
                      :watching-files false
                      :event-chan event-chan })
        render-to-node (.getElementById js/document "cmsnew")
        renderer (fn [state] (render-to (publishing-page state) render-to-node))]
    (add-watch state :state-changed (fn [_ _ _ nv] (renderer nv)))
    (renderer @state)

    (go-loop []
             (log (prn-str state))
             (let [publishing (<! publish-chan)]
               (swap! state add-new-publish)
               (<! (async/into []
                               (map< (fn [message]
                                       (swap! state add-publish-message message)
                                       message) publishing)))
               (recur)))
    
    (go-loop []
             (let [[msg data] (<! event-chan)]
               (log (prn-str [msg data]))
               (condp = msg
                 :watch-files-click (put! watching-chan 1)
                 :force-publish (heckle/publish heckle-site)          
                 true)
               (recur))
             )

    (go-loop []
             (<! watching-chan)
             (swap! state set-watching-files true)
             (loop []
               (heckle/publish heckle-site)
               (let [[v ch] (alts! [(timeout 8000) watching-chan])]
                 (if (= ch watching-chan)
                   (swap! state set-watching-files false)
                   (recur))))
             (recur))
    ))

(defn init [heckle-site]
  (start-logger-loop heckle-site))
