(ns cmsnew.heckle-publisher
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [crate.core :as crate]
   [cmsnew.authorization.persona :as session]
   [cmsnew.datastore.s3 :as store]
   [cmsnew.heckle :as heckle]
   [cmsnew.templates :as templ]
   [cmsnew.tooltipper :as tip]
   [cmsnew.log-utils :refer [ld lp log-chan]]
   [cmsnew.edn-page-editor :as page-edit]
   [cljs-uuid-utils :refer [make-random-uuid uuid-string]]
   [clojure.string :as string]
   [cljs.reader :refer [push-back-reader read-string]]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(defn publishing-page [heckle-site]
  [:div {:class "publishing-page"}
   [:div {:class "navbar navbar-default"}
    [:a {:class "navbar-brand" :href "#"} "Heckle Publisher"]
    [:ul.nav.navbar-nav
     [:li
      [:button.btn.btn-default.navbar-btn {:type "button"} "Publish"]]
     [:li
      [:button.btn.btn-default.navbar-btn {:type "button"} "Watch Files"]]     
     ]
    ]
   [:div {:id "main-area" :class "container"}
    [:ul.list-group.logger-area]
    ]])

(defn render-log-item [log-message]
  [:li.list-group-item (:msg log-message)
   (if (:list-data log-message)
     [:ul
      (map (fn [x] [:li (clj->js x)]) (:list-data log-message))]
     "")
   ])

(defn start-logger-loop [heckle-site]
  (-> ($ "#cmsnew")
      (jq/html (crate/html (publishing-page heckle-site))))
  (go-loop []
           (let [log-message (<! (:log-chan heckle-site))]
             (jq/append ($ ".logger-area") (crate/html (render-log-item log-message))))
           (recur)))

(defn init [heckle-site]
  (start-logger-loop heckle-site)
  (go-loop []
           (<! (timeout 8000))
           (heckle/publish heckle-site)
           (recur)))


