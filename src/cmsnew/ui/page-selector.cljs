(ns cmsnew.ui.page-selector
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [sablono.core :as sab :include-macros true]   
   [reactor.core :refer [input-value react-render] :as rct]
   [cmsnew.util.log-utils :refer [ld lp log-chan]]
   [clojure.string :as string]
   [cmsnew.publisher.core :as pub]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]
                   [reactor.macros :refer [owner-as]]))

(defn get-page-name [page]
  (-> (pub/make-target-path page)
      (pub/replace-extention "")))

(defn navbar [site]
  [:div.navbar.navbar-default
   [:a.navbar-brand { :href "#"} "Site pages"]])

(defn render-page [page event-chan]
  [:li.list-group-item
   [:a {:href "#"
        :onClick #(do (put! event-chan [:page-selected page]) false)}
    (get-page-name page)]])

(defn select-page-view [site event-chan]
  (sab/html
   [:div.select-pages
    (navbar site)
    [:div.container
     [:ul.list-group
      (let [pages (pub/get-edn-pages site)]
        (log pages)
        (map render-page
             pages
             (repeat event-chan))
        )]]
    ]))

(defn select-page-loop [site]
  (let [event-chan (chan)
        html-node (.getElementById js/document "cmsnew")]
    (jq/add-class ($ "body" ) "select-page")
    (go-loop []
             (<! (react-render html-node (select-page-view site event-chan)))
             (let [[msg data] (<! event-chan)]
               (ld [msg data])
               (if (= msg :page-selected)
                 data
                 (recur))))))

