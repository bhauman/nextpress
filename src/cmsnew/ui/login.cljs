(ns cmsnew.ui.login
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [sablono.core :as sab :include-macros true]   
   [cmsnew.ui.templates :as templ]
   [cmsnew.authorization.persona :as session]
   [reactor.core :refer [input-value react-render] :as rct]
   [cmsnew.publisher.util.log-utils :refer [ld lp log-chan]]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]
                   [reactor.macros :refer [owner-as]]))

(defn login-button [event-chan]
  (sab/html
   [:div.container.signin-area
    [:div.form-signin
     [:h2.form-signin-heading.center "please log in" ]
     [:button.btn.btn-primary.btn-block.btn-lg
      {:onClick #(do
                   (session/add-login-watch (fn [email]
                                              (put! event-chan [:login-successful email])))
                   (put! event-chan [:login-start])
                   (session/show-login-dialog))}
      "Sign In"]]]))

(defn loading-view [qualifier]
  (sab/html
   [:div.container.signin-area
    [:h2 qualifier]
    (templ/loading 85)]))

(defn login-loop []
  (let [event-chan (chan)
        html-node (.getElementById js/document "cmsnew")]
    (go
     (<! (react-render html-node (login-button event-chan)))
     (loop []
       (let [[msg email] (<! event-chan)]
         (ld [msg email])
         (condp = msg
           :login-start
           (do
             (<! (react-render html-node (loading-view "signing you in")))
             (recur))
           :login-successful email
           nil false
           (recur)
           ))))))
