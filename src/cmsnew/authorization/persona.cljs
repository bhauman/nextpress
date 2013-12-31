(ns cmsnew.authorization.persona
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]   
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(def logged-in-user (atom nil))

(defn on-login [signing-server-host assertion]
  (.ajax js/jQuery (clj->js {:type "POST"
                             :crossDomain true
                             :xhrFields { :withCredentials true }
                             :url (str signing-server-host "/auth/login") 
                             :data { :assertion assertion }
                             :success (fn [res status xhr]
                                        (log res)
                                        (reset! logged-in-user (.-email res)))
                             :error   (fn [xhr status err]
                                        (log "Login failed"))})))

(defn on-logout [signing-server-host]
  (.ajax js/jQuery (clj->js {:type "POST"
                             :crossDomain true
                             :xhrFields { :withCredentials true }
                             :url (str signing-server-host "/auth/logout") 
                             :success (fn [res status xhr]
                                        (reset! logged-in-user nil))
                             :error   (fn [xhr status err]
                                        (log "Logout failed"))})))

(defn setup-persona-watchers [signing-server-host user-email]
  (.watch (.-id js/navigator)
        (clj->js
         {:loggedInUser user-email
          :onlogin (partial on-login signing-server-host)
          :onlogout (partial on-logout signing-server-host)})))

(defn show-login-dialog []
  (-> js/navigator .-id .request))

(defn logout []
  (-> js/navigator .-id .logout))

(defn check-logged-in-status [signing-server-host]
  (let [out (chan)]
    (.ajax js/jQuery
           (clj->js {:type "GET"
                     :crossDomain true
                     :xhrFields { :withCredentials true }
                     :url (str signing-server-host "/logged_in_user")
                     :success (fn [d]
                                (let [logged-in-user (.-logged_in_user d)]
                                  (put! out  (if logged-in-user logged-in-user false))
                                  (close! out)))}))
    out))

(defn add-login-watch [callback]
  (add-watch logged-in-user :login-watch (fn [_ _ ov nv]
                                           (when (and (nil? ov)
                                                      (not (nil? nv)))
                                             (do
                                               (callback nv)
                                               (remove-watch logged-in-user :login-watch))))))

(defn init [signing-server-host]
  (go
   (let [user-email (<! (check-logged-in-status signing-server-host))]
     (if user-email
       (setup-persona-watchers signing-server-host user-email)
       (setup-persona-watchers signing-server-host nil))
     user-email)))

(defn get-login [signing-server-host]
  (let [out (chan)]
    (go
     (let [user-email (<! (check-logged-in-status signing-server-host))]
       (if user-email
         (do
           (reset! logged-in-user user-email)
           (put! out user-email))
         (do
           (add-login-watch (fn [email]
                              (log (str "email: " email))
                              (put! out email)))
           (show-login-dialog)))))
    out))
