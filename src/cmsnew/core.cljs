(ns cmsnew.core
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [cmsnew.authorization.persona :as session]
   [cmsnew.datastore.s3 :as store]
   [cmsnew.publisher.core :as pub]

   [cmsnew.util.log-utils :refer [ld lp log-chan]]

   [cmsnew.ui.publisher-page :as publisher-page]
   [cmsnew.ui.edn-page-editor :as page-edit]
   [cmsnew.ui.site-selector :refer [select-site-loop]]
   [cmsnew.ui.page-selector :refer [select-page-loop]]
   [cmsnew.ui.login :refer [login-loop]]
   
   [cljs-uuid-utils :refer [make-random-uuid uuid-string]]
   [clojure.string :as string]
   [cljs.reader :refer [push-back-reader read-string]]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(enable-console-print!)

#_(go
 (let [url-config (<! (select-site-loop))
       signing-service (get-in url-config [:config :signing-service])
       site-url (:site-url url-config)
       user-email (<! (session/init signing-service))
       user-email (or user-email (<! (login-loop)))]
   (when user-email
     (let [site (<! (pub/create-site-for-url site-url))]
       #_(pub/clear-cache site)
       #_(publisher-page/init site)
       (<! (pub/blocking-publish site))
       (loop []
         (let [edn-page (<! (select-page-loop site))]
           (log edn-page)
           (<! (page-edit/edit-page site edn-page)))
         (recur))))))


