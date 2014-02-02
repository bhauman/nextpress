(ns cmsnew.core
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [cmsnew.authorization.persona :as session]
   [cmsnew.publisher.datastore.s3 :as store]
   [cmsnew.publisher.datastore.cached-store]
   #_[cmsnew.publisher.core :as pub]

   [cmsnew.publishing-pipeline :as pub]
   
   [cmsnew.publisher.util.log-utils :refer [ld lp log-chan]]

   [cmsnew.ui.publisher-page :as publisher-page]
   [cmsnew.ui.edn-page-editor :as page-edit]
   [cmsnew.ui.site-selector :refer [select-site-loop]]
   [cmsnew.ui.page-selector :refer [select-page-loop]]
   [cmsnew.ui.login :refer [login-loop]]
   
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(enable-console-print!)

(go
 (let [] #_[url-config (<! (select-site-loop))
            signing-service (get-in url-config [:config :signing-service])
            site-url (:site-url url-config)
            user-email (<! (session/init signing-service))
            user-email (or user-email (<! (login-loop)))]
   (when true #_user-email
         (let [site (pub/create-site-for-url "http://nextpress-demo.s3-website-us-east-1.amazonaws.com")]
           ;; this loads everything
           (let [site (<! (pub/publish-site site))]
             (log (clj->js site))
             (loop []
               (let [edn-page (<! (select-page-loop site))]
                 (log edn-page)
                 (<! (page-edit/edit-page site edn-page)))
               (recur))         
         )))))
