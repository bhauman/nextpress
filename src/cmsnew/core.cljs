(ns cmsnew.core
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
   [cmsnew.heckle-publisher :as publisher]   
   [cljs-uuid-utils :refer [make-random-uuid uuid-string]]
   [clojure.string :as string]
   [cljs.reader :refer [push-back-reader read-string]]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(go
 (let [heckle-site (<! (heckle/create-heckle-for-url
                        "http://immubucket.s3-website-us-east-1.amazonaws.com"))
       user-email (<! (session/init (:signing-service heckle-site)))
       user-email (<! (session/get-login (:signing-service heckle-site)))
       source-files @(:source-files heckle-site) 
       pages (heckle/get-pages heckle-site source-files)
       orig-edn-page (first (filter heckle/edn-page? pages))
       page-items (get-in orig-edn-page [:front-matter :items])
       start-edn-page (assoc-in orig-edn-page [:front-matter :items] page-items)]
   (ld start-edn-page)

   (page-edit/edit-page heckle-site start-edn-page)

   #_(publisher/init heckle-site)
   #_(heckle/publish heckle-site)
   ))

