(ns cmsnew.site
  (:require
   [cmsnew.datastore.s3 :as store]
   [jayq.core :refer [$ append ajax inner css $deferred
                      when done resolve pipe on bind attr
                      offset] :as jq]
   [jayq.util :refer [log]]))



