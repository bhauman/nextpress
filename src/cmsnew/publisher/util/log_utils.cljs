(ns cmsnew.publisher.util.log-utils
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

;; logging utils
(defn ld [arg]
  "log data"
  (log (clj->js arg)))

(defn lp [arg]
  "print edn data"
  (log (prn-str arg)))

(defn log-chan [in] (map< #(do (ld %) %) in))
