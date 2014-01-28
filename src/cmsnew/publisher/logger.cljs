(ns cmsnew.publisher.logger
  (:require
   [cljs.core.async :refer [put!]]))

(defrecord LogMsg [msg type data])

(defn logger
  ([system msg typ data]
     (if-let [log-chan (:log-chan system)]
       (put! log-chan (LogMsg. msg typ data))
       (println msg)))
  ([system msg typ]
     (logger system msg typ nil))
  ([system msg]
     (logger system msg nil nil)))
