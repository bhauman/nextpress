(ns cmsnew.publisher.plugins.core
  (:require
   [cmsnew.publisher.util.async-utils :refer [chan?]]
   [cljs.core.async :refer [<! close! put! chan]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop]]))

(defn plugin<
  "This creates a plugin that gets put into the chain of plugins. It
  takes a function and a input channel. The function is passed a
  vector of two values. The first is the state after the last run of
  the pipeline. The second is the current state of the system."
  [f input]
  (let [out (chan)]
    (go-loop []
             (let [v (<! input)]
               (if v
                 (let [[o n] v
                       res (f [o n])]
                   (put! out [o (if (chan? res) (<! res) res)])
                   (recur))
                 (close! out))))
    out))

(defn hook!<
  "This is for side effecting at a certain point in the pipeline. The
  function is given the current state of the system. The result of
  function is ignored."

  [f input]
  (plugin<
   (fn [[_ site]]
     (f site)
     site)
   input))
