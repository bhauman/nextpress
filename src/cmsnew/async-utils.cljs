(ns cmsnew.async-utils
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(defn take-while [valid-pred in out]
    (go (loop []
          (if-let [v (<! in)]
            (do
              (put! out v)
              (if (not (valid-pred v))
                v
                (recur)))))))

(defn partition-chan
  ([start-pred in] (partition-chan start-pred (complement start-pred) in))
  ([start-pred end-pred in]
     (let [out (chan)]
       (go
        (loop []
          (if-let [val (<! in)]
            (do
              (if (start-pred val)
                (let [next-chan (chan)]
                  (>! out next-chan)
                  (>! next-chan val) ;; capture the first message
                  (<! (take-while (complement end-pred) in next-chan))
                  (close! next-chan)))
              (recur))
            (close! out))))
       out)))

