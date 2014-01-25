(ns cmsnew.util.async-utils
  (:refer-clojure :exclude [take-while flatten])
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [cljs.core.async.impl.protocols :refer [Channel]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(defn chan? [c]
  (satisfies? Channel c))

(defn atom-chan [a]
  (let [out (chan)]
    (add-watch a :atom-change
               (fn [_ _ ov nv] (put! out [ov nv])))
    out))

(defn dev-null [in]
  (go-loop [v (<! in)]
           (if (nil? v) :closed (recur (<! in)))))

(defn map-to-atom
  ([atom input]
     (go-loop [v (<! input)]
              (reset! atom v)
              (recur (<! input))) 
     atom)
  ([input] (map-to-atom (atom {}) input)))

(defn flatten
  ([in out]
     (go
      (loop []
        (let [v (<! in)]
          (when-not (nil? v)
            (if (satisfies? Channel v)
              (<! (flatten v out))
              (put! out v))
            (recur))))))
  ([in]
     (let [out (chan)]
       (go
        (<! (flatten in out))
        (close! out))
       out)))

(defn flatten-chans [input]
  (let [out (chan)]
    (go-loop [chan-val (<! input)]
             (loop []
               (let [real-val (<! chan-val)]
                 (if (not (nil? real-val))
                   (do
                     (put! out real-val)
                     (recur)))))
             (recur (<! input)))
    out))

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
