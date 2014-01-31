(ns cmsnew.publisher.datastore.local-store-map
  (:require
   [cljs.reader :as reader]
   [clojure.string :as string]
   [jayq.util :refer [log]]))

(defn- make-cache-key [ns k]
  (prn-str { :ns ns :k k}))

(deftype LocalStoreMap [ns]
  ILookup
  (-lookup [_ k not-found]
    (if-let [res (.getItem js/localStorage (make-cache-key ns k))]
      (reader/read-string res)
      not-found))
  (-lookup [o k]
    (-lookup o k nil))
  IAssociative
  (-contains-key? [o k]
    ;; bad implementation but works for this case and is fast
    (if (nil? (.getItem js/localStorage (make-cache-key ns k)))
      false
      true))
  (^clj -assoc [o k v]
    (.setItem js/localStorage
              (make-cache-key ns k)
              (prn-str v))
    o)
  IMap
  (^clj -dissoc [o k]
    (.removeItem js/localStorage (make-cache-key ns k))
    o)
  ISeqable
  (-seq [o]
    (let [ks
          (mapv :k
               (filter
                (fn [k]
                  (and (map? k)
                       (= ns (:ns k))))
                (keep (fn [i]
                       (if-let [r (.key js/localStorage i)]
                         (reader/read-string r)
                         i))
                      (range (.-length js/localStorage)))))]
      (mapv (fn [k] [k (get o k)]) ks))))

(defn tests []
  (let [cache (LocalStoreMap. "whaaaaaa")]
    (log "running LocalStoreMap tests")


    (doseq [[k v] cache]
      (dissoc cache k))
    
    (assert (= 0 (count cache)))
    
    #_(log (prn-str (count (map identity cache))))
    
    (assert (assoc cache :funner "this is fun"))
    (assoc cache :runner-boy "runner boy")
    (assert (nil? (get cache :funnerer)))
    (assert (= "this is fun" (get cache :funner)))
    (assert (nil? (:funnerer cache)))
    (assert (= "this is fun" (:funner cache)))        

    (assert (= false (-contains-key? cache :funnerer)))
    (assert (= true  (-contains-key? cache :funner)))
    
    (assert (nil? (:funner (dissoc cache :funner))))
    (assert (= "runner boy" (:runner-boy cache)))

    (assoc cache :runner-boy2 "runner boy2")
    (assoc cache :runner-boy3 "runner boy3")
    (assoc cache :runner-boy4 "runner boy4")
    
    (assert (= {:runner-boy "runner boy"
                :runner-boy2 "runner boy2"
                :runner-boy3 "runner boy3"
                :runner-boy4 "runner boy4"} (into {} (map identity cache))))
    
    (assert (= 4 (count cache)))
    (log "finished tests")
    ))

(tests)

