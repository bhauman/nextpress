(ns cmsnew.util.core)

(defn self-assoc [x key f]
  (assoc x key (f x)))

(defn map-to-key [key x]
  (zipmap (map key x) x))

(defn find-first [f coll]
  (first (filter f coll)))