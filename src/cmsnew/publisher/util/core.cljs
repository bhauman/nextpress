(ns cmsnew.publisher.util.core)

(defn self-assoc [x key f]
  (assoc x key (f x)))

(defn map-to-key [key x]
  (zipmap (map key x) x))

(defn find-first [f coll]
  (first (filter f coll)))

(defn insert-at [items index item]
  (concat (take index items) [item] (drop index items)))

(defn map-in [col key f]
  (update-in col key (fn [c] (map f c))))
