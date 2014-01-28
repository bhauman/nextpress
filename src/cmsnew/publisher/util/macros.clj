(ns cmsnew.publisher.util.macros)

(defmacro chan->>
  "I'm using this macro while there is a memory leak in safari"
  [expr & body]
  (let [correct
        (reverse
         (mapv (fn [x]
                 (if (seq? x)
                   (cons 'clojure.core/partial x)
                   (list 'clojure.core/partial x))) body))]
    `((comp ~@correct) ~expr)))
