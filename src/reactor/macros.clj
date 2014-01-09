(ns reactor.macros)

(defmacro pure [value children]
  `(reactor.core/Pure. (cljs.core/js-obj "value" ~value) (fn [] ~children)))

(defmacro defpure [name bindings & body]
  `(let [render-func# (fn ~bindings ~@body)]
     (defn ~name [& args#]
       (pure args# (apply render-func# args#)))))

