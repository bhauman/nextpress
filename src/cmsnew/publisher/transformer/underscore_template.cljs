(ns cmsnew.publisher.transformer.underscore-template)

(defn render-template [template-string data]
  (.template js/_ template-string (clj->js data)))
