(ns cmsnew.publisher.plugins.source-file-renderer
  (:require
   [cmsnew.util.core :refer [self-assoc]]
   [cmsnew.publisher.rendering.base :refer [render-page-with-templates]]))

(defn source-file-renderer [key-to-render]
  (fn [[_ site]]
    (let [to-rend  (site key-to-render)
          rendered (->> (vals to-rend)
                        (map #(self-assoc % :rendered-body
                                          (partial render-page-with-templates
                                                   site)))
                        (map (juxt :path identity))
                        (into {}))]
      (update-in site
                 [:rendered-files]
                 (fn [files] (merge files rendered))))))
