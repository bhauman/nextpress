(ns cmsnew.publisher.plugins.source-file-renderer
  (:require
   [cmsnew.publisher.util.core :refer [self-assoc find-first]]
   [cmsnew.publisher.rendering.base :refer [render-page-with-templates]]))

(defn source-file-renderer [key-to-render]
  (let [page-data-lookup (fn [site source-name]
                           (find-first #(= source-name (:name %))
                                       (get-in site [:template-env :site key-to-render])))]
    (fn [[_ site]]
      (let [to-rend  (filter :published (vals (site key-to-render))) 
            rendered (->> to-rend
                          (map #(self-assoc % :rendered-body
                                            (partial render-page-with-templates
                                                     site
                                                     page-data-lookup)))
                          (map (juxt :path identity))
                          (into {}))]
        (update-in site
                   [:rendered-files]
                   (fn [files] (merge files rendered)))))))
