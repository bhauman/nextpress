(ns cmsnew.publisher.rendering.base
  (:require
   [cmsnew.publisher.paths :as paths]   
   [cmsnew.publisher.transformer.underscore-template :refer [render-template]]))

(defn add-template-helpers [site env]
  (merge
   (into {} (map (fn [[k v]] [k (v site)])
                 (:template-helpers site)))
   env))

(defn renderer-for-source-file [site source-file]
  (let [ext (keyword (paths/extention-from-path (:path source-file)))
        source-type   (get-in site [:source-ext-types ext])]
    (get-in site [:source-type-renderer source-type])))

(defn render-raw-page-without-context [site source-file]
  (let [renderer (renderer-for-source-file site source-file)]
    (renderer site source-file (:template-env site))))

;; so far this is the worst refactored of the bunch
;; we are dependent on the :pages key

;; and we are hard wired to underscore templates

(defn render-page-with-templates [site page-data-lookup source-file]
  (let [start-layout (get-in source-file [:front-matter :layout])
        page-key-data (page-data-lookup site (:name source-file))
        data-for-page
        (add-template-helpers site
                              ;; refering to :pages here is a problem
                              (merge {:page page-key-data}
                                     (:template-env site)))
        renderer (renderer-for-source-file site source-file)]
    (loop [layout start-layout
           content (renderer site source-file data-for-page)]
      ; should not be adding .html here
      (if-let [layout-source-file (get (site :layouts) (str layout ".html"))]
        (recur
         (get-in layout-source-file [:front-matter :layout])
         (render-template (:body layout-source-file)
                          (assoc data-for-page :content content)))
        content))))
