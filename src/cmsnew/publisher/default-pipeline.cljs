(ns cmsnew.publisher.default-pipeline
  (:require
   [cmsnew.util.core :refer [find-first]]
   
   [cmsnew.transformer.markdown :refer [markdown-to-html]]
   [cmsnew.transformer.underscore-template :refer [render-template]]
   [cmsnew.publisher.rendering.edn-page :refer [render-edn-page]]

   [cmsnew.publisher.plugins.base :refer [add-template-helpers
                                          register-template-helper
                                          register-page-renderer
                                          source-extention->source-type
                                          
                                          get-source-file-list
                                          changed-source-files
                                          fetch-changed-source-files
                                          
                                          parse-data-files
                                          parse-partials
                                          parse-layouts
                                          parse-posts
                                          parse-pages
                                          get-sections
                                          page-sections
                                         
                                          posts-into-templ-env
                                          pages-into-templ-env
                                          data-into-templ-env
                                          merge-front-matter-into-page-templ-env
                                          merge-front-matter-into-post-templ-env
                                          add-rendered-content-to-pages-templ-env
                                          add-rendered-content-to-posts-templ-env
                                          ] :as plg]

   [cmsnew.publisher.plugins.source-file-renderer :refer [source-file-renderer]]
   
   [cmsnew.datastore.core :refer [
                                  source-file-list
                                  fetch-file
                                  store-source
                                  create-store]]
   [cmsnew.datastore.localstore :refer [LocalStore]]
   [cmsnew.datastore.s3-store :refer [S3tore]]  
   
   [cljs.core.async :as async
    :refer [chan close! put! to-chan map<]]
   [jayq.util :refer [log]])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go alt! go-loop]]
   [cmsnew.util.macros :refer [chan->>]])
  )

;; register page renderer

(defn markdown-renderer [site source-file env-data]
  (markdown-to-html (:body source-file)))

(defn edn-page-renderer [site source-file env-data]
  (render-edn-page site source-file))

(defn underscore-template-renderer [site source-file env-data]
  (render-template (:body source-file) env-data))

;; register template helper

(defn- get-page-by-path* [site page-path]
  (let [pages (get-in site [:template-env :site :pages])]
    (find-first #(= (:path %) page-path) pages)))

(defn get-page-helper [site]
  (fn [page-path]
    (clj->js (get-page-by-path* site page-path))))

(defn include-page-helper [site]
  (fn [page-path] (:content (get-page-by-path* site page-path))))

(defn render-partial-helper [site]
  (fn [partial-path partial-data]
    (if-let [partial ((:partials site) partial-path)]
      (let [env (merge (add-template-helpers site
                                             (:template-env site)) 
                       (js->clj partial-data))]
        (render-template (:body partial) env))
      "null partial")))

(defn plugin< [f input]
  (let [out (chan)]
    (go-loop []
             (let [v (<! input)]
               (if v
                 (let [[o n] v
                       res (f [o n])]
                   (put! out [o (if (.-takes res) (<! res) res)])
                   (recur))
                 (close! out))))
    out))

(defn render-pipeline [system-chan]
  (chan->> system-chan
           (map< (juxt identity identity))
           (plugin< (source-extention->source-type "md" "text/markdown"))
           (plugin< (source-extention->source-type "edn" "text/edn"))
           (plugin< (source-extention->source-type "html" "text/html"))
           (plugin< (register-page-renderer "text/markdown" markdown-renderer))
           (plugin< (register-page-renderer "text/edn" edn-page-renderer))
           (plugin< (register-page-renderer "text/html" underscore-template-renderer))
           (plugin< (register-template-helper :getPage get-page-helper))
           (plugin< (register-template-helper :includePage include-page-helper))
           (plugin< (register-template-helper :renderPartial render-partial-helper))           
           (plugin< get-source-file-list)
           (plugin< changed-source-files)
           (plugin< fetch-changed-source-files)
           (plugin< parse-pages)
           (plugin< parse-posts)           
           (plugin< parse-layouts)
           (plugin< parse-partials)
           (plugin< parse-data-files)
           (plugin< data-into-templ-env)
           (plugin< pages-into-templ-env)
           (plugin< posts-into-templ-env)
           (plugin< merge-front-matter-into-page-templ-env)
           (plugin< merge-front-matter-into-post-templ-env)
           ;; should break different rendering types into different
           ;; plugins (.ie md, edn, html) that way we can add
           ;; as many renderers as we want and can replace them as well
           (plugin< add-rendered-content-to-pages-templ-env)
           (plugin< add-rendered-content-to-posts-templ-env)
           ;; (plugin< page-includes-in-templ-env)
           ;; (plugin< get-page-in-templ-env)           
           (plugin< page-sections)
           ;; (plugin< render-partials)
           (plugin< (source-file-renderer :pages))
           
           ))

(let [lc (chan)
      in (to-chan [{ :log-chan lc
                     :page-path "_site_src"
                     :layout-path "_layouts"
                     :partial-path "_partials"                    
                     :post-path "_posts"
                     :data-path "_data"
                    :store
                    #_(create-store { :type :s3
                                     :bucket "nextpress-demo"
                                     :signing-service "http://localhost:4567"})
                    (create-store { :type :local
                                   :path-prefix "development"})
                    }])
      p (render-pipeline in)]
  (go-loop []
        (let [msg (<! lc)]
          (when msg
            (log (:msg msg))
            (recur))))
    
  (go (log (clj->js (last (<! p))))))

(defn load-localstore []
  (let [s3 (create-store { :type :s3
                          :bucket "nextpress-demo"
                          :signing-service "http://localhost:4567"})
        loc (create-store { :type :local
                           :path-prefix "development"})]
    (go
     (let [start-paths (mapv :path (<! (source-file-list s3)))]
       (loop [paths start-paths]
         (let [p (first paths)]
           (if p
             (let [file (<! (fetch-file {:store s3} p))
                   res  (<! (store-source {:store loc} file))]
               (log (clj->js file))
               (recur (rest paths)))
             (log (clj->js (<! (source-file-list loc)))))))))))

#_(load-localstore)

#_(let [st (S3Store. "nextpress-demo"
                   "http://localhost:4567")]
    #_(store! st "_data1.txt" "1" identity)
    #_(store! st "data2" 2 identity)
    #_(store! st "data3" 3 identity)
    
    #_(store! st "rata1" 1 identity)
    #_(store! st "rata2" 2 identity)
    #_(store! st "rata3" 3 identity)
    
    (get-source-file st "_config.edn" (fn [x] (log x)
                                        (store-source-file! st x (fn [result] (log result) ))
                                        ))
    (list-files st (fn [list] (log (clj->js list))))
    (list-files-with-prefix st "_" (fn [list] (log (clj->js list)))))

