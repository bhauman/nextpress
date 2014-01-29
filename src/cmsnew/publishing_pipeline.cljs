(ns cmsnew.publishing-pipeline
  (:require
   [cmsnew.publisher.logger :refer [logger]]

   [cmsnew.publisher.util.core :refer [find-first]]

   
   [cmsnew.publisher.transformer.markdown :refer [markdown-to-html]]
   [cmsnew.publisher.transformer.underscore-template :refer [render-template]]

   [cmsnew.publisher.plugins.base :refer [add-default
                                          add-template-helpers
                                          register-template-helper
                                          register-page-renderer
                                          source-extention->source-type
                                          
                                          get-source-file-list
                                          changed-source-files
                                          changed-rendered-files
                                          fetch-changed-source-files
                                          store-changed-rendered-files
                                          
                                          parse-data-files
                                          parse-partials
                                          parse-layouts
                                          parse-posts
                                          parse-pages
                                          get-sections
                                         
                                          posts-into-templ-env
                                          pages-into-templ-env
                                          data-into-templ-env
                                          merge-front-matter-into-page-templ-env
                                          merge-front-matter-into-post-templ-env
                                          add-rendered-content-to-pages-templ-env
                                          add-rendered-content-to-posts-templ-env
                                          ]]

   [cmsnew.publisher.plugins.core :refer [plugin<]]
   
   [cmsnew.publisher.plugins.source-file-renderer :refer [source-file-renderer]]

   [cmsnew.publisher.datastore.core :refer [
                                            source-file-list
                                            fetch-file
                                            store-source
                                            create-store]]

   [cmsnew.publisher.datastore.localstore :refer [LocalStore]]
   [cmsnew.publisher.datastore.s3-store :refer [S3tore]]
   
   [cmsnew.edn-page.rendering :refer [render-edn-page]]
   [cmsnew.edn-page.plugins :refer [page-sections]]
   [cljs.reader :refer [read-string]]
   
   [cljs.core.async :as async
    :refer [chan close! put! to-chan map< <!]]
   [jayq.core :as jq]
   [jayq.util :refer [log]])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go alt! go-loop]]
   [cmsnew.publisher.util.macros :refer [chan->>]]))

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

;; local plugins

(defn get-config [site-url]
  (let [out (chan)]
    (.get js/$ (str site-url "/_config.edn" )
          (fn [e]
            (put! out (-> e read-string))
            (close! out)))
    out))

(defn load-config
  "conditionally loads the config if it hasn't been loaded"
  [[o-site site]]
  (let [out (chan)]
    (go
     (let [config-data (<! (get-config (:site-url site)))]
       (if-not (:config-file-data site)
         (do
           (logger site "Loading config file ...")
           (put! out (assoc site
                       :config-file-data
                       config-data)))
         (put! out site))
       (close! out)))
    out))

(defn merge-config [[_ site]]
  "Puts config data into the base level of the state."
  (merge site (:config-file-data site)))

(defn create-storage [[_ site]]
  (if-not (:store site)
    (assoc site :store (create-store (:datastore site)))
    site))

(defn render-pipeline [system-chan]
  (chan->> system-chan
           (map< (juxt identity identity))
           (plugin< load-config)
           (plugin< merge-config)
           (plugin< create-storage)
           (plugin< (add-default :page-path "_site_src"))
           (plugin< (add-default :layout-path "_layouts"))
           (plugin< (add-default :partial-path "_partials"))
           (plugin< (add-default :post-path "_posts"))
           (plugin< (add-default :data-path "_data"))           
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
           (plugin< add-rendered-content-to-pages-templ-env)
           (plugin< add-rendered-content-to-posts-templ-env)
           (plugin< page-sections)
           (plugin< (source-file-renderer :pages))
           (plugin< (source-file-renderer :posts))
           (plugin< changed-rendered-files)
           (plugin< store-changed-rendered-files)))

(defn create-site-for-url [url]
  (let [in-chan (chan)]
    { :site-url url
      :log-chan (chan)
      :config-file-data { :datastore { :type :local
                                      :path-prefix "development" }}
      :pipeline-input in-chan 
      :pipeline-output (render-pipeline in-chan)}))

;; not digging this, seems fragile
(defn publish-site [{:keys [pipeline-input pipeline-output] :as site}]
  (go
   (>! pipeline-input (dissoc site
                              :pipeline-output
                              :pipeline-input))
   (assoc (last (<! pipeline-output))
     :pipeline-input pipeline-input
     :pipeline-output pipeline-output)))


#_(let [system (create-site-for-url "http://nextpress-demo.s3-website-us-east-1.amazonaws.com")]
  (go
   (log (clj->js (<! (publish-site system))))))

;; this is just a development helper
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
             (let [file (<! (fetch-file s3 p))
                   res  (<! (store-source loc file))]
               (log (clj->js file))
               (recur (rest paths)))
             (log (clj->js (<! (source-file-list loc)))))))))))


#_(load-localstore)

