(ns cmsnew.core
  (:require
   [cmsnew.datastore.s3 :as store]
   [markdown.core :as md]
   [jayq.util :refer [log]]))

(def system {
             :bucket "immubucket"
             :store-root "http://s3.amazonaws.com"
             :template-path "_layouts"
             :post-path "_posts"             
             })

(defn bucket-path [{:keys [store-root bucket]}]
  (str store-root "/" bucket))

(defn item-path [system path]
  (str (bucket-path system) "/" path))

(defn template-path [system path]
  (item-path system (str (:template-path system) "/" path)))

(defn post-path [system path]
  (item-path system (str (:post-path system) "/" path)))

(defn render-template [template-string data]
  (.template js/_ template-string (clj->js data)))

(defn render-template-to-path [system template-url data path]
  (store/get-text template-url
                  (fn [template-body]
                    (store/save-data-to-file path
                                             (render-template template-body data)
                                             "text/html"
                                             (fn [e] (log (.getResponseHeader e
                                                                             "x-amz-version-id")))))))

(store/get-bucket-list "immubucket" "_posts" (fn [x] (log (prn-str (doall x) ))))

(store/get-version (item-path system "index.html") (fn [x] (log (str "gversion" x))))

#_(store/get-text
 (post-path system "sample_post.md")
 (fn [md-body]
   (render-template-to-path system
                            (template-path system "default.html")
                            {:content_for_layout (md/mdToHtml md-body)}
                            "index.html")
   ))

#_(.log js/console
  (md/mdToHtml "##This is a heading\n\nwith a paragraph following it\n"))

#_(log (item-path system "index.html"))

#_(store/get-text (item-path system "_layouts/default.html")
                (fn [body]
                  (store/save-data-to-file "index.html"
                                           (render-template body {:content_for_layout "hey"})
                                           "text/html"
                                           (fn [e] (log (.getResponseHeader e
                                                                           "x-amz-version-id"))))
                  (log (str body))))

#_(log (render-template "<h1><%= name %></h2>" {:name "george"}))

#_(store/get-signed-put "testfile.json" "text/html" (fn [e] (log e)))

#_(store/save-data-to-file "index.html"
                         (render-template template {:content_for_layout body})
                         "text/html"
                         (fn [e] (log (.getResponseHeader e
                                                         "x-amz-version-id"))))

(store/get-versions-of-file "immubucket" "testerfile.json" (fn [x] (log (prn-str (doall x) ))))

