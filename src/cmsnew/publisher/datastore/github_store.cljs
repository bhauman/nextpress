(ns cmsnew.publisher.datastore.github-store
  (:require
   [cmsnew.publisher.datastore.core :refer [FileLister
                                            PutFile
                                            GetFile
                                            ToSourceFile
                                            ResourcePath
                                            -store!
                                            -get-file
                                            ->source-file
                                            list-files
                                            list-files-with-prefix
                                            resource-path
                                            create-store]]
   [cmsnew.publisher.util.core :refer [starts-with?]]
   [cmsnew.publisher.datastore.githubber :as github]
   [clojure.string :as string]
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [jayq.util :refer [log]])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(defrecord GithubStore [owner repo access-token]
  FileLister
  (list-files [this callback]
    (go
     (let [file-list
           (<! (github/list-files
                this repo "master"))
           fl (filter #(= (:type %) "blob") (:tree file-list)) 
           files (map
                  (fn [x] {:path (:path x)
                          :etag (:sha x)})
                  fl)]
       (log (clj->js fl))
       (callback files))))
  (list-files-with-prefix [this prefix callback]
    (list-files this
                (fn [files]
                  (callback (filter (fn [x] (starts-with? (:path x) prefix))
                                    files)))))
  
  PutFile
  (-store! [this path data options callback]
    (go
     (let [file (<! (github/get-file this path))
           resp (if (not (:request-error file))
                  (<! (github/update-file this
                                     {:path path
                                      :sha (:sha file)}
                                     data))
                  (<! (github/create-file this
                                          path
                                          data)))]
       ;; simply keep the gh-pages branch a copy of master
       (github/update-ref this
                          "gh-pages" (get-in resp [:commit :sha])
                          :force true)
       (callback resp))))
  
  (-store! [this path data callback]
    (-store! this path data {} callback))

  (-store-response-version [this resp]
    (get-in resp [:content :sha]))
  (-store-response-success? [this resp]
    (not (:request-error resp)))
  
  GetFile
  (-get-file [this path callback]
    (go
     (let [file-data (<! (github/get-file
                          this path))
           file-body (<! (github/get-file
                              this path :raw true))]
       (callback {:etag (:sha file-data)
                  :body file-body}))))
  
  ToSourceFile
  (->source-file [this path file-response]
    {:path path
     :etag (:etag file-response)
     :version (:etag file-response)
     :body (:body file-response)})
  
  ResourcePath ;; this may not make sense for this data store
  (resource-path [this path]
    (str "https://api.github.com/repos/" owner "/" repo "contents/" path "?ref=master")))

(def connect-data { :type :github
                    :owner "bhauman"
                    :repo "test-website"
                    :client_id "01d0480f7420a8e45aac"
                    :redirect_uri "http://nextpress.dev/test.html"
                    :login_server "http://localhost:4567" })

(defn authorize [connect-data callback]
  (github/get-authorized-access-token
   (assoc (select-keys connect-data [:client_id :redirect_uri :login_server])
     :scope "repo")
   callback))

(defmethod create-store :github [{:keys [owner repo access-token]}]
  (GithubStore. owner repo access-token))

#_(go
 (authorize connect-data (fn [x] (log (clj->js x))))
 
 #_(let [st (create-store {:type :github
                         :owner "bhauman"
                         :repo "test-website"
                         :access-token (github/get-access-token)})
       ]
   (-store! st "index.html" "<html>helloer3333</html>"
            (fn [x]
              (log (clj->js x))
              )
            )
   
   )
 )
