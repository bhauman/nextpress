(ns cmsnew.datastore.s3-store
  (:require
   [cmsnew.datastore.core :refer [FileLister
                                  PutFile
                                  GetFile
                                  ToSourceFile
                                  ResourcePath
                                  list-files
                                  resource-path
                                  create-store]]
   [cmsnew.datastore.s3]
   [clojure.string :as string]))

(defrecord S3Store [bucket signing-service]
  FileLister
  (list-files [this callback]
    (cmsnew.datastore.s3/list-files this "" callback))
  (list-files-with-prefix [this prefix callback]
    (cmsnew.datastore.s3/list-files this prefix callback))
  PutFile
  (-store! [this path data options callback]
    (store/save-data-to-file this
                             path
                             data
                             (or (:mime-type options) "text/plain")
                             callback))
  (-store-response-version [this resp]
    (.getResponseHeader resp "x-amz-version-id"))
  (-store-response-success? [this resp]
    ;; XXX we need to implement this
    true)
  GetFile
  (-get-file [this path callback]
    (store/get-text (resource-path this path) callback))
  ToSourceFile
  (->source-file [this path file-response]
    {:path path
     :etag (get-in file-response [:headers :etag])
     :version (get-in file-response [:headers :version])
     :body (:body file-response)})
  ResourcePath
  (resource-path [this path]
    (str "http://s3.amazonaws.com/" bucket "/" path)))

(defmethod create-store :s3 [{:keys [bucket signing-service]}]
  (S3Store. bucket signing-service))

