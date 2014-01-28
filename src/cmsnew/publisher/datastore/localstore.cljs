(ns cmsnew.publisher.datastore.localstore
  (:require
   [cmsnew.publisher.datastore.core :refer [FileLister
                                            PutFile
                                            GetFile
                                            ToSourceFile
                                            ResourcePath
                                            list-files
                                            resource-path
                                            create-store]]
   [clojure.string :as string]
   [goog.crypt.Md5]))

(defn starts-with? [s prefix]
  (zero? (.indexOf s prefix)))

(let [md (goog.crypt.Md5.)]
  (defn md5 [s]
    (.update md s)
    (apply
     str
     (map (fn [x]
            (str
             (if (> 16 x) "0" "")
             (.toString x 16))) (.digest md)))))

(defrecord LocalStore [path-prefix]

  FileLister
  (list-files [this callback]
    (callback
     (map (fn [x] {:path x
                  :etag (md5
                         (.getItem js/localStorage
                                   (str (:path-prefix this) "::" x)))})
          (map (fn [x] (string/replace-first x (str path-prefix "::") ""))
               (filter (fn [x] (starts-with? x (str path-prefix "::")))
                       (map #(.key js/localStorage %)
                            (range (.-length js/localStorage))))))))
  (list-files-with-prefix [this prefix callback]
    (list-files this
     (fn [files]
       (callback
        (filter (fn [x] (starts-with? (:path x) prefix))
                files)))))
  
  PutFile
  (-store! [this path data options callback]
    (.setItem js/localStorage
              (resource-path this path)
              data)
    (callback data))
  (-store-response-version [this resp] nil)
  (-store-response-success? [this resp] true)
  
  GetFile
  (-get-file [this path callback]
    (callback
     (.getItem js/localStorage
               (resource-path this path))))
  
  ToSourceFile
  (->source-file [this path file-response]
    {:path path
     :etag (md5 file-response)
     :body file-response})
  
  ResourcePath
  (resource-path [this path]
    (str (:path-prefix this) "::" path)))

(defmethod create-store :local [{:keys [path-prefix]}]
  (LocalStore. path-prefix))
