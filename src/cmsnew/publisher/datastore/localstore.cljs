(ns cmsnew.publisher.datastore.localstore
  (:require
   [cmsnew.publisher.datastore.core :refer [FileLister
                                            PutFile
                                            GetFile
                                            ToSourceFile
                                            ResourcePath
                                            -store!
                                            -get-file
                                            list-files
                                            resource-path
                                            create-store]]
   [cmsnew.publisher.datastore.local-store-map :refer [LocalStoreMap]]
   [cmsnew.publisher.util.core :refer [starts-with?]]
   [clojure.string :as string]
   [jayq.util :refer [log]]
   [goog.crypt.Md5]))

(let [md (goog.crypt.Md5.)]
  (defn md5 [s]
    (.update md s)
    (apply
     str
     (map (fn [x]
            (str
             (if (> 16 x) "0" "")
             (.toString x 16))) (.digest md)))))

(defrecord LocalStore [map-atom]
  FileLister
  (list-files [this callback]
    (callback
     (map (fn [[k v]] {:path k
                      :etag (if v (md5 v) v) })
          @map-atom)))
  (list-files-with-prefix [this prefix callback]
    (list-files this
     (fn [files]
       (callback
        (filter (fn [x] (starts-with? (:path x) prefix))
                files)))))
  
  PutFile
  (-store! [this path data options callback]
    (swap! map-atom assoc (resource-path this path) data)
    (callback data))
  (-store! [this path data callback]
    (-store! this path data {} callback))  
  (-store-response-version [this resp] nil)
  (-store-response-success? [this resp] true)
  
  GetFile
  (-get-file [this path callback]
    (callback
     (get @map-atom (resource-path this path))))
  
  ToSourceFile
  (->source-file [this path file-response]
    {:path path
     :etag (md5 file-response)
     :body file-response})
  
  ResourcePath
  (resource-path [_ path] path))

(defmethod create-store :local [{:keys [path-prefix]}]
  (LocalStore. (atom (LocalStoreMap. path-prefix))))


(defn tests []
  (let [ls (create-store {:type :local :path-prefix "jaja"})]
    (log "running LocalStore tests")
    (-store! ls
             "_path/myfile.txt"
             "this is some data"
             identity)
    (-store! ls
             "_path/myfile2.txt"
             "this is some data2"
             identity)
    (-store! ls
             "_path/myfile3.txt"
             "this is some data3"
             identity)
    (-store! ls
             "_path/myfile4.txt"
             "this is some data4"
             identity)    

    (list-files ls (fn [r]
                     (log (prn-str r))))

    (-get-file ls "_path/myfile4.txt" log)
    (-get-file ls "_path/myfile3.txt" log)
    (-get-file ls "_path/myfile2.txt" log)
    (-get-file ls "_path/myfile.txt" log)    
    

    )

  
  
  )

#_(tests)
