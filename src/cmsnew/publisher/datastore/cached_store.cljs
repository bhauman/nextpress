(ns cmsnew.publisher.datastore.cached-store
  (:require
   [cmsnew.publisher.datastore.core :refer [FileLister
                                            PutFile
                                            GetFile
                                            ToSourceFile
                                            ResourcePath
                                            list-files
                                            resource-path
                                            create-store
                                            -store!
                                            -store-response-version
                                            -store-response-success?
                                            ->source-file
                                            -get-file
                                            ]]
   [cmsnew.publisher.datastore.localstore]
   [cmsnew.publisher.datastore.local-store-map :refer [LocalStoreMap]]
   [cmsnew.publisher.util.core :refer [starts-with? changed-map-keys]]
   [cljs.reader :as reader]
   [clojure.string :as string]
   [jayq.util :refer [log]]))

(defrecord CachedStore [cache-atom datastore]
  ;; when you list files you invalidate the cache entries that have changed
  FileLister
  (list-files [this callback]
    (let [last-file-list (get @cache-atom :__file-list__)]
      (list-files datastore
                  (fn [file-list]
                    (let [old-path-etag-map (into {} (mapv (juxt :path :etag) last-file-list))
                          new-path-etag-map (into {} (mapv (juxt :path :etag) file-list))
                          changed-keys (changed-map-keys [old-path-etag-map new-path-etag-map])]
                      ;; here we are invalidating the cache
                      (doseq [p changed-keys]
                        (when (contains? @cache-atom p)
                          (swap! cache-atom dissoc p)))
                      (swap! cache-atom assoc :__file-list__ file-list)
                      (callback file-list))))))
  (list-files-with-prefix [this prefix callback]
    (list-files this
     (fn [files]
       (callback
        (filter (fn [x] (starts-with? (:path x) prefix))
                files)))))
  
  PutFile
  (-store! [this path data options callback]
    (-store! datastore path data options
             (fn [resp]
               (when (-store-response-success? datastore resp)
                 (swap! cache-atom assoc path data))
               (callback resp))))
  (-store! [this path data callback]
    (-store! this path data {} callback))  
  (-store-response-version [this resp]
    (-store-response-version datastore resp))
  (-store-response-success? [this resp]
    (-store-response-success? datastore resp))
  
  GetFile
  (-get-file [this path callback]
    (if-let [found-local (get @cache-atom path)]
      (callback found-local)
      (-get-file datastore path
                 (fn [res]
                   (swap! cache-atom assoc path res)
                   (callback res)))))
  
  ToSourceFile
  (->source-file [_ path file-response]
    (->source-file datastore path file-response))
  
  ResourcePath
  (resource-path [_ path] path))


(defn tests []
  (let [cache-atom (atom (LocalStoreMap. :devy))
        actst (create-store {:type :local
                             :path-prefix "test-dever"})
        st (CachedStore. cache-atom actst)]
    (log "running CachedStore tests ...")

    (doseq [[k v] @cache-atom]
      (swap! cache-atom dissoc k))
    
    (assert (zero? (count @cache-atom)))
    
    (-store! st "mypath1/" "mydata1" identity)

    (log @cache-atom)
    (log (get @cache-atom "mypath1/"))
    
    (assert (= 1 (count @cache-atom)))
    (assert (= "mydata1" (get @cache-atom "mypath1/")))

    (-get-file actst "mypath1/" (fn [res]
                                  (assert (= "mydata1" res))))

    (-get-file st "mypath1/" (fn [res]
                                  (assert (= "mydata1" res))))

    (-store! st "mypath1/" "mydata1-new" identity)

    (assert (= 1 (count @cache-atom)))
    (assert (= "mydata1-new" (get @cache-atom "mypath1/")))

    (-get-file actst "mypath1/" (fn [res]
                                  (assert (= "mydata1-new" res))))

    (-get-file st "mypath1/" (fn [res]
                                  (assert (= "mydata1-new" res))))    

    ;; changing the on the backing store will not change the data in
    ;; the cache
    (-store! actst "mypath1/" "mydata1-newer" identity)
    (-get-file actst "mypath1/" (fn [res]
                                  (assert (= "mydata1-newer" res))))
    
    
    (-get-file st "mypath1/" (fn [res]
                               (assert (= "mydata1-new" res))))
    
    ;; if we list the files though it should invalidate the file and
    ;; take it out of the cache
    (list-files st (fn [l]
                     ))

    (log (prn-str @cache-atom))
    ;; should get rid of the cache entry
    (assert (nil? (get @cache-atom "mypath1/")))
    ;; should only be the filelist
    (assert (= 1 (count @cache-atom)))
    ;; getting the file should install it into the cache
    (-get-file st "mypath1/" (fn [res]
                               (assert (= "mydata1-newer" res))))
    (log (prn-str @cache-atom))    
    (assert (= "mydata1-newer" (get @cache-atom "mypath1/")))
    
    ;; we can change the file in the underlying store
    (-store! actst "mypath1/" "mydata1-newer-file" identity)
    ;; but accessing it will return it from the cache
    (assert (= "mydata1-newer" (get @cache-atom "mypath1/")))
    (-get-file st "mypath1/" (fn [res]
                               (assert (= "mydata1-newer" res))))    
    ;; listing files will invalidate the cache and a get will fetch
    ;; the underlying value
    (list-files st
                (fn [l]
                  l))
    (assert (nil? (get @cache-atom "mypath1/")))
    (-get-file st "mypath1/" (fn [res]
                               (assert (= "mydata1-newer-file" res))))
        
    (log "finished tests")
    )
  )

#_(tests)
