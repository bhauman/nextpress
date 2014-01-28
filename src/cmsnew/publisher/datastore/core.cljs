(ns cmsnew.publisher.datastore.core
  (:require
    [cljs.core.async :as async :refer [chan put! close!]]))

(defprotocol FileLister
  (list-files [this callback])  
  (list-files-with-prefix [this prefix callback]))

(defprotocol PutFile
  (-store! [this path data options callback])
  (-store-response-success? [this response])
  (-store-response-version [this response])) 

(defprotocol GetFile
  (-get-file [this path callback]))

(defprotocol ToSourceFile
  (->source-file [this path file-response]))

(defprotocol ResourcePath
  (resource-path [this path]))

(defn get-source-file [st path callback]
  (-get-file st path (fn [f] (callback (->source-file st path f)))))

(defn store-source-file! [st source-file callback]
  (-store! st
           (:path source-file)
           (str (if-let [fm (:front-matter source-file)]
                  (prn-str fm) "")
                (:body source-file))
           {:mime-type "text/plain"}
           (fn [store-resp]
             (if (-store-response-success? st store-resp)
               (callback (assoc source-file :version
                                (-store-response-version st store-resp))) 
               (callback :failed)))))

(defn store-rendered-file! [st source-file callback]
  (-store! st
           (:target-path source-file)
           (:rendered-body source-file)           
           {:mime-type "text/html"}
           (fn [store-resp]
             (if (-store-response-success? st store-resp)
               (callback (assoc source-file :target-version
                                (-store-response-version st store-resp))) 
               (callback :failed)))))

(defn fetch-file [st path]
  (let [out (chan)]
    (get-source-file st
                     path
                     (fn [sf] (put! out sf) (close! out)))
    out))

(defn fetch-files [st paths]
  (async/merge (map (partial fetch-file st) paths)))

(defn store-rendered [st source-file]
  (let [out (chan)]
    (store-rendered-file! st
                          source-file
                          (fn [resp] (put! out resp) (close! out)))
    out))

(defn store-source [st source-file]
  (let [out (chan)]
    (store-source-file! st
                        source-file
                        (fn [resp] (put! out resp) (close! out)))
    out))

(defn store-files [st file-maps-list]
  (async/merge (map (partial store-rendered st) file-maps-list)))

(defn source-file-list [st]
  (let [out (chan)]
    (list-files-with-prefix st "_" #(do (put! out %) (close! out)))
    out))

(defmulti create-store #(:type %))
