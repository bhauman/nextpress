(ns cmsnew.datastore.s3
  (:require
   [jayq.core :refer [$ append ajax inner css $deferred
                      when done resolve pipe on bind attr
                      offset] :as jq]
   [jayq.util :refer [log]]
   [goog.net.XhrIo]
   [goog.dom.xml]))

(defn xhr-connection [] (goog.net.XhrIo.))

(def signing-service-url "http://localhost:4567/signput")

(defn url-for-name-type [name mime-type]
  (str signing-service-url "?name=" name "&mime=" mime-type))

(defn xhr-request
  ([url callback method content headers timeoutInterval]
     (.send goog.net.XhrIo url (fn [e] (callback (.-target e)))
            method content headers timeoutInterval))
  ([url callback] (xhr-request url callback nil nil nil nil))
  ([url callback method] (xhr-request url callback method nil nil nil))
  ([url callback method content] (xhr-request url callback method content nil nil))
  ([url callback method content headers] (xhr-request url callback method content (clj->js headers) nil)))

(defn get-text [url callback]
  (xhr-request url (fn [resp] (callback (.getResponseText resp)))))

(defn get-version [url callback]
  (log url)
  (xhr-request url (fn [resp] (callback (.getResponseHeader resp "x-amz-version-id"))) "HEAD"))

(defn get-signed-put [name mime-type callback]
  (xhr-request
   (url-for-name-type name mime-type)
   (fn [e] (callback (.getResponseJson e)))))

(defn upload-data [url data mime-type callback]
  (xhr-request url callback "PUT" data {"Content-Type" mime-type "x-amz-acl" "public-read"}))

(defn save-data-to-file [path-name data mime-type callback]
  (get-signed-put path-name
                  mime-type
                  (fn [res]
                    (upload-data (.-puturl res) data mime-type callback))))

(defn extract-key-value [response key]
  (let [versions (.getElementsByTagName (.getResponseXml response) key)]
    (map (fn [x] (.-nodeValue (aget (.-childNodes (aget versions x)) 0)))
         (range (.-length versions)))
    ))

(defn extract-versions [response] (extract-key-value response "VersionId"))
(defn extract-paths [response] (extract-key-value response "Key"))

(defn get-versions-of-file [bucket path-name callback]
  (xhr-request (str "http://s3.amazonaws.com/" bucket "/?versions&prefix=" path-name)
               (fn [res] (callback (extract-versions res)))))

(defn get-bucket-list [bucket path-name callback]
  (xhr-request (str "http://s3.amazonaws.com/" bucket "/?&prefix=" path-name)
               (fn [res] (callback (extract-paths res)))))

