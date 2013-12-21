(ns cmsnew.datastore.s3
  (:require
   [cljs-uuid-utils :refer [make-random-uuid uuid-string]]   
   [jayq.core :refer [$ append ajax inner css $deferred
                      when done resolve pipe on bind attr
                      offset] :as jq]
   [jayq.util :refer [log]]
   [goog.net.XhrIo]
   [goog.dom.xml]))

(def random-uuid (comp uuid-string make-random-uuid))

(defn xhr-connection [] (goog.net.XhrIo.))

(def signing-service-url "http://localhost:4567/signput")

(def image-signing-service-url "http://localhost:4567/sign-image-post")

(defn url-for-name-type [name mime-type]
  (str signing-service-url "?name=" name "&mime=" mime-type))

(defn url-for-image-upload [filename uuid mime-type]
  (str image-signing-service-url "?filename=" filename "&uuid=" uuid "&type=" mime-type))

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

(defn get-signed-image-post [uuid filename mime-type callback]
  (xhr-request (url-for-image-upload filename uuid mime-type)
               (fn [e] (callback (js->clj (.getResponseJson e) :keywordize-keys true)))))

(defn make-form-data [file fields]
  (let [fd (js/FormData.)]
    (mapv (fn [[k v]] (.append fd (name k) v)) fields)
    (.append fd "file" file)
    fd))

(defn upload-form-data [url form-data callback fail-callback progress-callback]
  (let [xhr (js/XMLHttpRequest.)]
    (.addEventListener (.-upload xhr) "progress"
                       (fn [e]
                         (let [percent (.floor js/Math (* 100 (/ (.-loaded e) (.-total e))))]
                           (progress-callback percent))))
    (.addEventListener xhr "load"
                       (fn [e]
                         (log xhr)
                         (if (= 204 (.-status xhr))
                           (callback xhr)
                           (fail-callback xhr)
                           )))
    (.open xhr "POST" url true)
    (.send xhr form-data)))

(defn upload-image-file [uuid file callback fail-callback progress-callback]
  (get-signed-image-post uuid (.-name file) (.-type file)
                         (fn [{:keys [url fields] :as res}]
                           (let [form-data (make-form-data file fields)
                                 file-url (str url (:key fields))]
                             (log "uploaded url: " file-url)
                             (upload-form-data url form-data
                                               (fn [x] (callback file file-url))
                                               (fn [x] (fail-callback file file-url))
                                               progress-callback)))))

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

