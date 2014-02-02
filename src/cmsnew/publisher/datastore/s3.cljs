(ns cmsnew.publisher.datastore.s3
  (:require
   [cljs-uuid-utils :refer [make-random-uuid uuid-string]]   
   [clojure.string :as string]
   [jayq.core :refer [$ append ajax inner css $deferred
                      when done resolve pipe on bind attr
                      offset] :as jq]
   [jayq.util :refer [log]]
   [goog.net.XhrIo]
   [goog.dom.xml]))

(def random-uuid (comp uuid-string make-random-uuid))

(defn xhr-connection [] (goog.net.XhrIo.))

(defn signing-service-url [signing-host]
  (str signing-host "/signput"))

(defn image-signing-service-url [signing-host]
  (str signing-host "/sign-file-post"))
 
(defn url-for-name-type [{:keys [signing-service] :as s3-store}
                         name mime-type]
  (str (signing-service-url signing-service) 
       "?name=" name "&mime=" mime-type))

(defn url-for-image-upload [s3-store path-name mime-type]
  (str (image-signing-service-url (:signing-service s3-store))
       "?name=" path-name "&type=" mime-type))

(defn xhr-request
  ([url callback method content headers timeoutInterval]
     (.send goog.net.XhrIo url (fn [e] (callback (.-target e)))
            method content headers timeoutInterval))
  ([url callback] (xhr-request url callback nil nil nil nil))
  ([url callback method] (xhr-request url callback method nil nil nil))
  ([url callback method content] (xhr-request url callback method content nil nil))
  ([url callback method content headers] (xhr-request url callback method content (clj->js headers) nil)))

(defn unquote-etag [etag]
  (get (string/split etag #"\"") 1))

;; switched to jquery because it provides the caching behavior I want
;; by default
(defn get-text [url callback]
  (.ajax
   js/jQuery 
   (clj->js
    {:type "GET"
     :cache false ; this appends _=timestamp to url
     :url url
     :success (fn [_ _ resp]
                (callback
                 {:headers { :etag (unquote-etag (.getResponseHeader resp "ETag"))
                            :content-type (.getResponseHeader resp "Content-Type")
                            :version (.getResponseHeader resp "x-amz-version-id") }
                  :body (.-responseText resp)}))})))

(defn get-version [url callback]
  (xhr-request url (fn [resp] (callback (.getResponseHeader resp "x-amz-version-id"))) "HEAD"))

(defn get-signed-put [s3-store name mime-type callback]
  (.ajax js/jQuery
         (clj->js {:type "GET"
                   :crossDomain true
                   :xhrFields { :withCredentials true }
                   :url (url-for-name-type s3-store name mime-type)
                   :success (fn [e] (callback e))})))

(defn upload-data [url data mime-type callback]
  (xhr-request url callback "PUT" data {"Content-Type" mime-type "x-amz-acl" "public-read"}))

(defn save-data-to-file [s3-store path-name data mime-type callback]
  (get-signed-put s3-store
                  path-name
                  mime-type
                  (fn [res]
                    (upload-data (.-puturl res) data mime-type callback))))

(defn get-signed-image-post [s3-store path-name filename mime-type callback]
  (.ajax js/jQuery
         (clj->js {:type "GET"
                   :crossDomain true
                   :xhrFields { :withCredentials true }
                   :url (url-for-image-upload s3-store path-name mime-type)
                   :success (fn [e] (callback (js->clj e :keywordize-keys true)))})))

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

(defn upload-image-file [s3-store path-name file callback fail-callback progress-callback]
  (get-signed-image-post s3-store path-name (.-name file) (.-type file)
                         (fn [{:keys [url fields] :as res}]
                           (log (prn-str res))
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

(defn node-list-seq [node-list]
  (map #(.item node-list %) (range (.-length node-list))))

(defn node-value [node-list]
  (.-textContent (aget node-list 0)))

(defn extract-tag-paths [response list-element child-tags]
  (let [xml (.getResponseXml response)
        contents (node-list-seq (.getElementsByTagName xml list-element))]
    (mapv (fn [c]
            (into {}
                  (map (fn [tag] [tag (node-value (.getElementsByTagName c tag))])
                       child-tags)))
          contents)))

(defn extract-path-etag [response]
  (map
     (fn [x] {:path (get x "Key")
             :etag (unquote-etag (get x "ETag"))})
     (extract-tag-paths response "Contents" ["Key" "ETag"])))

(defn get-versions-of-file [bucket path-name callback]
  (xhr-request (str "http://s3.amazonaws.com/" bucket "/?versions&prefix=" path-name)
               (fn [res]
                 (callback (extract-versions res)))))

(defn get-bucket-list [bucket path-name callback]
  (xhr-request (str "http://s3.amazonaws.com/" bucket "/?&prefix=" path-name)
               (fn [res]
                 (callback (extract-path-etag res)))))

(defn list-files [store path callback]
  (get-bucket-list (:bucket store) path callback))



(defn create-s3-store [signing-service-host bucket]
  {:type :s3
   :signing-service signing-service-host
   :bucket bucket})

