(ns cmsnew.publisher.datastore.githubber
  (:require
   [jayq.core :refer [$ ajax] :as jq]
   [jayq.util :refer [log]]
   [clojure.string :as string]
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [goog.crypt.base64])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go alt! go-loop]])  )

;; authorization

(def default-connect-data
  {:client_id "01d0480f7420a8e45aac"
   :redirect_uri "http://githubber.dev/index.html"
   :scope "repo"})

(defn parse-url [url]
  (into {}
        ((juxt
          #(vector :path (first %))
          #(vector :query (second %)))
         (string/split url #"\?"))))

(defn parse-query [query]
  (into {}
        (map (fn [part]
               ((juxt (comp keyword js/decodeURIComponent first)
                      (comp js/decodeURIComponent second))
                (string/split part #"\=")))
             (string/split query #"\&"))))

(defn to-query-params [coll]
  (string/join "&" (map #(string/join "=" (map (comp js/encodeURIComponent name) %)) coll)))

(defn ^:export authorize [connect-data]
  (let [data (assoc connect-data
               :state (str (rand-int 1000000000000)))
        query-data (to-query-params data)]
    (set! (.-href js/location)
          (str "https://github.com/login/oauth/authorize?" query-data))))

(defn get-token-for-code [connect-data code callback]
  (log "got here ")
  (log (prn-str connect-data))
  (.ajax js/jQuery
         (clj->js
          {:url (str (:login_server connect-data) "/login/oauth/access_token")
           :type "POST"
           :data {:code code}
           :success (fn [x] (callback (js->clj x :keywordize-keys true)))})))

(defn store-access-token! [token]
  (.setItem js/sessionStorage "githubber-access-token" token))

(defn get-access-token []
  (.getItem js/sessionStorage "githubber-access-token"))

(defn get-location []
  (.-href js/location ))

;; This authorization should only be on a separate loging page to
;; minimize asset loading.  Although caching should handle it pretty
;; well.

(defn get-authorized-access-token
  [connect-data callback]
  (let [url (parse-url (get-location))
        query (parse-query (:query url))
        access-token (get-access-token)]
    (if (and (:code query) (:state query))
      (get-token-for-code connect-data
                          (:code query)
                          (fn [res]
                            (log (prn-str res))
                            (if-let [token (:access_token res)]
                              (do
                                (store-access-token! token)
                                (set! (.-search js/location) ""))
                              (set! (.-search js/location) "failed_to_get_token=true"))))
      (if access-token
        (callback access-token)
        (authorize connect-data)))))

;; api interaction

(let [interp->keyword
      (fn [d x]
        (if (= \: (first x))
          ((keyword (string/join "" (rest x))) d)
          x))]
  (defn interpolate-path [path d]
    (string/join "/"
                 (map (partial interp->keyword d)
                      (string/split path #"/")))))

(defn request [c url & {:keys [url-data
                               verb
                               data
                               raw]}]
  (let [out (chan)]
       (.ajax js/jQuery
              (clj->js
               {:url (str "https://api.github.com"
                          (interpolate-path url (merge c url-data))
                          "?access_token=" (:access-token c)) 
                :type (or verb "GET")
                :headers { :Accept (str "application/vnd.github.v3"
                                        (if raw ".raw")
                                        "+json")}
                :dataType (if raw "text" "json")
                :data data
                :success (fn [x]
                           (put! out
                                 (if raw
                                   x
                                   (js->clj x :keywordize-keys true)))
                           (close! out)
                           )
                :error (fn [x] (put! out {:request-error x}) (close! out))
                }))
       out))

(defn stringify [d]
  (.stringify js/JSON (clj->js d)))

(defn base64-encode [text-data]
  (let [out (chan)
        reader (js/FileReader.)]
      (set! (.-onload reader)
            (fn [e]
              (let [enc (.. e -target -result)]
                (put! out (last (string/split enc #"base64,")))
                (close! out))))
      (.readAsDataURL reader (js/Blob. (array text-data)))
      out))

(defn repos [c]
  (request c "/user/repos"))

(defn authenticated-user [c]
  (request c "/user"))

(defn repo [c repo]
  (request c
           "/repos/:owner/:repo"
           :url-data { :repo repo }))

(defn branches [c repo]
  (request c
           "/repos/:owner/:repo/branches"
           :url-data { :repo repo }))

(defn branch-details [c repo name]
  (go
   (if-let [brnchs (<! (branches c repo))]
     (first (filter #(== (:name %) name) brnchs)))))

(defn create-repo [c name description & options]
  (request c 
           "/user/repos"
           { :verb "POST"
             :data (stringify
                   (assoc
                       (first options)
                     :name name
                     :description description))}))

(defn get-file
  ([c path & {:keys [raw]}]
     (request c
              "/repos/:owner/:repo/contents/:path"
              :url-data { :path path }
              :raw raw)))

; PUT /repos/:owner/:repo/contents/:path
(defn create-file [c path file-contents & {:keys [message branch] :as opts}]
  (go
   (<! (request c
                "/repos/:owner/:repo/contents/:path"
                :verb "PUT"
                :url-data { :path path }
                :data
                (stringify
                 {
                  :message (or message (str "creating file " path))
                  :branch  (or branch "master")
                  :content (<! (base64-encode file-contents))})))))

; PUT /repos/:owner/:repo/contents/:path
(defn update-file [c file file-contents & {:keys [message branch] :as opts}]
  (go
   (<! (request c
                "/repos/:owner/:repo/contents/:path"
                :verb "PUT"
                :url-data { :path (:path file) }
                :data
                (stringify
                 {
                  :sha (:sha file)
                  :message (or message (str "updating file " (:path file)))
                  :branch  (or branch "master")
                  :content (<! (base64-encode file-contents)) })))))

; DELETE /repos/:owner/:repo/contents/:path
(defn delete-file [c file & {:keys [message branch] :as opts}]
  (request c
           "/repos/:owner/:repo/contents/:path"
           :verb "DELETE"
           :url-data { :path file }
           :data
           (stringify
            {
             :sha (:sha file)
             :message (or message (str "deleting file " (:path file)))
             :branch  (or branch "master")})))

(defn get-ref [c refname]
  (request c
           "/repos/:owner/:repo/git/refs/:ref"
           :url-data { :ref (str "heads/" refname)}))

(defn create-ref [c refname sha]
  (request c
           "/repos/:owner/:repo/git/refs"
           :verb "POST"
           :data
           (stringify
            {:ref (str "refs/heads/" refname)
             :sha sha})))

(defn update-ref [c refname sha & {:keys [force]}] 
  (request c
           "/repos/:owner/:repo/git/refs/:ref"
           :url-data { :ref (str "heads/" refname)}
           :verb "PATCH"
           :data
           (stringify
            {:force (if force true false)
             :sha sha})))

(defn get-tree [c repo sha]
  (request c
           "/repos/:owner/:repo/git/trees/:sha"
           :url-data { :sha sha }
           :data { :recursive 1}))

(defn list-files [c repo branch-name]
  (go
   (let [tree-sha (get-in (<! (branch-details c repo branch-name))
                          [:commit :sha])]
     (<! (get-tree c repo tree-sha)))))


(comment
 (get-authorized-access-token (fn [t] (log t)))
  (def con { :access-token (get-access-token)
             :owner "bhauman"
             :repo "test-repo1"})
  (go 
   (log (clj->js (<! (list-files con (con :repo) "master")))))
  
  (log (clj->js con))
  
  (get-authorized-access-token (fn [t] (log t)))
  
  (defn test-data [t]
    (str "<html><body><h1>" t "</h1></body></html>"))
  
  (defn l [data]
    (log (clj->js data)))
  
  (go
   (log "hello")
 #_(l (<! (repo con "test-repo1")))
 #_(l (<! (authenticated-user con)))
 #_(l (<! (repos con)))  
 #_(l (<! (get-file con "index1.html" :raw true)))
 (l (<! (delete-file con
                     {:path "index3.html"} 
                     #_(test-data "some more testerly"))))
 
 #_(let [file (<! (get-file con "index1.html"))]
     (l (<! (delete-file con file))))
 
 #_(log (prn-str (<! (create-repo con "test-repo1" "This is a cool one" {:auto_init true}))))
 #_(log (<! (base64-encode "holycow this is some crazy stuff going on here I'm converting text to base 64")))
 
 #_(log (prn-str (<! (get-file con "index1.html")))) 
 )


#_a7b08edb388699bbb94b6b1be9f2b2d2dd753e71













(get-authorized-access-token (fn [res] (log (prn-str res))))



(enable-console-print!)

  )


