(ns cmsnew.core
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [crate.core :as crate]
   [cmsnew.datastore.s3 :as store]
   [cmsnew.heckle :as heckle]
   [cmsnew.templates :as templ]   
   [cljs-uuid-utils :refer [make-random-uuid uuid-string]]   
   [clojure.string :as string]
   [cljs.reader :refer [push-back-reader read-string]]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

;; logging utils
(defn ld [arg]
  "log data"
  (log (clj->js arg)))

(defn lp [arg]
  "print edn data"
  (log (prn-str arg)))

(defn log-chan [in] (map< #(do (ld %) %) in))

;; DOM helpers

(defn enclosing-form [element]
  (-> element $ (jq/parents "form") first $))

(defn serialize-form [form]
  (let [data (js->clj (.serializeArray form) :keywordize-keys true)]
    (reduce (fn [res {:keys [name value]}]
              (assoc res (keyword name) value))
            {} data)))

;; event channels

(defn click-chan [selector] 
  (let [out (chan)]
    (jq/on ($ "body") "click" selector
        {} (fn [e]
             (put! out [:click e])))
    out))

(defn form-submit-chan []
  (let [out (chan)]
    (jq/on ($ "#cmsnew") "click" "input[type=submit]" {}
        #(do
           (jq/prevent %)
           (.stopPropagation %)
           (put! out [:form-submit (serialize-form (enclosing-form (.-currentTarget %)))])))
    out))

(defn form-cancel-chan []
  (let [out (chan)]
    (jq/on ($ "#cmsnew") "click" "input[type=reset]" {}
        #(do
           (jq/prevent %)
           (.stopPropagation %)
           (put! out [:form-cancel %])))
    out))

;; getting the position of an element, this is ridiculous
(defn click-event-to-position-event [container-selector item-selector [msg event]]
  (let [target (.-currentTarget event)
        indexed-children (-> target
                             $
                             (jq/parents container-selector)
                             first
                             $
                             jq/children
                             (.map (fn [i,x] [i,x])))
        found-item-position (first (filter #(= target (last %)) indexed-children))]
    [:position-event { :position (first found-item-position)}]))

(defn position-to-data-item [items [_ {:keys [position] :as data}]]
  [:data-item (get (vec items) position)])

(defn position-event-chan [container-selector item-selector]
  (->> (click-chan (str container-selector " " item-selector))
       (map< (partial click-event-to-position-event container-selector item-selector))))

(defn render-data-page [page]
  (crate/html (templ/item-list "list-1" "list-1" (map templ/render-item (get-in page [:front-matter :items])))))

(defn edit-item [start-item-data input-chan]
  (go
   (jq/html ($ "#main-area") (crate/html (templ/item-form start-item-data {})))
   (loop [[msg new-data] (<! input-chan)
          item-data start-item-data]
     (ld new-data)
     (condp = msg
       :form-cancel false
       :form-submit (merge start-item-data new-data)
       (recur (<! input-chan) item-data)))))

(defn add-id? [{:keys [id] :as item}]
  (if id item (assoc item :id (uuid-string (make-random-uuid)))))

(defn merge-data-item-into-page [page data-item]
  (assoc-in page [:front-matter :items]
         (map (fn [x] (if (= (:id x) (:id data-item))
                       data-item x))
              (get-in page [:front-matter :items]))))
;; page helpers

(defn get-page-items [page]
  (get-in page [:front-matter :items]))




;; new flow
;; receive edit page event [:edit-page page-id]
;; emit [:render edit-page]

;; edit page item event [:edit-page-item item-id]
;; emit [:render-edit-page-item page-item-id]

;; submit page item event [:submit-page-item new-page-item-data]

(comment

  (defn edit-page [[msg data env]]
  (if (= msg :edit-page)
    [:render {:name :edit-page :page-path data} env]
    [msg data env]))

(defn edit-page-item [[msg data env]]
  (if (= msg :edit-page-item)
    [:render {:name :edit-page-item :data data} env]
    [msg data env]))

(defn find-with-key-like [items key value]
  (first (drop-while #(not= (key %) value) items)))

(defn render-edit-page-item [[msg data env]]
  "takes a [:render {:name :render-page-item :data {:position 5 :page-path some-path}}]"
  (if (and (= msg :render)
           (= :edit-page-item (:name data)))
    (let [page      (find-with-key-like (:pages env) :path (:page-path (:data data)))
          page-item (get (vec (get-page-items page)) (:position data))]
      (jq/html ($ "#main-area") (crate/html (templ/item-form page-item {})))      
      [msg data env]
      )
    [msg data env]
    )
  )

(defn render-edit-page [[msg data env]]
  (if (and (= msg :render)
           (= :edit-page (:name data)))
    (let [page (find-with-key-like (:pages env) :path (:page-path data))]
      (-> ($ "#cmsnew")
          (jq/html (crate/html (templ/edit-page (:front-matter page)))))
      (-> ($ "#main-area")
          (jq/html (render-data-page page)))
      [msg data env])
    [msg data env]))


(defn dataflow [in]
  (let [out (chan)]
    (->> in
         log-chan
         (map< edit-page)
         (map< edit-page-item)         
         log-chan
         (map< render-edit-page)
         (map< render-edit-page-item)         
         (async/into []))
    out))
  )

#_(go
 (let [pages (<! (heckle/get-pages heckle/system))
       start-edn-page (first (filter heckle/edn-page? pages))
       input-chan (chan)
       dflow-out (dataflow input-chan)
       env {:pages pages}]
   (>! input-chan [:edit-page (:path start-edn-page) env])
   (<! (async/timeout 1000))
   (log "here we are")
   (>! input-chan [:edit-page-item {:page-path (:path start-edn-page)
                                    :position 0}
                   env])))


(go
 (let [pages (<! (heckle/get-pages heckle/system))
       orig-edn-page (first (filter heckle/edn-page? pages))
       page-items (map add-id? (get-in orig-edn-page [:front-matter :items]))
       start-edn-page (assoc-in orig-edn-page [:front-matter :items] page-items) 
       edit-chan (position-event-chan ".edit-items-list" ".item")
       submit-chan (form-submit-chan)
       cancel-chan (form-cancel-chan)
       all-chans   (async/merge [edit-chan submit-chan cancel-chan])]
   (-> ($ "#cmsnew")
       (jq/append (crate/html (templ/edit-page (:front-matter start-edn-page)))))
   (loop [edn-page start-edn-page]
     (-> ($ "#main-area")
         (jq/html (render-data-page edn-page)))
     (let [[msg data] (<! all-chans)]
       (if (= msg :position-event)
         (let [[_ item-data] (position-to-data-item (get-page-items edn-page) [msg data])    
               new-data-item (<! (edit-item item-data all-chans))]
           (if new-data-item
             (let [new-page (merge-data-item-into-page edn-page new-data-item)]
               (ld new-page)
               (heckle/store-source-file heckle/system new-page)
               (recur new-page))
             (recur edn-page)))
         (recur edn-page))))))

#_(go
 (log (clj->js (<! (<! (heckle/process heckle/system))))))


