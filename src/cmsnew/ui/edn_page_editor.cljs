(ns cmsnew.ui.edn-page-editor
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [sablono.core :as sab :include-macros true]   
   [cmsnew.publisher.datastore.s3 :as store]
   [cmsnew.publisher.core :as pub]
   [cmsnew.ui.templates :as templ]
   [cmsnew.publisher.util.log-utils :refer [ld lp log-chan]]
   [cmsnew.publisher.util.async-utils :as async-util]

   ;; importing edn-items
   [cmsnew.edn-page.item :refer [deleted?]]
   [cmsnew.edn-page.items.heading]
   [cmsnew.edn-page.items.markdown]
   [cmsnew.edn-page.items.section]
   [cmsnew.edn-page.items.image]   
   
   [reactor.core :refer [react-render-loop]]
   [cljs-uuid-utils :refer [make-random-uuid uuid-string]]
   [clojure.string :as string]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))


;; page helpers

(def items-key [:front-matter :items])

(defn get-page-items [page]
  (get-in page items-key))

(defn empty-page? [page]
  (zero? (count (get-page-items page))))

(defn random-uuid []
  (uuid-string (make-random-uuid)))

(defn add-id? [{:keys [id] :as item}]
  (if id item (assoc item :id (uuid-string (make-random-uuid)))))

(defn new-item [type]
  (add-id? {:type type}))

(defn blank? [st]
  (or (nil? st)
      (not (.test #"\S" st))))


(defn merge-data-item-into-page [page data-item]
  (let [items (get-page-items page)]
    (assoc-in page items-key
              (if (deleted? data-item)
                (remove #(= (:id %) (:id data-item)) items)
                (map (fn [x] (if (= (:id x) (:id data-item))
                              data-item x))
                     items)))))

(defn insert-data-item-into-page [page position data-item]
  (let [items (get-page-items page)]
    (if (deleted? data-item)
      page
      (assoc-in page [:front-matter :items]
                (vec (concat (take position items)
                             [data-item]
                             (drop position items)))))))

(defn merge-front-matter-into-page [page new-front-matter]
  (update-in page [:front-matter] merge (dissoc new-front-matter :errors)))

(defn initial-item-to-empty-page [page]
  (if (empty-page? page)
    (assoc-in page items-key
              [(add-id? {:type :heading :size 2 :content "Edit this heading"})])
    page))

;; interaction controllers

(defn upload-image-file [site uuid file]
  (let [out (chan)]
    (store/upload-image-file (:s3-store site)
                             uuid file
                             (fn [file url]
                               (put! out [:success {:uuid uuid :url url :file file}])
                               (close! out))
                             (fn [file url]
                               (put! out [:failed  {:uuid uuid :url url :file file}])
                               (close! out))
                             (fn [percent]
                               (put! out [:progress percent])))
    out))

(defn new-image-item [uuid url file]
  {:id uuid
   :type :image
   :url  url
   :name (.-name file)
   :mime-type (.-type file)})

(defn handle-add-image [state data position]
  (log data)
  (let [site (:site @state)
        edn-page (:edn-page @state)
        file (aget (.-files (.-target data)) 0)
        file-upload-uuid (random-uuid)
        upload-chan (upload-image-file site file-upload-uuid file)]
    (log data)
    (log file)
    (go-loop []
             (let [[msg _data] (<! upload-chan)]
               (condp = msg
                 :failed edn-page
                 :success
                 (let [new-page (insert-data-item-into-page edn-page position
                                                            (new-image-item file-upload-uuid (:url _data) file))]
                   (pub/store-source-file site new-page)
                   (go (<! (timeout 1000))
                       (pub/publish site))                   
                   new-page)
                 :progress (do (log _data) (recur))
                 (recur))))))

;; right now this never exits as there is no exit event

(defn edit-item-new [state start-item-data]
  (go
   (swap! state assoc :editing-item start-item-data)
   (log (prn-str start-item-data))
   (loop [[msg new-data] (<! (:event-chan @state))
          item-data start-item-data]
     (ld [:yep msg new-data])
     (condp = msg
       :form-cancel false
       :change-edited-item (let [new-item (merge item-data new-data)]
                             (swap! state assoc :editing-item new-item)
                             (recur (<! (:event-chan @state)) new-item))
       :form-submit (merge item-data new-data)
       :form-delete (merge item-data {:deleted true}) 
       (recur (<! (:event-chan @state)) item-data)))))

(defn handle-edit-page-item-new [state id]
  (go
   (let [item-data (first (filter #(= (:id %) id) (get-page-items (:edn-page @state))))
         new-data-item (<! (edit-item-new state item-data))]
     (if new-data-item
       (let [new-page (merge-data-item-into-page (:edn-page @state) new-data-item)]
         (pub/store-source-file (:site @state) new-page)
         (go (<! (timeout 1000))
             (pub/publish (:site @state)))
         new-page)
       (:edn-page @state)))))

(defn handle-add-item-new [state start-item position]
  (go
   (let [item-data (assoc start-item :insert-position position)
         new-data-item (<! (edit-item-new state item-data))]
     (if new-data-item
       (let [fixed-data-item (dissoc new-data-item :insert-position)
             new-page (insert-data-item-into-page (:edn-page @state) position fixed-data-item)]
         (pub/store-source-file (:site @state) new-page)
         (go (<! (timeout 1000))
             (pub/publish (:site @state)))
         new-page)
       (:edn-page @state)))))

(defn handle-adding-item [page-state item-type position]
  (condp = item-type
    :heading
    (handle-add-item-new page-state
                             (add-id? {:type :heading :size 2}) position)
    :markdown
    (handle-add-item-new page-state
                         (new-item :markdown) position)
    nil (go (:edn-page page-state))
    (if (keyword? item-type) ; handle all types genericly
      (handle-add-item-new page-state
                           (new-item item-type) position)
      (go (:edn-page page-state)))))

(defn add-error [subject key msg]
  (update-in subject [:errors key]
             conj msg))

(defn cant-be-blank [key subject]
  (let [value (key subject)]
    (if (or (nil? value)
            (string/blank? value))
      (add-error subject key "Can't be blank")
      subject)))

(defn validate-front-matter [front-matter]
  (->> front-matter
       (cant-be-blank :title)
       (cant-be-blank :layout)))

(defn valid? [data]
  (zero? (count (:errors data))))

(defn handle-edit-settings [page-state]
  (let [{:keys [edn-page event-chan]} @page-state]
    (swap! page-state assoc :editing-front-matter (get-in @page-state [:edn-page :front-matter]))
    (go-loop []
             (let [[msg data] (<! event-chan)]
               (log (prn-str [msg data]))
               (condp = msg
                 :form-submit
                 (let [validated (validate-front-matter data)]
                   (if (valid? validated)
                     (let [new-page (merge-front-matter-into-page edn-page validated)]
                       (pub/store-source-file (:site @page-state) new-page)
                       (go (<! (timeout 1000))
                           (pub/publish (:site @page-state)))
                       new-page)
                     (recur)))
                 :form-cancel edn-page
                 nil edn-page
                 (recur))))))

(defn edit-edn-page-loop-new [page-state]
  (let [start-edn-page (initial-item-to-empty-page (:edn-page @page-state))]
    (swap! page-state assoc :edn-page start-edn-page)
    (go-loop [edn-page start-edn-page
              insert-position 0]
             (let [[msg data] (<! (:event-chan @page-state))]
               (log (prn-str [msg data]))
               (log insert-position)
               (condp = msg
                 :edit-settings
                 (let [new-page (<! (handle-edit-settings page-state))]
                   (swap! page-state assoc :edn-page new-page :editing-front-matter false)
                   (recur new-page insert-position))
                 :edit-item
                 (let [res-page (<! (handle-edit-page-item-new page-state (:id data)))
                       new-page (initial-item-to-empty-page res-page)]
                   (swap! page-state assoc :edn-page new-page :editing-item false)
                   (recur new-page insert-position))
                 :add-item
                 (let [new-page (<! (handle-adding-item page-state (:type data) insert-position))]
                   (swap! page-state assoc :edn-page new-page :editing-item false)
                   (recur new-page insert-position))
                 :image-selected
                 (let [new-page (<! (handle-add-image page-state data insert-position))]
                   (swap! page-state assoc :edn-page new-page)
                   (recur new-page insert-position))                 
                 :insert-position (recur edn-page data)
                 nil true
                 (recur edn-page insert-position))))))

(defn edit-page [site start-edn-page]
  (let [event-chan (chan)
        close-chan (chan)
        page-state (atom { :site site
                           :event-chan event-chan
                           :close-chan close-chan})
        target-node (.getElementById js/document "cmsnew")
        state-change-chan (async-util/atom-chan page-state)
        ]
    (->> state-change-chan
         (map< #(templ/edit-page (last %)))
         (react-render-loop target-node))
    (swap! page-state assoc :edn-page start-edn-page ) ;; initial render
    (go
     (let [[val ch] (alts! [close-chan (edit-edn-page-loop-new page-state)])]
           (close! event-chan)
           (close! state-change-chan)
           (close! close-chan)
           true))))
