(ns cmsnew.ui.edn-page-editor
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [sablono.core :as sab :include-macros true]   

   [cmsnew.publisher.datastore.s3 :as store]

   [cmsnew.publisher.datastore.core :refer [store-source
                                            fetch-file]]
   
   [cmsnew.publisher.datastore.localstore :refer [LocalStore]]
   [cmsnew.publisher.datastore.s3-store :refer [S3tore]]

   [cmsnew.publisher.plugins.core :refer [plugin<]]
   
   [cmsnew.publisher.source-file :as sf]
   [cmsnew.publisher.paths :as paths]   
   [cmsnew.publishing-pipeline :as pub]
   [cmsnew.ui.templates :as templ]
   
   [cmsnew.publisher.util.core :refer [find-first]]
   [cmsnew.publisher.util.log-utils :refer [ld lp log-chan]]
   [cmsnew.publisher.util.async-utils :as async-util :refer [pipe-without-close]]
   
   ;; importing edn-items
   [cmsnew.edn-page.item :refer [deleted? add-id? new-item random-uuid]]
   [cmsnew.edn-page.items.heading]
   [cmsnew.edn-page.items.markdown]
   [cmsnew.edn-page.items.section]
   [cmsnew.edn-page.items.image]

   ;; next release of Core.async
   #_[cljs.core.async.impl.protocols :refer [Channel closed?]]
   #_[cljs.core.async.impl.channels]
   

   [reactor.core :refer [react-render-loop]]
   [clojure.string :as string]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go alt! go-loop]]
   [cmsnew.publisher.util.macros :refer [chan->>]]))


(defn effect [x]
  (with-meta x {:effect true}))

;; page helpers

(def items-key [:front-matter :items])

(defn get-page-items [page]
  (get-in page items-key))

(defn empty-page? [page]
  (zero? (count (get-page-items page))))

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

(defn store-source-file-and-update [site source-file]
  (store-source (:store site) source-file)
  #_(go (<! (timeout 1000))
        (pub/publish site)))

;; interaction controllers

(defmulti upload-file #(type %))

(defmethod upload-file S3tore [dstore uuid file]
  (let [out (chan)]
    (store/upload-image-file dstore
                             (str "uploaded_images/" uuid "-" (.-name file)) 
                             file
                             (fn [file url]
                               (put! out [:upload-success {:uuid uuid :url url :file file}])
                               (close! out))
                             (fn [file url]
                               (put! out [:upload-failed  {:uuid uuid :url url :file file}])
                               (close! out))
                             (fn [percent]
                               (put! out [:upload-progress {:percent percent :uuid uuid :file file}])))
    out))

;; for offline development purposes
(defmethod upload-file LocalStore [dstore uuid file]
  (let [out (chan)]
    (let [reader (js/FileReader.)]
      (set! (.-onload reader)
            (fn [e]
              (put! out [:upload-success
                         {:uuid uuid
                          :url (.. e -target -result)
                          :file file}])
              (close! out)))
      (.readAsDataURL reader file))
    out))

(defn new-image-item [uuid url file]
  {:id uuid
   :type :image
   :url  url
   :name (.-name file)
   :mime-type (.-type file)})

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

;; helpers

;; transforms

;; editing items
(defn edit-item [ps {:keys [id]}]
  (when-let [item (find-first #(= (:id %) id)
                            (get-in ps [:edn-page :front-matter :items]))]
    (-> ps
        (assoc :editing-item item))))
 
(defn edit-item-form-cancel [ps data]
  (dissoc ps :editing-item))

;; helper
(defn- splice-item-into-page
  "this will add a new item or merge and existing item into a page"
  [page item]
  ;; should probably check to see if item exists in page first
  (if-let [position (:insert-position item)]
    (insert-data-item-into-page page position (dissoc item :insert-position))
    (merge-data-item-into-page page item)))

(defn edit-item-form-submit [ps data]
  (let [item (merge (:editing-item ps) data)
        new-page (splice-item-into-page (:edn-page ps) item)]
    (-> ps
        (assoc
            :effects [[:store-page {:edn-page new-page}]]
            :edn-page new-page)
        (dissoc :editing-item))))

(defn edit-item-form-delete [ps _]
  (edit-item-form-submit ps {:deleted true}))

(defn change-edited-item [ps data]
  (assoc ps :editing-item
         (merge (:editing-item ps) data)))

;; adding items

(defn insert-position [ps position]
  (when position
    (assoc ps :insert-position position)))

; this hijacks the editing item process
(defn add-item [ps data]
  (let [item (new-item (:type data))
        item (assoc item :insert-position (:insert-position ps))]
    (assoc ps :editing-item item)))

;; editing settings

(defn edit-settings [ps _]
  (assoc ps :editing-front-matter
    (get-in ps [:edn-page :front-matter])))

(defn edit-settings-form-cancel [ps _]
  (dissoc ps :editing-front-matter))

(defn edit-settings-form-submit [ps data]
  (let [validated (validate-front-matter data)]
    (if (valid? validated)
      (let [new-page (merge-front-matter-into-page (:edn-page ps) validated)]
        (-> ps
            (assoc
                :effects [[:store-page {:edn-page new-page}]]
                :edn-page new-page)
            (dissoc :editing-front-matter))))))

;; adding an image item

(defn image-selected [ps data]
  (let [position (:insert-position ps)
        file (aget (.-files (.-target data)) 0)
        file-upload-uuid (random-uuid)]
    (-> ps
        (assoc :effects [[:upload-image-file! {:file-upload-uuid file-upload-uuid
                                               :file file}]]
               :file-uploading { :insert-position position
                                 :filename (.-name file )
                                 :uuid file-upload-uuid
                                 :file file
                                 :progress 0 }))))

(defn image-upload-failed [ps data]
  (dissoc ps :file-uploading))

(defn image-upload-progress [ps data]
  (when (:file-uploading ps)
    (assoc-in ps [:file-uploading :progress] (:percent data))))

(defn image-upload-success [ps data]
  (let [image-item (new-image-item (:uuid data) (:url data) (:file data))
        new-page (insert-data-item-into-page
                  (:edn-page ps)
                  (get-in ps [:file-uploading :insert-position])
                  image-item)]
    (-> ps
        (assoc
            :effects [[:store-page {:edn-page new-page}]]
            :edn-page new-page)
        (dissoc :file-uploading))))

;; effects

(def store-page
  (effect
   (fn [ps-atom data]
     (when-let [edn-page (:edn-page data)]
       (store-source-file-and-update (:site @ps-atom) edn-page)))))

(def upload-image-file!
  (effect
   (fn [ps-atom data]
     (let [ps @ps-atom
           upload-chan (upload-file (get-in ps [:site :store])
                                    (:file-upload-uuid data)
                                    (:file data))]
       (pipe-without-close (map<
                            (fn [[msg d]]
                              [(keyword (str "image." (name msg))) d])
                            upload-chan) 
                           (:event-chan ps))))))

;;; core of system

(defn create-event-table [& events]
  (into {} events))

(defn handle-events [page-state event-table]
  (let [nilsafe-f
        (fn [f s d] ((fnil f s) s d))
        pullout-ef
        (fn [effects-chan f s d]
          (let [res (nilsafe-f f s d)]
            (if-let [effects (:effects res)]
              (do
                ; next release of core.async
                #_(when-not (closed? effects-chan))
                (put! effects-chan effects)
                (close! effects-chan)
                (dissoc res :effects))
              (do
                (close! effects-chan)
                res))))]
    (go-loop []
             (let [[msg data] (<! (:event-chan @page-state))]
               (log (prn-str [msg data]))
               (when msg
                 (when-let [f (msg event-table)]
                   (if (:effect (meta f))
                     (f page-state data)
                     (let [effects-chan (chan)
                           pullout-effects (partial pullout-ef effects-chan f)]
                       (swap! page-state pullout-effects data)
                       (when-let [first-effects (<! effects-chan)]
                         (log (prn-str first-effects))
                         (doseq [ef first-effects]
                           (put! (:event-chan @page-state) ef))))))
                 (recur))))))

(defn get-edn-page-for-path [page-state path]
  (go
   (sf/parse-front-matter
    (:site page-state)
    (<! (fetch-file (get-in page-state [:site :store])
                    path)))))

;; display data altering

(defn add-layout-list [[_ n]]
  (if (:editing-front-matter n)
    (let [llist (map (juxt identity identity)
                     (map (comp paths/remove-extention :name)
                          (vals (get-in n [:site :layouts]))))]
      (assoc n :layout-list llist))
    n))

(defn edit-page [site start-edn-page]
  (let [event-chan (chan)
        close-chan (chan)
        page-state (atom { :site site
                           :event-chan event-chan
                           :close-chan close-chan})
        target-node (.getElementById js/document "cmsnew")
        state-change-chan (async-util/atom-chan page-state)]
    
    (chan->> state-change-chan
             (plugin< add-layout-list)             
             (map< #(templ/edit-page (last %)))
             (react-render-loop target-node))
    
    (go
     (let [edn-page (<! (get-edn-page-for-path @page-state (:path start-edn-page)))]
       (swap! page-state assoc :edn-page edn-page )
       (<! (handle-events
            page-state
            (create-event-table
             [:edit-settings              edit-settings]
             [:edit-settings.form-cancel  edit-settings-form-cancel]
             [:edit-settings.form-submit  edit-settings-form-submit]
             [:edit-item                  edit-item]
             [:edit-item.form-cancel      edit-item-form-cancel]
             [:edit-item.change-edited-item     change-edited-item]
             [:edit-item.form-submit      edit-item-form-submit]
             [:edit-item.form-delete      edit-item-form-delete]             
             [:insert-position            insert-position]
             [:add-item                   add-item]
             [:image-selected             image-selected]
             [:image.upload-success       image-upload-success]
             [:image.upload-failed        image-upload-failed]
             [:image.upload-progress      image-upload-progress]

             ;; effects
             [:store-page store-page]
             [:upload-image-file! upload-image-file!]
             )
            ))
       
       ;; initial render
       #_(let [[val ch] (alts! [close-chan (edit-edn-page-loop-new page-state)])]
           (close! event-chan)
           (close! state-change-chan)
           (close! close-chan)
           (:edn-page @page-state))))))

