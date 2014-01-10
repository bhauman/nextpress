(ns cmsnew.edn-page-editor
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [crate.core :as crate]
   [sablono.core :as sab :include-macros true]   
   [cmsnew.authorization.persona :as session]
   [cmsnew.datastore.s3 :as store]
   [cmsnew.heckle :as heckle]
   [cmsnew.templates :as templ]
   [cmsnew.tooltipper :as tip]
   [cmsnew.log-utils :refer [ld lp log-chan]]
   [cmsnew.async-utils :as async-util]
   [cljs-uuid-utils :refer [make-random-uuid uuid-string]]
   [clojure.string :as string]
   [cljs.reader :refer [push-back-reader read-string]]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(defn render-to [react-dom html-node callback]
  (.renderComponent js/React react-dom html-node callback))

(defn react-render [html-node react-dom]
  "A blocking render call"
  (let [out (chan)]
    (render-to react-dom html-node (fn [] (put! out :rendered) (close! out)))
    out))

(defn react-render-loop [html-node react-dom-chan]
  (go-loop []
           
           (let [react-dom (<! react-dom-chan)]
             (log "rendering")
             (if (nil? react-dom)
               :finished
               (do (<! (react-render html-node react-dom))
                   (recur))))))

;; DOM helpers

(defn enclosing-form [element]
  (-> element $ (jq/parents "form") first $))

(defn serialize-form [form]
  (let [data (js->clj (.serializeArray form) :keywordize-keys true)]
    (reduce (fn [res {:keys [name value]}]
              (assoc res (keyword name) value))
            {} data)))

;; event channels

(defn touch-click-event [selector callback]
  (let [tchan (chan)]
    (jq/on ($ "body") "touchstart" selector (fn [e] (put! tchan [:touchstart e])))
    (jq/on ($ "body") "touchend"   selector (fn [e] (put! tchan [:touchend e])))
    (jq/on ($ "body") "touchmove"   selector (fn [e] (put! tchan [:touchmove e])))    
    (jq/on ($ "body") "click"      selector (fn [e] (put! tchan [:click e])))        
    (go-loop []
             (let [[msg d] (<! tchan)]
               (log (prn-str msg))
               (condp = msg
                 :click (callback d)
                 :touchstart (let [[value ch] (alts! [tchan (timeout 500)])]
                               (when (and (= ch tchan)
                                          (or (= (first value) :touchend)
                                              (= (first value) :click)))
                                 (do (log "touch-cluck")
                                     (callback (last value))
                                     ;; if click happens in next 400ms
                                     ;; ignore
                                     (when (not= (first value) :click)
                                       (let [t (timeout 400)]
                                         (loop []
                                           (let [[value ch] (alts! [tchan t])]
                                             (when (not= ch t)
                                               (recur)))))))))
                 false)
               (recur)))))

(defn click-chan [selector ev-name]
  (let [out (chan)]
    (touch-click-event selector
                       (fn [e]
                         (jq/prevent e)
                         (put! out [ev-name e])))
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

(defn add-image-chan []
  (let [out (chan)]
    (jq/on ($ "body") "click" ".add-image-item" {}
           (fn [_] (.click ($ "input.image-upload"))))
    (jq/on ($ "body") "change" "input.image-upload" {}
           (fn [e] (put! out [:image-selected e])))
    out))

;; behaviors

(defn heading-form-behavior []
  (let [hclicks (->> (click-chan ".heading-size-btn" :doh)
                     (map< (fn [[_ e]]
                             (let [target (.-target e)
                                   size   (.-size (.data ($ target)))
                                   form   (enclosing-form target)]
                               [form size]))))]
    (go-loop []
             (let [[form size] (<! hclicks)]
               
               (jq/remove-class ($ ".heading-size-btn" form) "active")
               (jq/val ($ "[name=size]" form) size)
               (jq/attr ($ ".heading-input" form) "data-size" size)
               (recur)))
    ))

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
  (->> (click-chan (str container-selector " " item-selector) :edit-item-click)
       (map< (partial click-event-to-position-event container-selector item-selector))))

(defn render-data-page [{:keys [edn-page]}]
  (crate/html (templ/item-list "list-1" "list-1" (map templ/render-editable-item
                                                      (get-in edn-page [:front-matter :items])))))

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

(defmulti deleted? #(:type %))

(defmethod deleted? :default [{:keys [deleted]}]
  deleted)

(defmethod deleted? :heading [{:keys [content deleted]}]
  (or deleted (blank? content)))

(defmethod deleted? :markdown [{:keys [content deleted]}]
  (or deleted (blank? content)))

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

(defn initial-item-to-empty-page [page]
  (if (empty-page? page)
    (assoc-in page items-key
              [(add-id? {:type :heading :size 2 :content "Edit this heading"})])
    page))

;; interaction controllers

(defn edit-item [start-item-data input-chan]
  (go
   (-> ($ (str "#" (:id start-item-data)))
       (jq/html (crate/html (templ/item-form start-item-data {})))
       (jq/remove-class "item"))
   (loop [[msg new-data] (<! input-chan)
          item-data start-item-data]
     (condp = msg
       :form-cancel false
       :form-submit (merge start-item-data new-data)
       :form-delete (merge start-item-data {:deleted true}) 
       (recur (<! input-chan) item-data)))))

(defn add-item [position start-item-data input-chan]
  (-> ($ "[data-pagename]")
      .children
      (aget position)
      $
      (.prepend (crate/html [:div {:id (:id start-item-data) :data-pageitem "placeholder"} "Placeholder for form"])))
  (edit-item start-item-data input-chan))

(defn handle-edit-page-item [heckle-site msg edn-page input-chan]
  (go
   (let [[_ item-data] (position-to-data-item (get-page-items edn-page) msg)    
         new-data-item (<! (edit-item item-data input-chan))]
     (if new-data-item
       (let [new-page (merge-data-item-into-page edn-page new-data-item)]
         (heckle/store-source-file heckle-site new-page)
         (go (<! (timeout 1000))
             (heckle/publish heckle-site))
         new-page)
       edn-page))))

(defn render-page [edn-page]
  (-> ($ "#main-area")
      (jq/html (render-data-page edn-page))))

(defn handle-add-item [heckle-site start-item position edn-page input-chan]
  (go
   (let [item-data start-item
         new-data-item (<! (add-item position item-data input-chan))]
     (if new-data-item
       (let [new-page (insert-data-item-into-page edn-page position new-data-item)]
         (heckle/store-source-file heckle-site new-page)
         (go (<! (timeout 1000))
             (heckle/publish heckle-site))
         new-page)
       edn-page))))

(defn upload-image-file [heckle-site uuid file]
  (let [out (chan)]
    (store/upload-image-file (:s3-store heckle-site)
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

(defn handle-add-image [heckle-site data position edn-page input-chan]
  (let [file (aget (.-files (.-target data)) 0)
        file-upload-uuid (random-uuid)
        upload-chan (upload-image-file heckle-site file-upload-uuid file)]
    (go-loop []
             (let [[msg _data] (<! upload-chan)]
               (condp = msg
                 :failed edn-page
                 :success
                 (let [new-page (insert-data-item-into-page edn-page position
                                                            (new-image-item file-upload-uuid (:url _data) file))]
                   (heckle/store-source-file heckle-site new-page)
                   (go (<! (timeout 1000))
                       (heckle/publish heckle-site))                   
                   new-page)
                 :progress (do (log _data) (recur))
                 (recur))))))

(defn tooltip-popover-loop [heckle-site edn-page position input-chan]
  (tip/popover-show)
  (go-loop []
           (let [[msg data] (<! input-chan)]
             (ld [msg data])
             (condp = msg
               :add-heading-item
               (do
                 (tip/popover-hide)
                 (tip/tooltip-hide)
                 (<! (handle-add-item heckle-site
                                      (add-id? {:type :heading :size 2}) position edn-page input-chan)))
               :add-text-item
               (do
                 (tip/popover-hide)
                 (tip/tooltip-hide)
                 (<! (handle-add-item heckle-site
                                      (new-item :markdown) position edn-page input-chan)))
               :image-selected
               (do
                 (tip/popover-hide)
                 (tip/tooltip-hide)
                 (<! (handle-add-image heckle-site
                                       data position edn-page input-chan))
                 )               
               :tooltip-click (do
                                (tip/popover-hide)
                                (log (prn-str [msg])) edn-page) 
               (recur)
               ))
           ))

;; right now this never exits as there is no exit event
(defn edit-edn-page-loop [heckle-site start-edn-page input-chan]
  (let [start-edn-page (initial-item-to-empty-page start-edn-page)]
    (render-page start-edn-page)
    (heading-form-behavior)
    (go-loop [edn-page start-edn-page
              tool-tip-pos 0]
             (let [[msg data] (<! input-chan)]
               (log (prn-str [msg data]))
               (condp = msg
                 :position-event
                 (let [res-page (<! (handle-edit-page-item heckle-site [msg data] edn-page input-chan))
                       new-page (initial-item-to-empty-page res-page)]
                   (render-page new-page)
                   (recur new-page tool-tip-pos))
                 :tooltip-position (do (tip/tooltip-render [msg data]) (recur edn-page (last data)))
                 :tooltip-hidden   (do (tip/tooltip-render [msg]) (recur edn-page tool-tip-pos))
                 :tooltip-click (let [res-page (<! (tooltip-popover-loop heckle-site edn-page tool-tip-pos input-chan))
                                      new-page (initial-item-to-empty-page res-page)]
                                  (log "getting here")
                                  (ld new-page)
                                  (render-page new-page)
                                  (recur new-page tool-tip-pos))
                 (recur edn-page tool-tip-pos))))))

(defn edit-item-new [state start-item-data]
  (go
   (swap! state assoc :editing-item start-item-data)
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
         (heckle/store-source-file (:heckle-site @state) new-page)
         (go (<! (timeout 1000))
             (heckle/publish (:heckle-site @state)))
         new-page)
       (:edn-page @state)))))

(defn edit-edn-page-loop-new [page-state]
  (let [start-edn-page (initial-item-to-empty-page (:edn-page @page-state))]
    (swap! page-state assoc :edn-page start-edn-page)
    (go-loop [edn-page start-edn-page]
             (let [[msg data] (<! (:event-chan @page-state))]
               (log (prn-str [msg data]))
               (condp = msg
                 :edit-item
                 (let [res-page (<! (handle-edit-page-item-new page-state (:id data)))
                       new-page (initial-item-to-empty-page res-page)]
                   (swap! page-state assoc :edn-page new-page :editing-item false)
                   (recur new-page))
                 (recur edn-page))))))

(defn- hookup-editing-messages []
  (async/merge [(position-event-chan ".edit-items-list" ".item")
          (click-chan ".add-heading-item" :add-heading-item)
          (click-chan ".add-text-item" :add-text-item)
          (add-image-chan)
          (form-submit-chan)
          (form-cancel-chan)
          (click-chan "button.form-delete" :form-delete)
          (click-chan "#tooltipper" :tooltip-click)
          (tip/tooltip-position-chan ".edit-items-list")
          ]))

(defn edit-page [heckle-site start-edn-page]
  (let [event-chan (chan)
        page-state (atom { :heckle-site heckle-site
                           :event-chan event-chan})
        target-node (.getElementById js/document "cmsnew")
        state-change-chan (async-util/atom-chan page-state)
        ]
    
    ;; initial render
    ;; (react-render target-node (templ/edit-page @page-state))
    ;; reactive rendering
    (->> state-change-chan
         (map< #(templ/edit-page (last %)))
         (react-render-loop target-node))
    
    (swap! page-state assoc :edn-page start-edn-page ) ;; initial render

    
    (go
       (<! (edit-edn-page-loop-new page-state)))

    )
  
  
  

  
    #_(tip/add-popover-to-tooltip)
    #_(go
       (<! (edit-edn-page-loop heckle-site start-edn-page all-chans))))


