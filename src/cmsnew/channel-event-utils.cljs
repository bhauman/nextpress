(ns cmsnew.channel-event-utils
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

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

(defn add-image-chan [out]
  (jq/on ($ "body") "click" ".add-image-item" {}
         (fn [_] (.click ($ "input.image-upload"))))
  (jq/on ($ "body") "change" "input.image-upload" {}
         (fn [e] (put! out [:image-selected e])))
  out)
;; behaviors
