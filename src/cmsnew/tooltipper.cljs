(ns cmsnew.tooltipper
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [crate.core :as crate]
   [cmsnew.templates :as templ]
   [cmsnew.log-utils :refer [ld lp log-chan]]   
   [clojure.string :as string]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(def page-top 62)

(defn top-bottom-boundary [item]
  (let [item   ($ item)
        height (.height item)
        top    (.-top (.offset item))
        bottom (+ top height)]
    [top bottom]))

(defn element-boundaries [container-selector]
  (map top-bottom-boundary (.children ($ container-selector))))

(defn hover-boundaries [container-selector]
  (let [el-bounds (flatten (element-boundaries container-selector))
        h-bounds  (concat [(- (first el-bounds) 16)] el-bounds [(+ 100 (last el-bounds))])]
    (vec (map vector
              (keep-indexed #(if (even? %1) %2) h-bounds)
              (keep-indexed #(if (odd? %1) %2) h-bounds)))))

(defn hover-position [y-offset boundaries]
  (first (keep-indexed (fn [i [t b]] (if (< t y-offset b) i))
                       boundaries)))

(defn mouse-position-chan [selector]
  (let [out (chan (sliding-buffer 1))]
      (jq/bind ($ selector) :mousemove
        #(do
           (put! out {:x (.-pageX %) :y (.-pageY %)})))
      out))

(defn inset-tooltip? []
  (let [body-width (.width ($ "body"))
        page-width (.width ($ "[data-pagename]"))]
    (> 80 (- body-width page-width))))

(defn tooltip-positions [container-selector pos-chan]
  (let [out (chan)]
    (go
     (loop [current-position nil]
       (let [container ($ container-selector)
             length ]
         (when (< 0 (.-length container))
           (let [{:keys [x y]} (<! pos-chan)
                 boundaries (hover-boundaries container-selector)
                 position (hover-position y boundaries)
                 ]
             (let [tool-y-position (+ 5 (first (get boundaries position)))
                   tool-x-position (+ (.-left (.offset container)) (if (inset-tooltip?) 0 -20))]
               (when (not= current-position position)
                 (>! out (if (nil? position)
                           [:tooltip-hidden]
                           [:tooltip-position [tool-x-position tool-y-position position]]))))
             (<! (timeout 50)) ;; throttle this a bit
             (recur position))))
       (<! (timeout 50))
       (recur current-position)))
    out))

(defn tooltip-hide []
  (jq/remove-class ($ "#tooltipper")
                   "tooltipper-active"))

(defn tooltip-render [[msg [x y]]]
  (condp = msg
    :tooltip-position
    (do
      (jq/add-class ($ "#tooltipper") "tooltipper-active")
      (jq/css ($ "#tooltipper") {:top (str y "px") :left (str x "px")}))
    :tooltip-hidden (tooltip-hide)))

(defn tooltip-renderer [tool-pos-chan]
  (let [out (chan)]
    (go-loop []
             (let [msg (<! tool-pos-chan)]
               (tooltip-render msg))
             (recur))))

(defn render-tooltip-content []
  (crate/html (templ/tooltip-template)))

(defn add-popover-to-tooltip []
  (.popover ($ "#tooltipper") (clj->js { :html true :content render-tooltip-content :trigger "manual"})))

(defn popover-show []
  (.popover ($ "#tooltipper") "show"))

(defn popover-hide []
  (.popover ($ "#tooltipper") "hide"))

(defn tooltip-position-chan [container-selector]
  (->> (mouse-position-chan "body")
       (tooltip-positions container-selector)))

(defn tooltip-chain [container-selector]
  (add-popover-to-tooltip)
  (->> (mouse-position-chan "body")
       (tooltip-positions container-selector)
       (map< tooltip-render)
       (async/into [])))

