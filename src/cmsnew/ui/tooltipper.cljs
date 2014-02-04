(ns cmsnew.ui.tooltipper
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [crate.core :as crate]
   [sablono.core :as sab :include-macros true]
   [cmsnew.publisher.util.log-utils :refer [ld lp log-chan]]
   [cmsnew.publisher.util.async-utils :as async-util]
   [reactor.core :as rct]
   [clojure.string :as string]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]))

(defn top-bottom-boundary [item]
  (let [item   ($ item)
        height (.height item)
        hover-margin (min 25 (int (/ height 2.0)))
        top    (.-top (.offset item))
        bottom (+ top height)]
    [top bottom]))

(defn element-boundaries [container-selector]
  (map top-bottom-boundary (.children ($ container-selector))))

(defn hover-boundaries [container-selector]
  (let [el-bounds (flatten (element-boundaries container-selector))
        h-bounds  (concat [(- (first el-bounds) 30)] el-bounds [(+ 100 (last el-bounds))])]
    (vec (map vector
              (keep-indexed #(if (even? %1) %2) h-bounds)
              (keep-indexed #(if (odd? %1) %2) h-bounds)))))

(defn hover-position [y-offset boundaries]
  (first (keep-indexed (fn [i [t b]] (if (< t y-offset b) i))
                       boundaries)))

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

(def Tooltipper
  (.createClass
   js/React
   (js-obj
    "getInitialState"
    (fn [] #js{ :mousePositionChan (chan (sliding-buffer 1))
               :top 0
               :left 0
               :pos 0
               :hidden true
               :display-popover false})
    "componentDidMount"
    (fn []
      (this-as this
               (let [mouse-chan (rct/get-state-val this :mousePositionChan)]
                 (.bind ($ "body") "mousemove.tooltip"
                        #(do
                           (put! mouse-chan {:x (.-pageX %) :y (.-pageY %)})))
                 (->> mouse-chan
                      (async/filter< (fn [{:keys [y]}]
                                       (let [disp-pos (rct/get-state-val this :display-popover)]
                                         (if disp-pos
                                           (not (< (- disp-pos 70) y (+ disp-pos 70)))
                                           true))))
                      (tooltip-positions (rct/get-prop-val this :watching))
                      (map< (fn [[msg data]]
                              (condp = msg                                
                                :tooltip-hidden (.setState this #js{ :hidden true
                                                                     :display-popover false}) 
                                :tooltip-position (let [[x y pos] data]
                                                    (.setState this #js {:hidden false
                                                                         :pos pos
                                                                         :display-popover false
                                                                         :top y
                                                                         :left x})))
                              true))
                      (async-util/dev-null))
                 )))
    "componentWillUnmount"
    (fn []
      (this-as this
               (let [mouse-chan (rct/get-state-val this :mousePositionChan)]
                 (close! mouse-chan)
                 (.unbind ($ "body") "mousemove.tooltip"))))
    "render"
    (fn []
      (this-as this
               (sab/html [:div#tooltipper.tooltipper
                          { :style {:position "absolute"
                                    :opacity (if (rct/get-state-val this :hidden) 0.0 1.0)
                                    :top (str (rct/get-state-val this :top) "px")
                                    :left (str (rct/get-state-val this :left) "px")}
                           :onClick (fn []
                                      (.setState
                                       this
                                       #js{ :display-popover
                                           (if (rct/get-state-val this :display-popover)
                                             false
                                             (rct/get-state-val this :top))})
                                      (when-let [callback (rct/get-prop-val this :onPositionSelect)]
                                        (callback (rct/get-state-val this :pos)))
                                      )} "+"
                          (let [children (rct/get-children this)]
                            (if (and children
                                     (rct/get-state-val this :display-popover))
                              [:div.popover.fade.right.in {:style { :top "-24px" :left "24px" :display "block"}}
                               [:div.arrow]
                               [:div.popover-content children]])
                            )]))))))

