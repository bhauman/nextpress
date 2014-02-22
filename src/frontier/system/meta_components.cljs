(ns frontier.system.meta-components
  (:require
   [cljs.core.async :as async
    :refer [put!]]
   [sablono.core :as sab :include-macros true]   
   [frontier.system.core :refer [iInputFilter
                                 iPluginInit
                                 iTransform
                                 iEffect
                                 iDerive
                                 -derive
                                 trans-helper*
                                 system
                                 add-effects
                                 component-group]]
   [jayq.util :refer [log]]))

(defn can-go-forward? [{:keys [history pointer]}]
  (< pointer (dec (count history))))

(defn can-go-back? [{:keys [pointer]}] (pos? pointer))

(defn current-state [{:keys [history pointer]}]
  (get history pointer))

(defmulti hist-trans identity)

(defmethod hist-trans :default [_ system data] system)

(defmethod hist-trans :collect [_ system data]
  (-> system
      (update-in [:pointer] (fn [p] (count (:history system))))
      (update-in [:history]
                 (fn [hist]
                   (conj (vec hist) data)))))

(defmethod hist-trans :back [_ {:keys [history pointer] :as sys} _]
  (if (can-go-back? sys)
    (-> sys
        (update-in [:pointer] dec)
        (assoc :render-state (get history (dec pointer))))
    sys))

(defmethod hist-trans :forward [_ {:keys [history pointer] :as sys} _]
  (if (can-go-forward? sys)
    (-> sys
        (update-in [:pointer] inc)
        (assoc :render-state (get history (inc pointer))))
    sys))

(defmethod hist-trans :keep [_ {:keys [history pointer] :as sys} _]
  (-> sys
      (add-effects [:set-state (get history pointer)])
      (update-in [:pointer] (dec (count history)))
      (dissoc :render-state)))

(defmethod hist-trans :cancel [_ {:keys [history pointer] :as sys} _]
  (-> sys
      (assoc :pointer (dec (count history)))
      (dissoc :render-state)))

(defn under-control [system]
  (if (:render-state system)
    (assoc system :under-control true)
    system))

(defn can-go-forward [state]
  (if (can-go-forward? state)
    (assoc state :can-go-forward true)
    state))

(defn can-go-back [state]
  (if (can-go-back? state)
    (assoc state :can-go-back true)
    state))

(defn add-msg [state]
  (assoc state :msg (:__msg (current-state state))))

(defrecord HistoryManager [managed-system-event-chan]
  iTransform
  (-transform [o [msg data] system]
    (hist-trans msg system data))
  iEffect
  (-effect [o [msg data] system event-chan effect-chan]
    (if (= msg :set-state)
      (put! managed-system-event-chan [:__system.set-state data])))
  iDerive
  (-derive [o system]
    (-> system
        under-control
        can-go-forward
        can-go-back
        add-msg)))

(defrecord SystemSetter []
  iTransform
  (-transform [o [msg data] system]
    (if (= msg :__system.set-state) data system)))

(defn managed-system [initial-state comp state-callback initial-inputs]
  (let [state (atom {})
        watch (add-watch state :renderer
                         (fn [_ _ _ {:keys [sys-state sys-chan hist-state hist-chan]}]
                           (state-callback sys-state sys-chan hist-state hist-chan)))
        sys-comp (component-group
                  (SystemSetter.)
                  comp)
        sys (system
             initial-state
             sys-comp
             (fn [s event-chan]
               (swap! state
                      assoc
                      :sys-state s
                      :sys-chan event-chan))
             initial-inputs)
        history (system {}
                        (component-group
                         (HistoryManager. (:event-chan sys)))
                        (fn [s event-chan]
                          (swap! state
                                 assoc
                                 :hist-state s
                                 :hist-chan event-chan)))]
    (add-watch (:state sys) :history-collect
               (fn [_ _ _ n]
                 (put! (:event-chan history) [:collect (-derive sys-comp n)])))
    (when initial-inputs
      (doseq [msg initial-inputs]
        (swap! (:state sys) (partial trans-helper* sys-comp identity) msg)))
    sys))

(defn render-history-controls [{:keys [under-control can-go-back can-go-forward msg] :as sys} hist-chan]
  (log (clj->js sys))
  (sab/html
   [:div.navbar.navbar-default
    [:ul.nav.navbar-nav
     (if can-go-back
       [:li
        [:a
         {:className ""
          :href "#"
          :onClick (fn [x]
                     (.preventDefault x)
                     (put! hist-chan [:back]))}
         [:span.glyphicon.glyphicon-step-backward]]]
       [:li])
     (if under-control
       [:li
        [:a
         {:className ""
          :onClick (fn [x]
                     (.preventDefault x)
                     (put! hist-chan [:cancel]))}
         "continue"]]
       [:li]) 
     (if under-control
       [:li
        [:a
         {:className ""
          :onClick (fn [x]
                     (.preventDefault x)
                     (put! hist-chan [:keep]))}
         "keep"]]
       [:li])
     (if (and under-control can-go-forward)
       [:li
        [:a
         {:className "right"
          :onClick (fn [x]
                     (.preventDefault x)
                     (put! hist-chan [:forward]))}
         [:span.glyphicon.glyphicon-step-forward]]]
       [:li]
       )
     ]
    [:p.navbar-text (prn-str msg)]
    ]
   ))
