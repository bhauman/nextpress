(ns frontier.core
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [frontier.system.core :refer [system system-with-initial-inputs component-group]]
   [frontier.system.example-components :refer [ExampleCounter
                                               ExampleTodos]]
   [frontier.system.meta-components :refer [managed-system
                                            render-history-controls]]   
   [reactor.core :refer [render-to raw]]
   [sablono.core :as sab :include-macros true]  
   [jayq.core :refer [$ html]]
   [jayq.util :refer [log]])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go alt! go-loop]]
   [cmsnew.publisher.util.macros :refer [chan->>]])  
  )

(defn create-test-divs [parent-sel count]
  (mapv
   (fn [x]
     (.append ($ parent-sel) (str "<div id='test" x "'></div>" )))
   (range count)))


(defn render-input-message-links [msgs event-chan]
  [:ul
   (map (fn [x] [:li [:a
                     { :onClick (fn [] (put! event-chan x)) }
                     (prn-str x)]])
        msgs)])

(defn json-renderer [target-id]
  (let [target-node (.getElementById js/document target-id)]
    (fn [cs event-chan hist-state hist-chan]
      (let [state (or (:render-state hist-state) cs)]
        (render-to (sab/html
                    [:div
                     (raw (.render_json js/JSONRenderer (clj->js (dissoc state :__msg))))
                     (render-input-message-links
                      [[:create-todo {:content "hello"}]
                       [:create-todo {:content "goodbye"}]
                       [:create-todo {:content "heller"}]]
                      event-chan)
                   (render-history-controls hist-state hist-chan)])
                   target-node
                   identity)))))

(create-test-divs "#cmsnew" 5)

#_(system-with-initial-inputs {}
        (component-group
         (ExampleCounter.)
         (ExampleTodos.))
        (json-renderer "test0")
        [[:create-todo {:content "hello"}]
         [:create-todo {:content "goodbye"}]
         [:create-todo {:content "heller"}]])

(managed-system {}
        (component-group
         (ExampleCounter.)
         (ExampleTodos.))
        (json-renderer "test0")
        [[:create-todo {:content "hello"}]
         [:create-todo {:content "goodbye"}]
         [:create-todo {:content "heller"}]])
