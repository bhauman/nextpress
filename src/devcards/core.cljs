(ns devcards.core
  (:require
   [frontier.core :refer [iPluginInit
                          iTransform
                          iDerive
                          iInputFilter
                          run
                          compose]]
   [jayq.core :refer [$]]
   [crate.core :as c]
   [clojure.string :as string]
   [cljs.core.async :refer [put!]]
   [cljs.reader :refer [read-string]]))

(enable-console-print!)

(defn ljs [d]
  (.log js/console (clj->js d)))

(defn unique-card-id [path]
  (string/join "-" (map name (cons :cardpath path))))

(defn current-page [data]
  (and (:current-path data)
       (:cards data)
       (get-in (:cards data) (:current-path data))))

(defrecord DevCard [path tags func])

(def devcard? (partial instance? DevCard))

;; input filters
(defmulti ifilter first)

(defmethod ifilter :default [msg state] msg)

(defmethod ifilter :set-current-path [[_ {:keys [path]}] state]
  [:current-path {:path (vec (map keyword (string/split path ":::")))}])

;; transforms
(defmulti dev-trans first)

(defmethod dev-trans :default [msg state] state)

(defmethod dev-trans :register-card [[_ {:keys [path tags func]}] state]
  (update-in state (cons :cards path)
             (fn [x] (DevCard. path tags func))))

(defmethod dev-trans :add-to-current-path [[_ {:keys [path]}] state]
  (update-in state [:current-path] 
             (fn [x] (conj x (keyword path)))))

(defmethod dev-trans :current-path [[_ {:keys [path]}] state]
  (assoc state :current-path (vec path)))

;; derivatives

(defn display-current-cards [state]
  (let [cur (current-page state)]
    (if (devcard? cur)
      (assoc state :display-single-card cur)
      (-> state
          (assoc :display-dir-paths
            (filter (complement (comp devcard? second)) cur))
          (assoc :display-cards
            (filter (comp devcard? second) cur))))))

(defn breadcrumbs [{:keys [current-path] :as state}]
  (let [cpath (map name (cons :home current-path))
        crumbs
        (map (juxt last rest)
             (rest (map-indexed
                    (fn [i v] (subvec v 0 i))
                    (take (inc (count cpath))
                          (repeat (vec cpath))))))]
    (assoc state :breadcrumbs crumbs)))

(defrecord DevCards []
  iInputFilter
  (-filter-input [o msg state]
    (ifilter msg state))
  iTransform
  (-transform [o msg state]
    (dev-trans msg state))
  iDerive
  (-derive [o state]
    (-> state
        display-current-cards
        breadcrumbs)))

(defrecord CurrentPathSessionStorage []
  iPluginInit
  (-initialize [_ state event-chan]
    (when-let [current-path (.getItem js/sessionStorage "__devcards__current-path")]
      (when-let [path (try (read-string current-path) (catch js/Error e nil))]
        (put! event-chan [:current-path {:path path}]))))
  iDerive
  (-derive [o state]
    (.setItem js/sessionStorage "__devcards__current-path" (prn-str (:current-path state)))
    state))

(defn naked-card [{:keys [path]}]
  [:div {:id (unique-card-id path)}])

(defn card-template [{:keys [path] :as card}]
  [:div.panel.panel-default
   [:div.panel-heading.set-current-path
    {:data-path (string/join ":::" (map name path))}
    [:span.panel-title (name (last path)) " "]]
   (naked-card card)])

(defn display-cards [cards]
  (map (comp card-template second) 
       cards))

(defn dir-links [dirs]
  [:div.list-group
   (map (fn [[key child-tree]]
          [:a.list-group-item.add-to-current-path
           {:data-path (name key)}
           [:span.glyphicon.glyphicon-folder-close]
           [:span.badge (count child-tree)]
           [:span " " (name key)]])
        dirs)])

(defn breadcrumbs-templ [crumbs]
  [:ol.breadcrumb
   (map (fn [[n path]]
          [:li
           [:a.set-current-path {:href "#"
                                 :data-path (string/join ":::" path)}
            n]])
        crumbs)])

(defn main-template [data]
  [:div
   [:div.navbar.navbar-default.navbar-inverse.navbar-static-top
    [:div.container
    [:div.navbar-header
     [:a.navbar-brand "Devcards"]]]]
   [:div.container
    [:div.row
     [:div.col-md-9
      (when-let [crumbs (:breadcrumbs data)]
        (breadcrumbs-templ crumbs))
      (when-let [dir-paths (:display-dir-paths data)] 
        (dir-links dir-paths))
      (when-let [cards (:display-cards data)]
        (display-cards cards))
      (when-let [card (:display-single-card data)]
        (naked-card card))]
     [:div.col-md-3
      ]
     ]
    ]
   ])

(defn devcard-renderer [{:keys [state event-chan]}]
  (ljs state)
  (.html ($ "#devcards") (c/html (main-template state)))
  (let [some-cards (map second (:display-cards state))
        cards (if-let [card (:display-single-card state)]
                (cons card some-cards)
                some-cards)]
    (mapv
     (fn [{:keys [func path]}]
       (func ($ (str "#" (unique-card-id path)))))
     cards)))

(def devcard-start { :current-path []
                     :cards {} })

(def devcard-system (run devcard-start
                         (compose
                          (DevCards.)
                          (CurrentPathSessionStorage.))
                         devcard-renderer))

(defn data-to-message [msg-name event-chan]
  (fn [e]
    (let [t (.-currentTarget e)]
      (when-let [data (.data ($ t))]
        (put! event-chan
              [msg-name
               (js->clj data
                        :keywordize-keys true)])))))

(defn register-listeners [sel event-chan]
    (.on ($ sel) "click" "a.add-to-current-path"
         (data-to-message :add-to-current-path event-chan))
    (.on ($ sel) "click" ".set-current-path"
         (data-to-message :set-current-path event-chan)))

(register-listeners "#devcards" (:event-chan devcard-system))

(defn register-card [path tags func]
  (put! (:event-chan devcard-system)
        [:register-card {:path path :tags tags :func func}]))

(register-card [:main :fun]
               "This is a fun description"
               (fn [el]
                 (.html ($ el) (c/html [:h1 "Hello its' working"]))))

(register-card [:main :fun2]
               "This is a fun description"
               (fn [el]
                 (.html ($ el) (c/html [:h1 "Hello 2"]))))

(register-card [:main :fun3]
               "This is a fun description"
               (fn [el]
                 (.html ($ el) (c/html [:h1 "Hello 3"]))))

(register-card [:main :fun4 :wellwell]
               "This is a well well description"
               (fn [el]
                 (.html ($ el) (c/html [:h1 "Hello its' working well well"]))))

(register-card [:main :fun4 :wellwell2]
               "This is a well well description"
               (fn [el]
                 (.html ($ el) (c/html [:h1 "Hello its' working well well well"]))))

(register-card [:main :fun4 :wellwell3]
               "This is a well well description"
               (fn [el]
                 (.html ($ el) (c/html [:h1 "Hello its' working well well well"]))))

(register-card [:other :fun4]
               "This is a fun description"
               (fn [el]
                 (.html ($ el) (c/html [:h1 "Hello 4"]))))
