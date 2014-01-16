(ns cmsnew.ui.site-selector
  (:require
   [cljs.core.async :as async
    :refer [<! >! chan close! sliding-buffer put! take! alts! timeout onto-chan map< to-chan filter<]]
   [sablono.core :as sab :include-macros true]   
   [cmsnew.ui.templates :as templ]
   [reactor.core :refer [input-value react-render] :as rct]
   [cmsnew.util.log-utils :refer [ld lp log-chan]]
   [clojure.string :as string]
   [cmsnew.publisher.core :as pub]
   [jayq.core :refer [$] :as jq]
   [jayq.util :refer [log]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt! go-loop]]
                   [reactor.macros :refer [owner-as]]))

(defn select-site-form [url errors event-chan]
  (owner-as
   owner
   (sab/html
    [:div.container.signin-area
     [:form.form-signin
      {:role "form"
       :onSubmit #(do (put! event-chan [:site-selected (input-value owner :site-url)])
                      (.preventDefault %))}
      [:h2.form-signin-heading "Enter site url" ]
      (templ/control-group
       :site-url errors
       (sab/text-field
        {:ref "site-url"
         ;; this is for testing
         :value "immubucket.s3-website-us-east-1.amazonaws.com"
         :placeholder "www.my-nextpress-site.com"
         :className "form-control" } "site-url"))
      (sab/submit-button {:className "btn btn-primary btn-block btn-lg"} "Go")]])))

(defn loading-view []
  (sab/html
   [:div.container.signin-area
    [:h2 "loading site"]
    (templ/loading 85)]))

(defn valid-url? [site-url]
  (not (string/blank? site-url)))

(defn correct-input-url [input-url]
  (str "http://" input-url))

(defn validate-site [input-url]
  (go (if (valid-url? input-url)
        (let [config (<! (pub/get-config (correct-input-url input-url)))]
          (if (and config (map? config) (:bucket config))
            config
            false))
        false)))

(defn select-site-loop []
  (let [event-chan (chan)
        html-node (.getElementById js/document "cmsnew")]
    (jq/add-class ($ "body" ) "select-site")
    (go-loop [url ""
              errors {}]
             (<! (react-render html-node (select-site-form url errors event-chan)))
             (let [[msg data] (<! event-chan)]
               (ld [msg data])
               (if (= msg :site-selected)
                 (if-let [validated-config (<! (validate-site data))]
                   (do
                     (ld validated-config)
                     (jq/remove-class ($ "body" ) "select-site")
                     (<! (react-render html-node (loading-view)))
                     { :site-url (correct-input-url data)
                       :config  validated-config })
                   (recur url {:site-url ["Not a valid site url"]}))
                 (recur url errors))))))

