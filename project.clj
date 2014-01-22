(defproject cmsnew "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]                 
                 [sablono "0.1.5"]
                 [crate "0.2.4"]
                 [jayq "2.4.0"]
                 [org.clojars.franks42/cljs-uuid-utils "0.1.3"]                 
                 [com.cemerick/austin "0.1.3"]]
  
  :plugins [[lein-cljsbuild "1.0.1"]]

  :profiles {:dev {:resource-paths ["dev"]
                   :plugins [[com.cemerick/austin "0.1.3"]]}}
  :cljsbuild {
              :builds [{:id "cmsnew"
                        :source-paths ["src/cmsnew" "src/reactor"]
                        :compiler {:output-to "resources/public/js/compiled/cmsnew.js"
                                   :output-dir "resources/public/js/compiled/out"
                                   :externs ["resources/public/js/externs/jquery-1.9.js"]
                                   :optimizations :none
                                   :source-map true}}
                       {:id "pigrep"
                        :source-paths ["src/pigrep"]
                        :compiler {:output-to "resources/public/js/compiled/pigrep.js"
                                   :optimizations :simple
                                   :pretty-print true}}]})
