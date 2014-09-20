(defproject comments "0.0.1-SNAPSHOT"
  :description "Commentingx Plugin for Websites"
  :url "http://"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj" "src/cljs"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [com.cemerick/piggieback "0.1.3"]
                 [ring "1.2.1"]  
                 [compojure "1.1.6" :exclusions [org.clojure/core.incubator]]
                 [enlive "1.1.5"]
                 [com.cemerick/friend "0.2.0"]
                 [org.clojure/tools.reader "0.8.3"]
                 [javax.servlet/servlet-api "2.5"]

                 ;;figwheel requirements
                 [com.facebook/react "0.11.1"]
                 [figwheel "0.1.4-SNAPSHOT"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [sablono "0.2.21"]
                 [om "0.7.1"]]

  :plugins [[com.cemerick/austin "0.1.5"]
            [lein-cljsbuild "1.0.3"]
            [lein-ring "0.8.11"]
            [lein-figwheel "0.1.4-SNAPSHOT"]]

  :profiles {:dev {:repl-options {:init-ns comments.core}
                   :plugins []
                   :cljsbuild {:builds [{:source-paths ["src/cljs"]
                                         :compiler {:output-to "resources/public/scripts/compiled/app.js"
                                                    :output-dir "resources/public/scripts/compiled"
                                                    :optimizations :none
                                                    :source-map true}}]}}}
  :main comments.core

  :figwheel {:http-server-root "public" ;; default and assumes "resources" 
             :server-port 3449 ;; default
             ;; :css-dirs ["public/resources/css"] ;; watch and update CSS
             :ring-handler comments.core/secured-site
             }
  
  
  :ring {:handler comments.core/secured-site}
  ;; :ring :handler comments.core/secured-site
  ;;       :init comments.core/-main
  ;;       :listener-class comments
  
)
