(defproject web-service "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [compojure "1.6.1"]
                 [ring "1.7.0"]]
  :main ^:skip-aot web-service.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
