(defproject hotel-booking-engine "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [compojure "1.4.0"]
                 [ring/ring-defaults "0.1.5"]
                 [ring-middleware-format "0.7.0"]
                 [ring/ring-json "0.4.0"]
                 [log4j/log4j "1.2.17"]
                 [com.taoensso/timbre "4.2.1"]
                 [clj-time "0.11.0"]
                 [com.novemberain/monger "3.0.2"]
                 [com.novemberain/validateur "2.5.0"]
                 [slingshot "0.12.2"]]
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler hotel-booking-engine.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})