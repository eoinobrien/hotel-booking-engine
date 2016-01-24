(ns hotel-booking-engine.handler
  (:use compojure.core
        ring.util.response
        [ring.middleware.format-response :only [wrap-restful-response]])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(defroutes api-routes
           (context "/api" []
             (OPTIONS "/" []
               (->
                 (response {:version "0.1.0-SNAPSHOT"})
                 (header "Allow" "OPTIONS")))
             (ANY "/" []
               (->
                 (response nil)
                 (status 405)
                 (header "Allow" "OPTIONS"))))
           (route/not-found "Nothing to see here, move along now"))

(def app
  (->
    (handler/api api-routes)
    (wrap-restful-response)))