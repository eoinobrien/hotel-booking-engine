(ns hotel-booking-engine.handler
  (:require [clojure.walk :refer [keywordize-keys]]
            [compojure.core :refer [ANY
                                    DELETE
                                    GET
                                    HEAD
                                    OPTIONS
                                    POST
                                    PUT
                                    context
                                    defroutes]]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [hotel-booking-engine.data :as data]
            [hotel-booking-engine.http :as http]
            [hotel-booking-engine.middleware :refer [wrap-exception-handler
                                        wrap-request-logger
                                        wrap-response-logger]]
            [ring.middleware.format-response :refer [wrap-restful-response]]
            [ring.middleware.json :refer [wrap-json-body]]))

(defroutes api-routes
           (context "/api" []
             (OPTIONS "/" []
               (http/options [:options] {:version "0.1.0-SNAPSHOT"}))
             (ANY "/" []
               (http/method-not-allowed [:options]))
             (context "/rooms" []
               (GET "/" []
                 (http/ok (data/get-rooms)))
               (GET "/:id" [id]
                 (http/ok (data/get-room id)))
               (HEAD "/:id" [id]
                 (http/not-implemented))
               (POST "/" [:as req]
                 (let [room (data/create-room (keywordize-keys (req :body)))
                       location (http/url-from req (str (room :_id)))]
                   (http/created location room)))
               (PUT "/:id" [id :as req]
                 (let [room (data/update-room id (keywordize-keys (req :body)))
                       location (http/url-from req (str (room :_id)))]
                   (http/created location room)))
               (DELETE "/:id" [id]
                 (http/ok (data/delete-room id)))
               (OPTIONS "/" []
                 (http/options [:options :get :head :put :post :delete]))
               (ANY "/" []
                 (http/method-not-allowed [:options :get :head :put :post :delete])))
             (context "/book" []
               (GET "/" []
                 (http/ok (data/get-rooms)))
               (POST "/" [:as req]
                 (let [booking (data/create-booking (keywordize-keys (req :body)))
                       location (http/url-from req (str (booking :_id)))]
                   (http/created location booking)))
               (OPTIONS "/" []
                 (http/options [:options :get :post]))
               (ANY "/" []
                 (http/method-not-allowed [:options :get :post]))))
           (route/not-found "Nothing to see here, move along now"))

(def app
  "Application entry point & handler chain"
  (->
    (handler/api api-routes)
    (wrap-json-body)
    (wrap-request-logger)
    (wrap-exception-handler)
    (wrap-response-logger)
    (wrap-restful-response)))