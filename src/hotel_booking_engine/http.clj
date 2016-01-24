(ns hotel-booking-engine.http
  (:require [clojure.string :refer [upper-case join]]
            [ring.mock.request :refer [header]]
            [ring.util.response :refer [response status]]))

(defn url-from
  "Create a location URL from request data and additional path elements"
  [{scheme :scheme server-name :server-name server-port :server-port uri :uri}
   & path-elements]
  (str "http://" server-name ":" server-port  uri "/" (join "/" path-elements)))

(defn options
  "Generate a 200 HTTP response with an Allow header containing the provided
  HTTP method names - response for an HTTP OPTIONS request"
  ([] (options #{:options} nil))
  ([allowed] (options allowed nil))
  ([allowed body]
   (->
     (response body)
     (header "Allow" (join ", " (map (comp upper-case name) allowed))))))

(defn method-not-allowed
  "Generate a 405 response with an Allow header containing the provided HTTP
  method names"
  [allowed]
  (->
    (options allowed)
    (status 405)))

(defn no-content?
  "Check for a nil or empty response and set status to 204 (No Content) with
  nil body"
  [body]
  (if (or (nil? body) (empty? body))
    (->
      (response nil)
      (status 204))
    (response body)))

(defn not-implemented
  "Return an HTTP 501 (Not Implemented)"
  []
  (->
    (response nil)
    (status 501)))

(defn created
  "Return an HTTP 201 (Created)"
  ([url]
   (created url nil))
  ([url body]
   (->
     (response body)
     (status 201)
     (header "Location" url))))

(defn ok
  "Return an HTTP 200 (OK)"
  [body]
  (->
    (response body)
    (status 200)))
