(ns hotel-booking-engine.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [hotel-booking-engine.handler :refer :all]))

(deftest test-api-routes
  (testing "API Options"
    (let [response (api-routes (mock/request :options "/api"))]
      (is (= (response :status) 200))
      (is (contains? (response :body) :version))))
  (testing "API Get"
    (let [response (api-routes (mock/request :get "/api"))]
      (is (= (response :status) 405))
      (is (nil? (response :body)))))
  (testing "Not Found"
    (let [response (api-routes (mock/request :get "/invalid"))]
      (is (= (response :status) 404)))))