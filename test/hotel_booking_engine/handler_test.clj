(ns hotel-booking-engine.handler-test
  (:require [clojure.test :refer :all]
            [hotel-booking-engine.handler :refer :all]
            [ring.mock.request :refer :all]
            [slingshot.test :refer :all]))

(deftest test-api-routes
  (testing "API Options"
    (let [response (api-routes (request :options "/api"))]
      (is (= (response :status) 200))
      (is (contains? (response :body) :version))))
  (testing "API Get"
    (let [response (api-routes (request :get "/api"))]
      (is (= (response :status) 405))
      (is (nil? (response :body)))))
  (testing "Not Found"
    (let [response (api-routes (request :get "/invalid"))]
      (is (= (response :status) 404)))))

(deftest test-create-doit
  (testing "Create valid doit"
    (let [response (api-routes
                     (-> (request :post "/api/doits")
                         (assoc :body {:title "Test DoIt"})))
          response-body (response :body)
          response-headers (response :headers)]
      (is (= (response :status) 201))
      (is (contains? response-headers "location"))
      (is (map? response-body))
      (is (contains? response-body :_id))
      (is (contains? response-body :title))
      (is (= (response-body :title) "Test DoIt"))
      (is (contains? response-body :created))
      (is (contains? response-body :modified)))))

(deftest test-get-doit
  (testing "Get valid doit"
    (let [response (api-routes
                     (-> (request :post "/api/doits")
                         (assoc :body {:title "Test DoIt"})))
          id (.toString (:_id (response :body)))]
      (is (= (response :status) 201))
      (let [response (api-routes (request :get (str "/api/doits/" id)))
            response-body (response :body)]
        (is (= (response :status) 200))
        (is (map? response-body))
        (is (contains? response-body :_id))
        (is (contains? response-body :title))
        (is (= (response-body :title) "Test DoIt"))
        (is (contains? response-body :created))
        (is (contains? response-body :modified)))))
  (testing "Get with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid]
                  (api-routes (request :get "/api/doits/123456789")))))
  (testing "Get non-existent DoIt"
    (is (thrown+? [:type :hotel-booking-engine.data/not-found]
                  (api-routes (request :get "/api/doits/532d14c35f6cacc494ee47bc"))))))

(deftest test-delete-doit
  (testing "Delete valid doit"
    (let [response (api-routes
                     (-> (request :post "/api/doits")
                         (assoc :body {:title "Test DoIt"})))
          id (.toString (:_id (response :body)))]
      (is (= (response :status) 201))
      (let [response (api-routes (request :delete (str "/api/doits/" id)))
            response-body (response :body)]
        (is (= (response :status) 200)))))
  (testing "Delete with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid]
                  (api-routes (request :delete "/api/doits/123456789")))))
  (testing "Delete non-existent DoIt"
    (is (thrown+? [:type :hotel-booking-engine.data/not-found]
                  (api-routes (request :delete "/api/doits/532d14c35f6cacc494ee47bc"))))))
