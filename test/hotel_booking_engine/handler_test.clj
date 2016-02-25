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

(deftest test-create-room
  (testing "Create valid room"
    (let [response (api-routes
                     (-> (request :post "/api/rooms")
                         (assoc :body {:title "Test Room"
                                       :num-occupants 1
                                       :base-rate 20
                                       :num-rooms 1})))
          response-body (response :body)
          response-headers (response :headers)]
      (is (= (response :status) 201))
      (is (contains? response-headers "location"))
      (is (map? response-body))
      (is (contains? response-body :_id))
      (is (contains? response-body :title))
      (is (= (response-body :title) "Test Room"))
      (is (contains? response-body :created))
      (is (contains? response-body :modified)))))

(deftest test-get-room
  (testing "Get valid room"
    (let [response (api-routes
                     (-> (request :post "/api/rooms")
                         (assoc :body {:title "Test Room"
                                       :num-occupants 1
                                       :base-rate 20
                                       :num-rooms 1})))
          id (.toString (:_id (response :body)))]
      (is (= (response :status) 201))
      (let [response (api-routes (request :get (str "/api/rooms/" id)))
            response-body (response :body)]
        (is (= (response :status) 200))
        (is (map? response-body))
        (is (contains? response-body :_id))
        (is (contains? response-body :title))
        (is (= (response-body :title) "Test Room"))
        (is (contains? response-body :created))
        (is (contains? response-body :modified)))))
  (testing "Get with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid]
                  (api-routes (request :get "/api/rooms/123456789")))))
  (testing "Get non-existent Room"
    (is (thrown+? [:type :hotel-booking-engine.data/not-found]
                  (api-routes (request :get "/api/rooms/532d14c35f6cacc494ee47bc"))))))

(deftest test-delete-room
  (testing "Delete valid room"
    (let [response (api-routes
                     (-> (request :post "/api/rooms")
                         (assoc :body {:title "Test Room"
                                       :num-occupants 1
                                       :base-rate 20
                                       :num-rooms 1})))
          id (.toString (:_id (response :body)))]
      (is (= (response :status) 201))
      (let [response (api-routes (request :delete (str "/api/rooms/" id)))
            response-body (response :body)]
        (is (= (response :status) 200)))))
  (testing "Delete with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid]
                  (api-routes (request :delete "/api/rooms/123456789")))))
  (testing "Delete non-existent Room"
    (is (thrown+? [:type :hotel-booking-engine.data/not-found]
                  (api-routes (request :delete "/api/rooms/532d14c35f6cacc494ee47bc"))))))
