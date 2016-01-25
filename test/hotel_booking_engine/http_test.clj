(ns hotel-booking-engine.http-test
  (:require [clojure.test :refer :all]
            [hotel-booking-engine.http :refer :all]
            [ring.mock.request :refer :all]))

(deftest test-url-from
  (testing "Create basic URL"
    (let [url (url-from {:server-name "localhost" :server-port "8080"})]
      (is (string? url))
      (is (= "http://localhost:8080"))))
  (testing "Create basic URL"
    (let [url (url-from {:server-name "localhost" :server-port "8080" :uri "/api/rooms"})]
      (is (string? url))
      (is (= "http://localhost:8080/api/rooms/50e64dd544ae5146ffbb8acf"))))
  (testing "Create basic URL"
    (let [url (url-from {:server-name "localhost" :server-port "8080" :uri "/api/rooms"} "50e64dd544ae5146ffbb8acf" "field")]
      (is (string? url))
      (is (= "http://localhost:8080/api/rooms/50e64dd544ae5146ffbb8acf/field")))))

(deftest test-http-options
  (testing "HTTP Options Default Response"
    (let [response (options)]
      (is (= (response :status) 200))
      (is (nil? (response :body)))
      (is (= (get-in response [:headers "Allow"] "OPTIONS")))))
  (testing "HTTP Options With-Allowed Response"
    (let [response (options [:get :post])]
      (is (= (response :status) 200))
      (is (nil? (response :body)))
      (is (= (get-in response [:headers "Allow"] "GET, POST")))))
  (testing "HTTP Options With-Body Response"
    (let [response (options [:get :post] {:version "version-number"})]
      (is (= (response :status) 200))
      (is (map? (response :body)))
      (is (contains? (response :body) :version))
      (is (= (get-in response [:headers "Allow"] "GET, POST"))))))

(deftest test-http-method-not-allowed
  (testing "HTTP Method Not Allowed With-Options"
    (let [response (method-not-allowed [:options :get])]
      (is (= (response :status) 405))
      (is (nil? (response :body)))
      (is (= (get-in response [:headers "Allow"] "OPTIONS, GET"))))))

(deftest test-http-no-content?
  (testing "HTTP No-Content nil body"
    (let [response (no-content? nil)]
      (is (= (response :status) 204))
      (is (nil? (response :body)))))
  (testing "HTTP No-Content empty body"
    (let [response (no-content? {})]
      (is (= (response :status) 204))
      (is (nil? (response :body)))))
  (testing "HTTP No-Content not-a-sequence body"
    (let [response (no-content? "string")]
      (is (= (response :status) 200)))))

(deftest test-http-not-implemented
  (testing "HTTP No-Content not-a-sequence Response"
    (let [response (not-implemented)]
      (is (= (response :status) 501))
      (is (nil? (response :body))))))

(deftest test-http-created
  (testing "Create with location"
    (let [response (created (url-from (request :post "/api/rooms") "50e64dd544ae5146ffbb8acf"))
          location (get-in response [:headers "location"])]
      (is (= (response :status) 201))
      (is (= location "http://localhost:80/api/rooms/50e64dd544ae5146ffbb8acf"))
      (is (nil? (response :body)))))
  (testing "Create with location & body"
    (let [response (created (url-from (request :post "/api/rooms") "50e64dd544ae5146ffbb8acf") {:title "test"})
          location (get-in response [:headers "location"])
          body (response :body)]
      (is (= (response :status) 201))
      (is (= location "http://localhost:80/api/rooms/50e64dd544ae5146ffbb8acf"))
      (is (map? body)))))