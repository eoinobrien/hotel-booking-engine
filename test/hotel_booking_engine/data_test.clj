(ns hotel-booking-engine.data-test
  (:require [clj-time.core :as time]
            [clojure.test :refer :all]
            [hotel-booking-engine.data :refer :all]
            [monger.core :refer [connect get-db]]
            [slingshot.test :refer :all]))

(defn mongo-connection [f]
  (let [mongo-client (connect { :host "localhost" :port 27017 })]
    (get-db mongo-client "booking-test")))

(use-fixtures :once mongo-connection)

(deftest test-validation
  (testing "Valid DoIt ID"
    (is (nil? (validate ["532d14c35f6cacc494ee47bc" :hotel-booking-engine.data/ObjectId]))))
  (testing "Invalid DoIt ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (validate ["123456789" :hotel-booking-engine.data/ObjectId]))))
  (testing "Valid DoIt"
    (is (nil? (validate [(created-now (modified-now (with-oid {:title "Testing, 123"}))) :hotel-booking-engine.data/DoIt]))))
  (testing "Invalid DoIt"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (validate [{:title "Testing, 123"} :hotel-booking-engine.data/DoIt])))))

(deftest test-create-doit
  (testing "Create Valid DoIt"
    (let [doit {:title "Newly Created Test DoIt"
                :description "A New Test DoIt"
                :due (time/plus (time/now) (time/weeks 2))
                :priority 1}
          created (create-doit doit)]
      (is (map? created))
      (is (contains? created :_id))
      (is (contains? created :title))
      (is (contains? created :description))
      (is (contains? created :due))
      (is (contains? created :priority))
      (is (contains? created :created))
      (is (contains? created :modified))))
  (testing "Create Invalid DoIt"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (create-doit {})))))

(deftest test-get-doit
  (testing "Get valid doit"
    (let [created (create-doit {:title "Get Test DoIt"})
          doit (get-doit (.toString (created :_id)))]
      (is (map? doit))
      (is (contains? created :_id))
      (is (contains? created :title))
      (is (contains? created :created))
      (is (contains? created :modified))))
  (testing "Get with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (get-doit "123456789"))))
  (testing "Get non-existent DoIt"
    (is (thrown+? [:type :hotel-booking-engine.data/not-found] (get-doit "532d14c35f6cacc494ee47bc")))))

(deftest test-delete-doit
  (testing "Delete doit"
    (let [created (create-doit {:title "Delete Test DoIt"})
          deleted (delete-doit (.toString (created :_id)))]
      (is (not (nil? deleted)))))
  (testing "Delete with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (delete-doit "123456789")))))