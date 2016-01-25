(ns hotel-booking-engine.data-test
  (:require [clj-time.core :as time]
            [clojure.test :refer :all]
            [hotel-booking-engine.data :refer :all]
            [monger.core :refer [connect get-db]]
            [slingshot.test :refer :all]))

(defn mongo-connection [f]
  (let [mongo-client (connect {:host "localhost" :port 27017})]
    (get-db mongo-client "booking-test"))
  (f))

(use-fixtures :once mongo-connection)

(deftest test-validation
  (testing "Valid Room ID"
    (is (nil? (validate ["532d14c35f6cacc494ee47bc" :hotel-booking-engine.data/ObjectId]))))
  (testing "Invalid Room ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (validate ["123456789" :hotel-booking-engine.data/ObjectId]))))
  (testing "Valid Room"
    (is (nil? (validate [(created-now (modified-now (with-oid {:title         "Get Test Room"
                                                               :num_occupants 1
                                                               :base_rate     20
                                                               :num_rooms     1})))
                         :hotel-booking-engine.data/Room]))))
  (testing "Invalid Room"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (validate [{:title         "Get Test Room"
                                                                         :num_occupants 1
                                                                         :base_rate     20
                                                                         :num_rooms     1}
                                                                        :hotel-booking-engine.data/Room])))))

(deftest test-create-room
  (testing "Create Valid Room"
    (let [room {:title         "Newly Created Test Room"
                :num_occupants 1
                :base_rate     20
                :num_rooms     1}
          created (create-room room)]
      (is (map? created))
      (is (contains? created :_id))
      (is (contains? created :title))
      (is (contains? created :num_occupants))
      (is (contains? created :base_rate))
      (is (contains? created :num_rooms))
      (is (contains? created :created))
      (is (contains? created :modified))))
  (testing "Create Invalid Room"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (create-room {})))))

(deftest test-get-room
  (testing "Get valid room"
    (let [created (create-room {:title         "Get Test Room"
                                :num_occupants 1
                                :base_rate     20
                                :num_rooms     1})
          room (get-room (.toString (created :_id)))]
      (is (map? room))
      (is (contains? room :_id))
      (is (contains? room :title))
      (is (contains? room :num_occupants))
      (is (contains? room :base_rate))
      (is (contains? room :num_rooms))
      (is (contains? room :created))
      (is (contains? room :modified))))
  (testing "Get with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (get-room "123456789"))))
  (testing "Get non-existent Room"
    (is (thrown+? [:type :hotel-booking-engine.data/not-found] (get-room "532d14c35f6cacc494ee47bc")))))

(deftest test-update-room
  (testing "Update Valid Room"
    (let [room {:title         "Newly Updated Test Room"
                :num_occupants 1
                :base_rate     20
                :num_rooms     1}
          id (.toString ((create-room room) :_id))
          created (update-room id {:base_rate 30})]
      (is (map? created))
      (is (contains? created :_id))
      (is (contains? created :title))
      (is (contains? created :num_occupants))
      (is (contains? created :base_rate))
      (is (contains? created :num_rooms))
      (is (contains? created :created))
      (is (contains? created :modified))))
  (testing "Update Existing Room with Empty Map"
    (let [room {:title         "Newly Updated Test Room"
                :num_occupants 1
                :base_rate     20
                :num_rooms     1}
          id (.toString ((create-room room) :_id))
          created (update-room id {})]
      (is (map? created))
      (is (contains? created :_id))
      (is (contains? created :title))
      (is (contains? created :num_occupants))
      (is (contains? created :base_rate))
      (is (contains? created :num_rooms))
      (is (contains? created :created))
      (is (contains? created :modified))))
  (testing "Update Non Existant Room"
    (is (thrown+? [:type :hotel-booking-engine.data/not-found] (update-room "532d14c35f6cacc494efd333" {}))))
  (testing "Update Room with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (update-room "123456789" {:title         "Newly Updated Test Room"
                                                                                       :num_occupants 1
                                                                                       :base_rate     20
                                                                                       :num_rooms     1
                                                                                       :_id           "123456789"})))))

(deftest test-delete-room
  (testing "Delete room"
    (let [created (create-room {:title         "Newly Created Test Room"
                                :num_occupants 1
                                :base_rate     20
                                :num_rooms     1})
          deleted (delete-room (.toString (created :_id)))]
      (is (not (nil? deleted)))))
  (testing "Delete with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (delete-room "123456789")))))