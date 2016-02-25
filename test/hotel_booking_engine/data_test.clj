(ns hotel-booking-engine.data-test
  (:require [clj-time.core :as time]
            [clojure.test :refer :all]
            [hotel-booking-engine.data :refer :all]
            [monger.core :refer [connect get-db]]
            [slingshot.test :refer :all])
  (:import (org.bson.types ObjectId)))

(defn mongo-connection [f]
  (let [mongo-client (connect {:host "localhost" :port 27017})]
    (reset! mongo-db (get-db mongo-client "booking-test"))

    ;(drop @mongo-db "rooms")
    ;(drop @mongo-db "bookings")
    (f)))


(use-fixtures :once mongo-connection)

(deftest test-validation
  (testing "Valid ID"
    (is (nil? (validate ["532d14c35f6cacc494ee47bc" :hotel-booking-engine.data/ObjectId]))))
  (testing "Invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (validate ["123456789" :hotel-booking-engine.data/ObjectId]))))
  (testing "Valid Room"
    (is (nil? (validate [(created-now (modified-now (with-oid {:title         "Get Test Room"
                                                               :num-occupants 1
                                                               :base-rate     20
                                                               :num-rooms     1})))
                         :hotel-booking-engine.data/Room]))))
  (testing "Invalid Room"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (validate [{:title         "Get Test Room"
                                                                         :num-occupants 1
                                                                         :base-rate     20
                                                                         :num-rooms     1}
                                                                        :hotel-booking-engine.data/Room]))))
  (testing "Valid Booking"
    (is (nil? (validate [(created-now (modified-now (with-oid {:customer      {
                                                                               :name  "John Smith"
                                                                               :email "john.smith@testing.com"
                                                                               }
                                                               :rooms         ["56a66568f4fa2c733146b58e"],
                                                               :checkin-date  "2016-01-25"
                                                               :checkout-date "2016-01-26"
                                                               :num-people    1
                                                               :amount        30
                                                               :paid-date     nil
                                                               })))
                         :hotel-booking-engine.data/Booking]))))
  (testing "Invalid Booking"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (validate [{:customer      {
                                                                                         :name  "John Smith"
                                                                                         :email "john.smith@testing.com"
                                                                                         }
                                                                         :rooms         ["56a66568f4fa2c733146b58e"],
                                                                         :checkin-date  "2016-01-25"
                                                                         :checkout-date "2016-01-26"
                                                                         :num-people    1
                                                                         :amount        30
                                                                         :paid-date     nil
                                                                         }
                                                                        :hotel-booking-engine.data/Booking])))))

(deftest test-create-room
  (testing "Create Valid Room"
    (let [room {:title         "Newly Created Test Room"
                :num-occupants 1
                :base-rate     20
                :num-rooms     1}
          created (create-room room)]
      (is (map? created))
      (is (contains? created :_id))
      (is (contains? created :title))
      (is (contains? created :num-occupants))
      (is (contains? created :base-rate))
      (is (contains? created :num-rooms))
      (is (contains? created :created))
      (is (contains? created :modified))))
  (testing "Create Invalid Room"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (create-room {})))))

(deftest test-get-room
  (testing "Get valid room"
    (let [created (create-room {:title         "Get Test Room"
                                :num-occupants 1
                                :base-rate     20
                                :num-rooms     1})
          room (get-room (.toString (created :_id)))]
      (is (map? room))
      (is (contains? room :_id))
      (is (contains? room :title))
      (is (contains? room :num-occupants))
      (is (contains? room :base-rate))
      (is (contains? room :num-rooms))
      (is (contains? room :created))
      (is (contains? room :modified))))
  (testing "Get Room with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (get-room "123456789"))))
  (testing "Get non-existent Room"
    (is (thrown+? [:type :hotel-booking-engine.data/not-found] (get-room "532d14c35f6cacc494ee47bc")))))

(deftest test-update-room
  (testing "Update Valid Room"
    (let [room {:title         "Newly Updated Test Room"
                :num-occupants 1
                :base-rate     20
                :num-rooms     1}
          id (.toString ((create-room room) :_id))
          created (update-room id {:base-rate 30})]
      (is (map? created))
      (is (contains? created :_id))
      (is (contains? created :title))
      (is (contains? created :num-occupants))
      (is (contains? created :base-rate))
      (is (contains? created :num-rooms))
      (is (contains? created :created))
      (is (contains? created :modified))))
  (testing "Update Existing Room with Empty Map"
    (let [room {:title         "Newly Updated Test Room"
                :num-occupants 1
                :base-rate     20
                :num-rooms     1}
          id (.toString ((create-room room) :_id))
          created (update-room id {})]
      (is (map? created))
      (is (contains? created :_id))
      (is (contains? created :title))
      (is (contains? created :num-occupants))
      (is (contains? created :base-rate))
      (is (contains? created :num-rooms))
      (is (contains? created :created))
      (is (contains? created :modified))))
  (testing "Update Non Existant Room"
    (is (thrown+? [:type :hotel-booking-engine.data/not-found] (update-room "532d14c35f6cacc494efd333" {}))))
  (testing "Update Room with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (update-room "123456789" {:title         "Newly Updated Test Room"
                                                                                       :num-occupants 1
                                                                                       :base-rate     20
                                                                                       :num-rooms     1
                                                                                       :_id           "123456789"})))))

(deftest test-delete-room
  (testing "Delete room"
    (let [created (create-room {:title         "Newly Created Test Room"
                                :num-occupants 1
                                :base-rate     20
                                :num-rooms     1})
          deleted (delete-room (.toString (created :_id)))]
      (is (not (nil? deleted)))))
  (testing "Delete with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (delete-room "123456789")))))

(deftest test-create-booking
  (testing "Create Valid Booking"
    (let [room (create-room {:title         "Create Test Room"
                             :num-occupants 1
                             :base-rate     20
                             :num-rooms     1})
          booking {:customer      {
                                   :name  "John Smith"
                                   :email "john.smith@testing.com"
                                   }
                   :rooms         [(.toString (:_id room))],
                   :checkin-date  "2016-01-25"
                   :checkout-date "2016-01-26"
                   :num-people    1
                   :amount        30
                   :paid-date     nil}
          created (create-booking booking)]
      (is (map? created))
      (is (contains? created :_id))
      (is (contains? created :customer))
      (is (not (nil? (get-in created [:customer :name]))))
      (is (not (nil? (get-in created [:customer :email]))))
      (is (contains? created :rooms))
      (is (contains? created :checkin-date))
      (is (contains? created :checkout-date))
      (is (contains? created :num-people))
      (is (contains? created :amount))
      (is (contains? created :paid-date))
      (is (contains? created :created))
      (is (contains? created :modified))))
  (testing "Create Invalid Booking"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (create-booking {})))))

(deftest test-get-booking
  (testing "Get valid Booking"
    (let [room (create-room {:title         "Create Test Room"
                             :num-occupants 1
                             :base-rate     20
                             :num-rooms     1})
          created (create-booking {:customer      {
                                                   :name  "John Smith"
                                                   :email "john.smith@testing.com"
                                                   }
                                   :rooms         [(.toString (:_id room))],
                                   :checkin-date  "2016-01-25"
                                   :checkout-date "2016-01-26"
                                   :num-people    1
                                   :amount        30
                                   :paid-date     nil})
          booking (get-booking (.toString (:_id created)))]
      (is (map? booking))
      (is (contains? booking :_id))
      (is (contains? booking :customer))
      (is (not (nil? (get-in booking [:customer :name]))))
      (is (not (nil? (get-in booking [:customer :email]))))
      (is (contains? booking :rooms))
      (is (contains? booking :checkin-date))
      (is (contains? booking :checkout-date))
      (is (contains? booking :num-people))
      (is (contains? booking :amount))
      (is (contains? booking :paid-date))
      (is (contains? booking :created))
      (is (contains? booking :modified))))
  (testing "Get Booking with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (get-booking "123456789"))))
  (testing "Get non-existent Booking"
    (is (thrown+? [:type :hotel-booking-engine.data/not-found] (get-booking "532d14c35f6cacc494ee47bc")))))

(deftest test-update-booking
  (testing "Update Valid Boking"
    (let [room (create-room {:title         "Create Test Room"
                             :num-occupants 1
                             :base-rate     20
                             :num-rooms     1})
          booking-created (create-booking {:customer      {
                                                           :name  "John Smith"
                                                           :email "john.smith@testing.com"
                                                           }
                                           :rooms         [(.toString (:_id room))],
                                           :checkin-date  "2016-01-25"
                                           :checkout-date "2016-01-26"
                                           :num-people    1
                                           :amount        30
                                           :paid-date     nil})
          id (.toString (:_id booking-created))
          created (update-booking id {:rate 30})]
      (is (map? created))
      (is (contains? created :_id))
      (is (contains? created :customer))
      (is (not (nil? (get-in created [:customer :name]))))
      (is (not (nil? (get-in created [:customer :email]))))
      (is (contains? created :rooms))
      (is (contains? created :checkin-date))
      (is (contains? created :checkout-date))
      (is (contains? created :num-people))
      (is (contains? created :amount))
      (is (contains? created :paid-date))
      (is (contains? created :created))
      (is (contains? created :modified))))
  (testing "Update Existing Room with Empty Map"
    (let [room (create-room {:title         "Create Test Room"
                             :num-occupants 1
                             :base-rate     20
                             :num-rooms     1})
          booking-created (create-booking {:customer      {
                                                           :name  "John Smith"
                                                           :email "john.smith@testing.com"
                                                           }
                                           :rooms         [(.toString (:_id room))],
                                           :checkin-date  "2016-01-25"
                                           :checkout-date "2016-01-26"
                                           :num-people    1
                                           :amount        30
                                           :paid-date     nil})
          id (.toString (:_id booking-created))
          created (update-booking id {})]
      (is (map? created))
      (is (contains? created :_id))
      (is (contains? created :customer))
      (is (not (nil? (get-in created [:customer :name]))))
      (is (not (nil? (get-in created [:customer :email]))))
      (is (contains? created :rooms))
      (is (contains? created :checkin-date))
      (is (contains? created :checkout-date))
      (is (contains? created :num-people))
      (is (contains? created :amount))
      (is (contains? created :paid-date))
      (is (contains? created :created))
      (is (contains? created :modified))))
  (testing "Update Non Existant Room"
    (is (thrown+? [:type :hotel-booking-engine.data/not-found] (update-booking "532d14c35f6cacc494efd333" {}))))
  (testing "Update Room with invalid ID"
    (is (thrown+? [:type :hotel-booking-engine.data/invalid] (update-booking "123456789" {:customer      {
                                                                                                          :name  "John Smith"
                                                                                                          :email "john.smith@testing.com"
                                                                                                          }
                                                                                          :rooms         ["56a66568f4fa2c733146b58e"],
                                                                                          :checkin-date  "2016-01-25"
                                                                                          :checkout-date "2016-01-26"
                                                                                          :num-people    1
                                                                                          :amount        30
                                                                                          :paid-date     nil
                                                                                          :_id           "123456789"})))))