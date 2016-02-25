(ns hotel-booking-engine.data
  (:require [clj-time.core :as time]
            [clj-time.format :refer [parse formatter formatters]]
            [clj-time.coerce :refer [to-string to-date]]
            [clojure.set :refer [rename-keys]]
            [monger.collection :as collection]
            [monger.core :refer [connect get-db]]
            [monger.result :refer [acknowledged?]]
            [monger.util :as util]
            [monger.conversion :as convert]
            [monger.joda-time]
            [taoensso.timbre :refer [log debug warn]]
            [clojure.pprint :refer [pprint]]
            [validateur.validation :refer [presence-of format-of
                                           valid? validation-set]]
            [slingshot.slingshot :refer [throw+]])
  (:import (org.bson.types ObjectId)))

(def mongo-db (atom {}))

;;
;; Database Connection Details
;;

(def mongo-options
  {:host                     "localhost"
   :port                     27017
   :db                       "booking"
   :rooms-collection         "rooms"
   :bookings-collection      "bookings"
   :room-calender-collection "room-calender"})

(let [mongo-client (connect mongo-options)]
  (reset! mongo-db (get-db mongo-client (mongo-options :db))))

;;
;; Utility Functions
;;

(defn with-oid
  "Add a new Object ID to an Object"
  [object]
  (assoc object :_id (util/object-id)))

(defn created-now
  "Set the created time in an Object to the current time"
  [object]
  (assoc object :created (time/now)))

(defn modified-now
  "Set the modified time in an Object to the current time"
  [object]
  (assoc object :modified (time/now)))

;;
;; Validation Functions
;; (Inspired by http://stackoverflow.com/questions/1640311/should-i-use-a-function-or-a-macro-to-validate-arguments-in-clojure)
;;

(defmulti validate* (fn [val val-type] val-type))

(defmethod validate* ::ObjectId
  [id _]
  (if-not (and
            (not (nil? id))
            (string? id)
            (re-matches #"[0-9a-f]{24}" id))
    (throw+ {:type ::invalid} "Invalid ID")))

(defmethod validate* ::Room
  [room _]
  (if-not (valid? (validation-set
                    (presence-of :_id)
                    (presence-of :title)
                    (presence-of :num-rooms)
                    (presence-of :base-rate)
                    (presence-of :num-occupants)
                    (presence-of :created)
                    (presence-of :modified)) room)
    (throw+ {:type ::invalid} "Invalid Room")))

(defmethod validate* ::Booking
  [booking _]
  (if-not (valid? (validation-set
                    (presence-of :_id)
                    (presence-of [:customer :name])
                    (presence-of [:customer :email])
                    (presence-of :rooms)
                    (presence-of :checkin-date)
                    (format-of :checkin-date :format #"[1-2][0-9][0-9][0-9]-(0[0-9]|1[0-2])-([0-2][0-9]|31)")
                    (presence-of :checkout-date)
                    (format-of :checkout-date :format #"[1-2][0-9][0-9][0-9]-(0[0-9]|1[0-2])-([0-2][0-9]|31)")
                    (presence-of :num-people)
                    (presence-of :amount)
                    (presence-of :created)
                    (presence-of :modified)) booking)
    (throw+ {:type ::invalid} "Invalid Booking")))

(defn validate
  "Execute a sequence of validation tests"
  [& tests]
  (doseq [test tests] (apply validate* test)))

;;
;; Rooms
;;

(defn get-rooms
  "Get all Rooms"
  []
  (collection/find-maps @mongo-db (mongo-options :rooms-collection)))

(defn create-room
  "Insert a Room into the database"
  [room]
  (let [new-room (created-now (modified-now (with-oid room)))]
    (validate [new-room ::Room])
    (if (acknowledged? (collection/insert @mongo-db (mongo-options :rooms-collection) new-room))
      new-room
      (throw+ {:type ::failed} "Create Failed"))))

(defn get-room
  "Fetch a Room by ID"
  [id]
  (validate [id ::ObjectId])
  (let [room (collection/find-map-by-id @mongo-db (mongo-options :rooms-collection)
                                        (ObjectId. id))]
    (if (nil? room)
      (throw+ {:type ::not-found} (str id " not found"))
      room)))

(defn update-room
  "Update a Room in the database"
  [id room]
  (validate [id ::ObjectId])
  (let [old-room (get-room id)
        new-room (merge old-room (dissoc (modified-now room) :_id))]
    (validate [new-room ::Room])
    (if (acknowledged? (collection/update-by-id @mongo-db (mongo-options :rooms-collection)
                                                (ObjectId. id) new-room))
      new-room
      (throw+ {:type ::failed} "Update Failed"))))

(defn delete-room
  "Delete a Room by ID"
  [id]
  (validate [id ::ObjectId])
  (let [room (get-room id)]
    (if (acknowledged? (collection/remove-by-id @mongo-db (mongo-options :rooms-collection)
                                                (util/object-id id)))
      room
      (throw+ {:type ::failed} "Delete Failed"))))


;;
;; Room Calender
;; - data about all rooms on a single day
;;
(defn create-calender-day
  [date]
  (let [rooms (get-rooms)
        room-ids (map #(.toString (get % :_id)) rooms)
        rooms-map (zipmap room-ids (map #(rename-keys (dissoc (assoc % :bookings []) :created :modified)  {:base-rate :rate}) rooms))

        calender-day {:date  (to-string date)
                      :rooms rooms-map}
        calender-day (created-now (modified-now calender-day))
        calender-day (assoc calender-day :_id (ObjectId. (to-date date)))]
    (if (acknowledged? (collection/insert @mongo-db (mongo-options :room-calender-collection) calender-day))
      calender-day
      (throw+ {:type ::failed} "Create Failed"))))

(defn get-calender-day
  "Fetch a Room Calender Details by Date"
  [date]
  (let [calender-day (collection/find-map-by-id @mongo-db (mongo-options :room-calender-collection)
                                                (ObjectId. (to-date date)))]
    (if (nil? calender-day)
      (create-calender-day date)
      calender-day)))

(defn add-booking-to-calender-day-room-map
  [booking calender-day-room]
  (let [num-booked-rooms (count (:bookings calender-day-room))]
    (if (< num-booked-rooms (:num-rooms calender-day-room))
      (conj (:bookings calender-day-room) (.toString (:_id booking)))
      (throw+ {:type ::booked-up} "No rooms available"))))

(defn remove-booking-from-calender-day-room-map
  [booking calender-day-room]
  (disj calender-day-room :bookings (.toString (:_id booking))))

(defn add-booking-to-calender-day
  [booking date]
  (validate [booking ::Booking])
  (let [calender-day (get-calender-day date)
        rooms-to-book (get booking :rooms)
        calender-day (apply
                       #(assoc-in calender-day [:rooms %]
                                  (add-booking-to-calender-day-room-map booking (get-in calender-day [:rooms %])))
                       rooms-to-book)]
    (if (acknowledged? (collection/update-by-id @mongo-db (mongo-options :room-calender-collection)
                                                (ObjectId. (to-date date)) calender-day))
      calender-day
      (throw+ {:type ::failed} "Adding of Booking Failed"))))

(defn remove-booking-from-calender-day
  [booking date]
  (validate [booking ::Booking])
  (let [calender-day (get-calender-day date)
        rooms-to-book (get booking :rooms)
        calender-day (map
                       #(assoc-in calender-day [:rooms %] (remove-booking-from-calender-day-room-map booking (get-in calender-day [:rooms %])))
                       rooms-to-book)]
    (if (acknowledged? (collection/update-by-id @mongo-db (mongo-options :room-calender-collection)
                                                (ObjectId. (to-date date)) calender-day))
      calender-day
      (throw+ {:type ::failed} "Removing of Booking Failed"))))

(defn add-booking-to-calender
  [booking]
  (let [checkin-date (parse (:date formatters) (:checkin-date booking))
        checkout-date (parse (:date formatters) (:checkout-date booking))]
    (loop [date checkin-date]
      (when (time/before? date checkout-date)
        (add-booking-to-calender-day booking date)
        (time/plus checkin-date (time/days 1))))))

(defn remove-booking-from-calender
  [booking]
  (let [checkin-date (parse (:date formatters) (:checkin-date booking))
        checkout-date (parse (:date formatters) (:checkout-date booking))]
    (loop [date checkin-date]
      (when (time/before? date checkout-date)
        (remove-booking-from-calender-day booking date)
        (time/plus checkin-date (time/days 1))))))

;;
;; Bookings
;;

(defn get-bookings
  "Get all Bookings"
  []
  (collection/find-maps @mongo-db (mongo-options :bookings-collection)))

(defn create-booking
  "Insert a Booking into the database"
  [booking]
  (let [new-booking (created-now (modified-now (with-oid booking)))]
    (validate [new-booking ::Booking])
    (if (acknowledged? (collection/insert @mongo-db (mongo-options :bookings-collection) new-booking))
      (if (not (nil? (add-booking-to-calender new-booking)))
        new-booking
        (throw+ {:type ::failed} "Create Failed")))))

(defn get-booking
  "Fetch a Booking by ID"
  [id]
  (validate [id ::ObjectId])
  (let [booking (collection/find-map-by-id @mongo-db (mongo-options :bookings-collection)
                                           (ObjectId. id))]
    (if (nil? booking)
      (throw+ {:type ::not-found} (str id " not found"))
      booking)))

(defn update-booking
  "Update a Booking in the database"
  [id booking]
  (validate [id ::ObjectId])
  (let [old-booking (get-booking id)
        new-booking (merge old-booking (dissoc (modified-now booking) :_id))]
    (validate [new-booking ::Booking])
    (if (acknowledged? (collection/update-by-id @mongo-db (mongo-options :bookings-collection)
                                                (ObjectId. id) new-booking))
      new-booking
      (throw+ {:type ::failed} "Update Failed"))))

(defn delete-booking
  "Delete a Booking by ID"
  [id]
  (validate [id ::ObjectId])
  (let [booking (get-booking id)]
    (if (acknowledged? (collection/remove-by-id @mongo-db (mongo-options :bookings-collection)
                                                (ObjectId. id)))
      (if (not (nil? (remove-booking-from-calender booking)))
        booking
        (throw+ {:type ::failed} "Delete Failed")))))

;;
;; Availability
;;
;(defn get-available-rooms
;  "Find all available rooms between a checkin and checkout date"
;  [checkin-date checkout-date]
;  (let [available-rooms []
;        rooms (get-rooms)
;
;        ]
;    (map #()
;         rooms))
;  (collection/find-maps @mongo-db (mongo-options :rooms-collection)))