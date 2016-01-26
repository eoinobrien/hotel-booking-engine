(ns hotel-booking-engine.data
  (:require [clj-time.core :as time]
            [monger.collection :as collection]
            [monger.core :refer [connect get-db]]
            [monger.result :refer [acknowledged?]]
            [monger.util :as util]
            [monger.joda-time]
            [taoensso.timbre :refer [debug warn]]
            [clojure.pprint :refer [pprint]]
            [validateur.validation :refer [presence-of
                                           valid? validation-set]]
            [slingshot.slingshot :refer [throw+]])
  (:import (org.bson.types ObjectId)))

(def mongo-db (atom {}))

;;
;; Database Connection Details
;;

(def mongo-options
  {:host                "localhost"
   :port                27017
   :db                  "booking"
   :rooms-collection    "rooms"
   :bookings-collection "bookings"})

(let [mongo-client (connect mongo-options)]
  (reset! mongo-db (get-db mongo-client (mongo-options :db))))

;;
;; Utility Functions
;;

(defn with-oid
  "Add a new Object ID to a Room"
  [room]
  (assoc room :_id (util/object-id)))

(defn created-now
  "Set the created time in a Room to the current time"
  [room]
  (assoc room :created (time/now)))

(defn modified-now
  "Set the modified time in a Room to the current time"
  [room]
  (assoc room :modified (time/now)))

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
                    (presence-of :num_rooms)
                    (presence-of :base_rate)
                    (presence-of :num_occupants)
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
                    (presence-of :checkin_date)
                    (presence-of :checkout_date)
                    (presence-of :num_people)
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
  (let [room (collection/find-map-by-id @mongo-db (mongo-options :rooms-collection) (ObjectId. id))]
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
    (if (acknowledged? (collection/update-by-id @mongo-db (mongo-options :rooms-collection) (ObjectId. id) new-room))
      new-room
      (throw+ {:type ::failed} "Update Failed"))))

(defn delete-room
  "Delete a Room by ID"
  [id]
  (validate [id ::ObjectId])
  (let [room (get-room id)]
    (if (acknowledged? (collection/remove-by-id @mongo-db (mongo-options :rooms-collection) (ObjectId. id)))
      room
      (throw+ {:type ::failed} "Delete Failed"))))


;;
;; Bookings
;;

(defn create-booking
  "Insert a Booking into the database"
  [booking]
  (let [new-booking (created-now (modified-now (with-oid booking)))]
    (validate [new-booking ::Booking])
    (if (acknowledged? (collection/insert @mongo-db (mongo-options :bookings-collection) new-booking))
      new-booking
      (throw+ {:type ::failed} "Create Failed"))))

(defn get-booking
  "Fetch a Booking by ID"
  [id]
  (validate [id ::ObjectId])
  (let [booking (collection/find-map-by-id @mongo-db (mongo-options :bookings-collection) (ObjectId. id))]
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
    (if (acknowledged? (collection/update-by-id @mongo-db (mongo-options :bookings-collection) (ObjectId. id) new-booking))
      new-booking
      (throw+ {:type ::failed} "Update Failed"))))

