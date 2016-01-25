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

;;
;; Database Connection Details
;;

(def mongo-options
  {:host             "localhost"
   :port             27017
   :db               "booking"
   :rooms-collection "rooms"})

(let [mongo-client (connect mongo-options)]
  (def mongo-db (get-db mongo-client (mongo-options :db))))

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

(defn validate
  "Execute a sequence of validation tests"
  [& tests]
  (doseq [test tests] (apply validate* test)))

;;
;; DB Access Functions
;;

(defn get-rooms
  "Get all Rooms"
  []
  (collection/find-maps mongo-db (mongo-options :rooms-collection)))

(defn create-room
  "Insert a Room into the database"
  [room]
  (let [new-room (created-now (modified-now (with-oid room)))]
    (validate [new-room ::Room])
    (if (acknowledged? (collection/insert mongo-db (mongo-options :rooms-collection) new-room))
      new-room
      (throw+ {:type ::failed} "Create Failed"))))

(defn get-room
  "Fetch a Room by ID"
  [id]
  (validate [id ::ObjectId])
  (let [room (collection/find-map-by-id mongo-db (mongo-options :rooms-collection) (ObjectId. id))]
    (if (nil? room)
      (throw+ {:type ::not-found} (str id " not found"))
      room)))

(defn update-room
  "Update a Room in the database"
  [id room]
  (validate [id ::ObjectId])
  (let [new-room (modified-now room)]
    (validate [new-room ::Room])
    (if (acknowledged? (collection/update-by-id mongo-db (mongo-options :rooms-collection) id new-room))
      new-room
      (throw+ {:type ::failed} "Update Failed"))))

(defn delete-room
  "Delete a Room by ID"
  [id]
  (validate [id ::ObjectId])
  (let [room (get-room id)]
    (if (acknowledged? (collection/remove-by-id mongo-db (mongo-options :rooms-collection) (ObjectId. id)))
      room
      (throw+ {:type ::failed} "Delete Failed"))))
