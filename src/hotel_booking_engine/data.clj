(ns hotel-booking-engine.data
  (:require [clj-time.core :as time]
            [monger.collection :as collection]
            [monger.core :refer [connect get-db]]
            [monger.result :refer [acknowledged?]]
            [monger.util :as util]
            [monger.joda-time]
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
   :doits-collection "doits"})

(let [mongo-client (connect mongo-options)]
  (def mongo-db (get-db mongo-client (mongo-options :db))))

;;
;; Utility Functions
;;

(defn with-oid
  "Add a new Object ID to a DoIt"
  [doit]
  (assoc doit :_id (util/object-id)))

(defn created-now
  "Set the created time in a DoIt to the current time"
  [doit]
  (assoc doit :created (time/now)))

(defn modified-now
  "Set the modified time in a DoIt to the current time"
  [doit]
  (assoc doit :modified (time/now)))

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

(defmethod validate* ::DoIt
  [doit _]
  (if-not (valid? (validation-set
                    (presence-of :_id)
                    (presence-of :title)
                    (presence-of :created)
                    (presence-of :modified)) doit)
    (throw+ {:type ::invalid} "Invalid DoIt")))

(defn validate
  "Execute a sequence of validation tests"
  [& tests]
  (doseq [test tests] (apply validate* test)))

;;
;; DB Access Functions
;;

(defn create-doit
  "Insert a DoIt into the database"
  [doit]
  (let [new-doit (created-now (modified-now (with-oid doit)))]
    (validate [new-doit ::DoIt])
    (if (acknowledged? (collection/insert mongo-db (mongo-options :doits-collection) new-doit))
      new-doit
      (throw+ {:type ::failed} "Create Failed"))))

(defn get-doit
  "Fetch a DoIt by ID"
  [id]
  (validate [id ::ObjectId])
  (let [doit (collection/find-map-by-id mongo-db (mongo-options :doits-collection) (ObjectId. id))]
    (if (nil? doit)
      (throw+ {:type ::not-found} (str id " not found"))
      doit)))

(defn delete-doit
  "Delete a DoIt by ID"
  [id]
  (validate [id ::ObjectId])
  (let [doit (get-doit id)]
    (if (acknowledged? (collection/remove-by-id mongo-db (mongo-options :doits-collection) (ObjectId. id)))
      doit
      (throw+ {:type ::failed} "Delete Failed"))))
