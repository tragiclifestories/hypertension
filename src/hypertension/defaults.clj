(ns hypertension.defaults
  (:require [clojure.string :as s]
            [defun :refer [defun]]))

;;; default implementation of `ApiResource` protocol.
;;; will construct URLs similar to the examples provided
;;; in the json-api spec (https://jsonapi.org/format)

;;; note that the URL formats here are NOT part of the spec:
;;; that would defeat the point of hypermedia entirely.
;;; They are merely as good a way to define URLs as any.(
(defn- construct-full-url
  [path base-url]
  (let [normalized (s/replace base-url #"\/+$" "")]
    (str normalized path)))

(defun resource-url
  "Construct URL for resource from `:id` and `:resource-type` keys."
  ([subject {:base-url base-url}]
   (if-let [path (resource-url subject)]
     (construct-full-url path base-url)))

  ([{:id id :resource-type type}] (str "/" type "/" id))

  ([_] nil))

(defn- qualified-resource
  ([extra-fragment key [subject :as resource-url-args]]
   (qualified-resource extra-fragment key resource-url-args name))
  ([extra-fragment key [subject :as resource-url-args] f]
   (if-let [base (apply resource-url resource-url-args)]
     (when (get-in subject [:related key])
       (str base extra-fragment (f key))))))

(defun relationship-url
  ([subject related-name & args]
   (qualified-resource "/relationships/" related-name (cons subject args))))

(defun related-resource-url
  ([subject [key id] & args]
   (qualified-resource "/" key (cons subject args) #(str (name %) "/" id)))
  ([subject key & args]
   (qualified-resource "/" key (cons subject args)))
  ([_ _] nil))
