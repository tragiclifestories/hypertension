(ns hypertension.core
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

(defun _resource-url
  "Construct URL for resource from `:id` and `:type` keys."
  ([subject {:base-url base-url}]
   (if-let [path (_resource-url subject)]
     (construct-full-url path base-url)))

  ([{:id id :type type}] (str "/" type "/" id))

  ([_] nil))

(defn- qualified-resource
  ([extra-fragment key [subject :as resource-url-args]]
   (println extra-fragment key resource-url-args)
   (qualified-resource extra-fragment key resource-url-args name))
  ([extra-fragment key [subject :as resource-url-args] f]
   (if-let [base (apply _resource-url resource-url-args)]
     (when (get subject key)
       (str base extra-fragment (f key))))))

(defun _relationship-url
  ([subject related-name & args]
   (println related-name)
   (qualified-resource "/relationships/" related-name (cons subject args))))

(defun _related-resource-url
  ([subject [key id] & args]
   (qualified-resource "/" key (cons subject args) #(str (name %) "/" id)))
  ([subject key & args]
   (qualified-resource "/" key (cons subject args)))
  ([_ _] nil))

(defprotocol ApiResource
  (resource-url [self] [self opts] "Construct url for resource itself"))

(defprotocol ApiRelatedResource
  (related-resource-url
    [self related-resource] [self related-resource opts]
    "construct url for related resource"))

(defprotocol ApiRelationship
  (relationship-url
    [self related-name] [self related-name opts]
    "Construct URL for relationship"))

(extend-type clojure.lang.PersistentArrayMap
  ApiResource
  (resource-url
    ([self] (_resource-url self))
    ([self opts] (_resource-url self opts)))
  ApiRelationship
  (relationship-url
    ([self related-name]
     (_relationship-url self related-name))
    ([self related-name opts]
     (_relationship-url self related-name opts)))
  ApiRelatedResource
  (related-resource-url
    ([self related-resource] (_related-resource-url self related-resource))
    ([self related-resource opts] (_related-resource-url self related-resource opts))))

(defn- do-thing [resource related-objects]
  (reduce (fn [acc item]
            (println (get related-objects item))
            (assoc acc item
                   {:links {:self (relationship-url resource item)
                            :related (related-resource-url resource (get related-objects item))}
                    :data (get related-objects item)}))
          {}
          (keys related-objects)))

(defn- parse-relationships [resource {related-keys :related-keys}]
  (if related-keys
    (let [attrs (get-in resource [:data :attributes])
          related-objects (do-thing (:data resource) (select-keys attrs related-keys))
          base-resource (assoc-in resource [:data :attributes]
                                  (apply (partial dissoc attrs) related-keys))]
      (assoc-in base-resource [:data :relationships] related-objects))
    resource))



(defn json-api-data
  ([resource]
   (let [attrs (dissoc resource :id :type)]
     {:links {:self (resource-url resource)}
      :data {:id (:id resource)
             :type (:type resource)
             :attributes attrs}}))
  ([resource opts]
   (-> (json-api-data resource)
        (parse-relationships opts))))


(related-resource-url {:id 5 :type "foo" :bar {:id 5 :type "bar"}} :bar)
