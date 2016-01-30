(ns hypertension.core-test
  (:require  [midje.sweet :refer :all]
             [hypertension.core :refer :all]))

(defrecord Dummy [id type])
(defrecord Dummy2 [id]
  ApiResource
  (resource-url [self] (str "bar"))
  (resource-url [self opts] (str "baz")))

(facts "default implementation of resource-url"
  (fact "returns nil for record with missing fields"
    (resource-url {}) => nil
    (resource-url {:id 1}) => nil
    (resource-url {:type "foo"}) => nil)

  (let [subject {:id 1 :type "foo"}]
    (fact "constructs url path for resource"
      (resource-url subject) => "/foo/1")
    (facts "options"
      (fact "will append to base-url"
        (resource-url {} {:base-url "foo.com"}) => nil
        (resource-url subject {:base-url "foo.com"}) => "foo.com/foo/1"
        (resource-url subject {:base-url "foo.com/"}) => "foo.com/foo/1")))

  (facts "default relationship-url"
    (fact "returns nil for record with missing fields"
      (relationship-url {} :bar) => nil
      (relationship-url {:id 1} :bar) => nil
      (relationship-url {:type "foo"} :bar) => nil
      (relationship-url {:type "foo" :id 1} :bar) => nil)

    (let [subject {:id 1
                   :type "foo"
                   :bar {}
                   "baz" {}}]
      (fact "creates url for named relationship"
        (relationship-url subject :bar) => "/foo/1/relationships/bar"
        (relationship-url subject "baz") => "/foo/1/relationships/baz")
      (fact "passes on :base-url option"
        (relationship-url subject :bar {:base-url "foo.com"})
        => "foo.com/foo/1/relationships/bar"))))

(facts "default related-resource-url"
  (fact "returns nil for record with missing fields"
    (related-resource-url {} :bar) => nil
    (related-resource-url {:id 1} :bar) => nil
    (related-resource-url {:type "foo"} :bar) => nil
    (related-resource-url {:type "foo" :id 1} :bar) => nil)

  (let [subject {:id 1
                 :type "foo"
                 :bar {:id 2
                       :type "baz"}
                 :quux [{:id 3 :type "baz"}]}]
    (fact "creates url for named relationship"
      (related-resource-url subject :bar) => "/foo/1/bar")
    (fact "to-many relationship"
      (related-resource-url subject [:bar 3]) => "/foo/1/bar/3")
    (fact "passes on :base-url option"
      (related-resource-url subject :bar {:base-url "foo.com"})
      => "foo.com/foo/1/bar")))

(facts "protocol"
  (let [rec (->Dummy 5 "foo")]
    (fact "default impl works with records"
      (resource-url rec) => "/foo/5"))
  (let [rec (->Dummy2 5)]
    (fact "can be overridden"
      (resource-url rec) => "bar"
      (resource-url rec {}) => "baz")))

(facts "json-api-data - one resource"
  (fact "creates a valid JSON API data structure"
    (json-api-data {:id 5 :type "foo"}) =>
    {:data {:id 5
            :type "foo"
            :attributes {}}
     :links {:self "/foo/5"}}
    (json-api-data {:id 5 :type "foo" :bar "baz"}) =>
    {:data {:id 5
            :type "foo"
            :attributes {:bar "baz"}}
     :links {:self "/foo/5"}})
  (fact "parses relationships"
    (json-api-data {:id 5 :type "foo"
                    :blah {:id 5 :type "baz"}}
                   {:related-keys [:blah]}) =>
                   {:data {:id 5
                           :type "foo"
                           :attributes {}
                           :relationships
                           {:blah {:links {:self "/foo/1/relationships/blah"
                                          :related "/foo/1/blah"}
                                  :data {:type "baz" :id 5}}}}
                    :links {:self "/foo/5"}}))
