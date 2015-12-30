(ns hypertension.core-test
  (:require  [midje.sweet :refer :all]
             [hypertension.core :refer :all]))

(facts "default implementation of resource-url"
  (fact "returns nil for record with missing fields"
    (resource-url {}) => nil
    (resource-url {:id 1}) => nil
    (resource-url {:resource-type "foo"}) => nil)

  (let [subject {:id 1 :resource-type "foo"}]
    (fact "constructs url path for resource"
      (resource-url subject) => "/foo/1")
    (facts "options"
      (fact "will append to base-url"
        (resource-url {} {:base-url "foo.com"}) => nil
        (resource-url subject {:base-url "foo.com"}) => "foo.com/foo/1"
        (resource-url subject {:base-url "foo.com/"}) => "foo.com/foo/1"))))

(facts "default relationship-url"
  (fact "returns nil for record with missing fields"
    (relationship-url {} :bar) => nil
    (relationship-url {:id 1} :bar) => nil
    (relationship-url {:resource-type "foo"} :bar) => nil
    (relationship-url {:resource-type "foo" :id 1} :bar) => nil)

  (let [subject {:id 1
                 :resource-type "foo"
                 :related {:bar {}
                           "baz" {}}}]
    (fact "creates url for named relationship"
      (relationship-url subject :bar) => "/foo/1/relationships/bar"
      (relationship-url subject "baz") => "/foo/1/relationships/baz")
    (fact "passes on :base-url option"
      (relationship-url subject :bar {:base-url "foo.com"})
      => "foo.com/foo/1/relationships/bar")))

(facts "default related-resource-url"
  (fact "returns nil for record with missing fields"
    (related-resource-url {} :bar) => nil
    (related-resource-url {:id 1} :bar) => nil
    (related-resource-url {:resource-type "foo"} :bar) => nil
    (related-resource-url {:resource-type "foo" :id 1} :bar) => nil)

  (let [subject {:id 1
                 :resource-type "foo"
                 :related {:bar {:id 2
                                 :resource-type "baz"}
                           :quux [{:id 3 :resource-type "baz"}]}}]
    (fact "creates url for named relationship"
      (related-resource-url subject :bar) => "/foo/1/bar")

    (fact "to-many relationship"
      (related-resource-url subject [:bar 3]) => "/foo/1/bar/3")
    (fact "passes on :base-url option"
      (related-resource-url subject :bar {:base-url "foo.com"})
      => "foo.com/foo/1/bar")))
