(ns hypertension.core-test
  (:require [midje.sweet :refer :all]
            [hypertension.core :refer :all]))

(facts "about `entity-response`"
  (fact "it handles a basic record"
    a => 1
    (against-background
     (around :checks
             (let [a 1]
               ?form)))))
