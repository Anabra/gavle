(ns gavle.util-test
  (:require
   [clojure.test :refer :all]
   [clojure.template :refer [do-template]]
   [gavle.util :as sut]))

(deftest update-in-if-exists
  (do-template
   [title m ks f expected]
   (let [actual (sut/update-in-if-exists m ks f)]
     (testing title
       (is (= expected actual))))

   "update existing nested key"
   {:a {:b "x"}} [:a :b] keyword
   {:a {:b :x}}

   "update non-existent nested key"
   {:a {:c "x"}} [:a :b] keyword
   {:a {:c "x"}}

   "update non-existent nested key (entire nested structure missing)"
   {:a "x"} [:a :b] keyword
   {:a "x"}))
