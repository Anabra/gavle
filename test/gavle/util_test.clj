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

(deftest assoc-front
  (do-template
   [title m kvs expected]
   (let [actual (apply (partial sut/assoc-front m) kvs)]
     (testing title
       (is (= expected actual))))

   "assoc into empty"
   {} [:a 1]
   {:a 1}

   "assoc multiple"
   {} [:a 1 :b 2]
   {:a 1 :b 2}

   "asoc multiple to non-empty"
   {:a 1} [:b 2 :c 3]
   {:a 1 :b 2 :c 3}))
