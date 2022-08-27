(ns gavle.client-test
  (:require
   [gavle.client :as sut]
   [clojure.template :refer [do-template]]
   [clojure.test :refer :all]
   [vcr-clj.clj-http :refer [with-cassette]]
   [clojure.string :as str]))

(deftest retry-fn-with-wait
  (let [inc-atom (fn [x] (swap! x inc))]
    (do-template
     [title failed? num-retries expected-value-of-atom]
     (let [just-a-mutable-int (atom 0)]
       (testing title
         (is (= expected-value-of-atom
                (sut/retry-fn-with-wait failed? num-retries (fn [] (inc-atom just-a-mutable-int)))))))

     "succesful on first trial"
     (constantly false) 1
     1

     "succesful after 2 retries"
     #(< % 3) 3
     3

     #"unsuccesful after 1 retry"
     (constantly true) 1
     nil)))

;; NOTE: the only difference here is that the function under test accepts an expression
(deftest with-retries
  (let [inc-atom (fn [x] (swap! x inc))]
    (do-template
     [title failed? num-retries expected-value-of-atom]
     (let [just-a-mutable-int (atom 0)]
       (testing title
         (is (= expected-value-of-atom
                (sut/with-retries failed? num-retries (inc-atom just-a-mutable-int))))))

     "succesful on first trial"
     (constantly false) 1
     1

     "succesful after 2 retries"
     #(< % 3) 3
     3

     #"unsuccesful after 1 retry"
     (constantly true) 1
     nil)))

(deftest get-with-handlers!
  (do-template
   [title url params handlers expected]
   (testing title
     (with-cassette (keyword (str "get-with-handlers-" (str/replace title #" " "-")))
       (let [{:keys [status body] :as result} (sut/get-with-handlers! url params handlers)]
         (if (= status 200)
           (is
            (some? body)
            (= ::should-return-the-200-response-as-is expected))
           (is
            (= expected
               (sut/get-with-handlers! url params handlers)))))))

   "simple success"
   "https://google.com" {} {:default (constantly ::got-some-other-error)}
   ::should-return-the-200-response-as-is

   "simple success with params"
   "https://www.google.com/search" {:query-params {:q "asd"}}
   {:default (constantly ::got-some-other-error)}
   ::should-return-the-200-response-as-is

   "404"
   "https://raw.githubusercontent.com/Anabra/gavle/main/XXXX" {}
   {404 (constantly ::got-404) :default (constantly ::got-some-other-error)}
   ::got-404

   "default handler"
   "https://raw.githubusercontent.com/Anabra/gavle/main/XXXX" {}
   {:default (constantly ::got-some-other-error)}
   ::got-some-other-error

   "404 with params"
   "https://raw.githubusercontent.com/Anabra/gavle/main/XXXX" {:foo "bar"}
   {404 (fn [request {:keys [status] :as _response}] [status request]) :default (constantly ::got-some-other-error)}
   [404
    {:url "https://raw.githubusercontent.com/Anabra/gavle/main/XXXX",
     :params {:foo "bar", :throw-exceptions false}}]))
