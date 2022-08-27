(ns gavle.client
  (:require
   [clj-http.client :as client]
   [gavle.log :as log]
   [gavle.util :as util]))

(defn truncate
  [s]
  (str (subs s 0 100) "...<truncated>"))

(defn retry-fn-with-wait
  [failed? num-tries nullary-fn]
  (let [og-result (nullary-fn)]
    (loop [remaining-tries (dec num-tries)
           wait-time       1000
           result          og-result]
      (cond
        (< remaining-tries 0)
        (log/error! "Maximum number of retries reached, returning nil")

        (failed? result)
        (do
          (log/info! (str "Expression failed, retrying after " (/ wait-time 1000) " seconds"))
          (Thread/sleep wait-time)
          (recur (dec remaining-tries)
                 (* 2 wait-time)
                 (nullary-fn)))

        :else result))))

(defmacro with-retries
  "Try evaluationg an expression, and retry if the result failed.
  Returns nil if none of the attempts succeeded.
  "
  [failed? num-tries expr]
  `(retry-fn-with-wait ~failed? ~num-tries (fn [] ~expr)))

(defn log-non-200-error
  [{:keys [url params] :as _request}
   {:keys [status headers body request-time] :as _response}]
  (log/error! "Request returned non-200 status code"
              {:request
               {:url    url
                :params params}
               :reponse
               {:status       status
                :headers      headers
                :request-time request-time
                :body         (truncate body)}}))

(defn get-with-handlers!
  [url params {:keys [default] :as handlers}]
  (let [params-no-ex (assoc params :throw-exceptions false)
        {:keys [status] :as response} (client/get url params-no-ex)
        request {:url    url
                 :params params-no-ex}
        handler (get handlers status default)]
    (case status
      200 response
      (handler request response))))

(defn retry-get-with-handlers!
  [url params handlers]
  (let [{:keys [body]}
        (with-retries #(not= 200 (:status %)) 3
          (get-with-handlers! url params handlers))]
    (try
      (util/parse-json body)
      (catch Exception e
        (log/error! "Response could not be parsed as JSON"
                    {:request
                     {:url    url
                      :params params}
                     :response
                     {:body body}})))))
