(ns evoclj.http.api-test
  "component — HTTP API shell route smoke tests.

  Every test drives the Ring handler through `ring.mock.request` with
  no live server."
  (:require [clojure.test :refer [deftest is testing]]
            [ring.mock.request :as mock]
            [evoclj.http.api :as api]))

;; ============================================================================
;; health
;; ============================================================================

(deftest health-returns-200
  (testing "GET /health returns 200 and a map body"
    (let [response (api/handler (mock/request :get "/health"))]
      (is (= 200 (:status response)))
      (is (= {:status "ok"} (:body response)))
      (is (= "application/edn" (get-in response [:headers "Content-Type"]))))))

;; ============================================================================
;; sessions/:id
;; ============================================================================

(deftest session-status-returns-200
  (testing "GET /sessions/:id returns 200 with the session id in the body"
    (let [response (api/handler (mock/request :get "/sessions/abc-123"))]
      (is (= 200 (:status response)))
      (is (= {:session/id "abc-123" :status "unknown"} (:body response)))
      (is (= "application/edn" (get-in response [:headers "Content-Type"]))))))

;; ============================================================================
;; evolution/status
;; ============================================================================

(deftest evolution-status-returns-200
  (testing "GET /evolution/status returns 200 and a map body"
    (let [response (api/handler (mock/request :get "/evolution/status"))]
      (is (= 200 (:status response)))
      (is (map? (:body response)))
      (is (= "ok" (get (:body response) :status)))
      (is (= "application/edn" (get-in response [:headers "Content-Type"]))))))

;; ============================================================================
;; 404
;; ============================================================================

(deftest unknown-route-returns-404
  (testing "unknown routes return 404 with an error map body"
    (let [response (api/handler (mock/request :get "/unknown"))]
      (is (= 404 (:status response)))
      (is (= :http/not-found (get-in response [:body :error/type]))))))
