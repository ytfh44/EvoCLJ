(ns evoclj.http.api-test
  "component — HTTP API shell route smoke tests.

  Every test drives the Ring handler through `ring.mock.request` with
  no live server."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.http.api :as api]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [ring.mock.request :as mock])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private db-paths (atom []))

(defn- temp-db []
  (let [p (str (Files/createTempFile "evoclj-http-" ".db" (make-array FileAttribute 0)))]
    (swap! db-paths conj p)
    (sqlite/spec p)))

(defn- cleanup! []
  (doseq [p @db-paths]
    (try (clojure.java.io/delete-file p) (catch Exception _ nil)))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- seed-current!
  "Migrated db with one CURRENT generation row; returns the db spec."
  []
  (let [db (temp-db)]
    (migrate/migrate! db)
    (jdbc/execute! db ["INSERT INTO generations (id, genome_id, resolution_id, state, current, created_at) VALUES (?, ?, ?, ?, ?, ?)"
                       "generation-1"
                       "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                       "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                       "active"
                       1
                       "2025-01-01T00:00:00Z"])
    db))

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

;; ============================================================================
;; deployment/current (host polling read path)
;; ============================================================================

(deftest deployment-current-returns-current-generation
  (testing "GET /api/deployment/current returns 200 with the CURRENT generation"
    (let [db (seed-current!)
          response (api/handler (assoc (mock/request :get "/api/deployment/current")
                                       :system {:store/sqlite db}))]
      (is (= 200 (:status response)))
      (is (= "generation-1" (get-in response [:body :generation/id])))
      (is (= "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
             (get-in response [:body :genome/id])))
      (is (nil? (get-in response [:body :canary])) "no persisted rollout state — nil canary")
      (is (string? (get-in response [:body :timestamp]))))))

(deftest deployment-current-returns-404-when-no-current
  (testing "a store with no CURRENT row is a 404 the host answers by falling back to seed"
    (let [db (temp-db)
          _ (migrate/migrate! db)
          response (api/handler (assoc (mock/request :get "/api/deployment/current")
                                       :system {:store/sqlite db}))]
      (is (= 404 (:status response)))
      (is (= :deployment/no-current-generation (get-in response [:body :error/type]))))))

(deftest deployment-current-returns-500-when-no-store
  (testing "a request with no sqlite store is a host wiring bug — 500, never a quiet empty poll"
    (let [response (api/handler (mock/request :get "/api/deployment/current"))]
      (is (= 500 (:status response)))
      (is (= :http/store-unavailable (get-in response [:body :error/type]))))))
