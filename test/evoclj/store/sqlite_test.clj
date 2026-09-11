(ns evoclj.store.sqlite-test
  "Contract tests for evoclj.store.sqlite/db-spec — the single coercion
  point for every db handle shape the runtime accepts.

  The stores-map case is the defect this pins: the executor handle shape
  {:sqlite <spec> :cas <cas>} must unwrap to the nested :sqlite spec. A
  previous private copy in evoclj.runtime.subagent-cancel tested a generic
  map branch first and returned the WHOLE stores map, which `spec` passes
  through unchanged — so java.jdbc received a map with no :subprotocol/
  :subname and the call failed."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.store.session-store :as session-store]
            [evoclj.store.sqlite :as sqlite]))

(def ^:private jdbc-spec
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname ":memory:"})

(deftest path-string-and-jdbc-spec-pass-through
  (testing "a path string and an already-formed jdbc spec are returned unchanged"
    (is (= "C:/state/evoclj.db" (sqlite/db-spec "C:/state/evoclj.db")))
    (is (= jdbc-spec (sqlite/db-spec jdbc-spec)))))

(deftest stores-map-unwraps-to-nested-sqlite-spec
  (testing "the executor :stores map yields its :sqlite spec, not the outer map"
    (is (= jdbc-spec
           (sqlite/db-spec {:sqlite jdbc-spec :cas {:root "evoclj-cas"}})))))

(deftest db-bearing-map-unwraps-to-nested-db-spec
  (testing "{:db <spec>} yields the nested spec"
    (is (= jdbc-spec (sqlite/db-spec {:db jdbc-spec})))))

(deftest sqlite-branch-precedes-db-branch
  (testing "order is load-bearing: :sqlite wins over :db"
    (is (= jdbc-spec
           (sqlite/db-spec {:sqlite jdbc-spec :db {:subname "other.db"}})))))

(deftest session-store-handle-unwraps
  (testing "a SessionStore deftype unwraps through the reflective .-db fallback
            (store.sqlite cannot name it in an instance? check without a cycle)"
    (is (= jdbc-spec (sqlite/db-spec (session-store/make-session-store jdbc-spec))))))
