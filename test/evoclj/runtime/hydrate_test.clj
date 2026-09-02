(ns evoclj.runtime.hydrate-test
  "H1 hydration factory tests: real genome load path vs fallback."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [evoclj.genome.load :as load]
            [evoclj.compiler.core :as compiler]
            [evoclj.runtime.hydrate :as hydrate]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.genome :as store-genome]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.util UUID)))

;; --- fixtures --------------------------------------------------------------

(def ^:private fixture-catalog
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(def ^:private route-descriptor
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- fixture-loaded-genome
  "Load the minimal-valid bundle and attach the seed route program."
  []
  (assoc (load/load-genome (.toPath (io/file (io/resource "fixtures/genomes/minimal-valid"))))
         :programs [route-descriptor]))

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [p (str (java.nio.file.Files/createTempFile "evoclj-hydrate-" ".db"
                                                   (make-array java.nio.file.attribute.FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (try (java.nio.file.Files/deleteIfExists (java.nio.file.Paths/get p (into-array String [])))
         (catch Exception _)))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(def ^:private now "2025-01-01T00:00:00Z")

(defn- fresh-db []
  (let [path (temp-db-path)]
    (migrate/migrate! path)
    path))
(defn- seed-identity!
  "Seed artifacts, genomes, and a generation row so create-session! FK checks pass."
  [db genome-id resolution-id phenotype-id generation-id]
  (sqlite/with-db [conn db]
    (doseq [h [genome-id resolution-id phenotype-id]]
      (try (jdbc/insert! conn :artifacts {:hash h :media_type "application/octet-stream" :size 0 :created_at now})
           (catch Exception _)))
    (try (jdbc/insert! conn :genomes {:id genome-id :created_at now})
         (catch Exception _))
    (try (jdbc/insert! conn :generations {:id generation-id :genome_id genome-id :resolution_id resolution-id :parent_id nil :state "active" :current 1 :created_at now})
         (catch Exception _))))

(defn- create-pinned-session! [db genome-id resolution-id phenotype-id generation-id]
  (seed-identity! db genome-id resolution-id phenotype-id generation-id)
  (let [sess (session/create-session! db
                                      {:genome/id genome-id
                                       :resolution/id resolution-id
                                       :phenotype/id phenotype-id
                                       :generation/id generation-id})]
    (event/append-event! db
                         {:session/id (:session/id sess)
                          :generation/id generation-id
                          :phenotype/id phenotype-id
                          :event/type :session/created
                          :prev/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sess))

(defn- fresh-cas []
  (cas/->cas (str (java.nio.file.Files/createTempDirectory "evoclj-hydrate-cas-"
                                                           (make-array java.nio.file.attribute.FileAttribute 0)))))

;; --- fake ids (content-address-shaped but not registered) -------------------

(def ^:private fake-genome     (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private fake-resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private fake-phenotype  (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private fake-gen "generation-fake")

;; ============================================================================
;; 1 — real genome load round-trip returns real (non-fallback) topology
;; ============================================================================

(deftest real-genome-load-returns-real-topology
  (let [db (fresh-db)
        cas-store (fresh-cas)
        loaded (fixture-loaded-genome)
        compiled (compiler/compile-genome loaded fixture-catalog)
        gid (:genome/id loaded)
        rid (:code/resolution-id compiled)
        cid (:code/id compiled)]
    (store-genome/register-loaded-genome! cas-store db loaded fixture-catalog)
    (let [sess (create-pinned-session! db gid rid cid "generation-real")
          handle (hydrate/hydrate {:sqlite db :cas cas-store} (:session/id sess))
          hc (:compiled handle)]
      (testing "pin identity is preserved in the compiled genome"
        (is (= gid (:code/genome-id hc)))
        (is (= rid (:code/resolution-id hc)))
        (is (= cid (:code/id hc))))
      (testing "topology is REAL (not the fallback echo)"
        (is (= :graph/main (:graph/id (:topology hc))))
        (is (= :node/planner (:entry (:topology hc))))
        (is (not= :graph/subagent-echo (:graph/id (:topology hc)))))
      (testing "programs carry the real route descriptor"
        (is (contains? (:programs hc) :program/route))
        (is (= 'agent.route/run (get-in hc [:programs :program/route :entry]))))
      (testing "a real program source is present (not the synthetic fallback)"
        (is (string? (get-in hc [:programs :program/route :file])))))))

;; ============================================================================
;; 2 — fake-id (no registered bundle) still succeeds via fallback
;; ============================================================================

(deftest fake-id-uses-fallback-topology
  (let [db (fresh-db)
        cas-store (fresh-cas)
        sess (create-pinned-session! db fake-genome fake-resolution fake-phenotype fake-gen)
        handle (hydrate/hydrate {:sqlite db :cas cas-store} (:session/id sess))
        hc (:compiled handle)]
    (testing "fallback topology is used"
      (is (= :graph/subagent-echo (:graph/id (:topology hc))))
      (is (= :node/tool (:entry (:topology hc)))))
    (testing "pinned ids are preserved in fallback"
      (is (= fake-genome (:compiled/genome-id hc)))
      (is (= fake-resolution (:compiled/resolution-id hc)))
      (is (= fake-phenotype (:code/id hc))))))

;; ============================================================================
;; 3 — a registered bundle whose pin code-id disagrees throws :hydrate/pin-mismatch
;; ============================================================================

(deftest mismatched-code-id-throws-pin-mismatch
  (let [db (fresh-db)
        cas-store (fresh-cas)
        loaded (fixture-loaded-genome)
        compiled (compiler/compile-genome loaded fixture-catalog)
        gid (:genome/id loaded)
        rid (:code/resolution-id compiled)
        wrong-cid (str "sha256:" (apply str (repeat 64 "d")))]
    (store-genome/register-loaded-genome! cas-store db loaded fixture-catalog)
    (let [sess (create-pinned-session! db gid rid wrong-cid "generation-mismatch")
          result (try
                   (hydrate/hydrate {:sqlite db :cas cas-store} (:session/id sess))
                   {:ok true}
                   (catch clojure.lang.ExceptionInfo e
                     {:error/type (:error/type (ex-data e))}))]
      (testing "hydration fails closed with :hydrate/pin-mismatch"
        (is (= :hydrate/pin-mismatch (:error/type result)))))))

;; ============================================================================
;; 4 — bare sqlite db (call-site convention, no :cas) still falls back
;; ============================================================================

(deftest bare-sqlite-db-uses-fallback
  (let [db (fresh-db)
        sess (create-pinned-session! db fake-genome fake-resolution fake-phenotype fake-gen)
        handle (hydrate/hydrate db (:session/id sess))
        hc (:compiled handle)]
    (is (= :graph/subagent-echo (:graph/id (:topology hc))))))