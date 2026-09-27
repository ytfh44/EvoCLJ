(ns evoclj.runtime.hydrate-test
  "H1 hydration factory tests: real genome load path vs fallback."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [evoclj.genome.load :as load]
            [evoclj.compiler.core :as compiler]
            [evoclj.runtime.hydrate :as hydrate]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.genome :as store-genome]
            [evoclj.store.identity :as identity]
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
  [db genome-id resolution-id code-id generation-id]
  (sqlite/with-db [conn db]
    (doseq [h [genome-id resolution-id code-id]]
      (try (jdbc/insert! conn :artifacts {:hash h :media_type "application/octet-stream" :size 0 :created_at now})
           (catch Exception _)))
    (try (jdbc/insert! conn :genomes {:id genome-id :created_at now})
         (catch Exception _))
    (try (jdbc/insert! conn :generations {:id generation-id :genome_id genome-id :resolution_id resolution-id :parent_id nil :state "active" :current 1 :created_at now})
         (catch Exception _))))

(defn- create-pinned-session!
  [db genome-id resolution-id code-id generation-id]
  (seed-identity! db genome-id resolution-id code-id generation-id)
  (let [sess (session/create-session! db
                                      {:genome/id genome-id
                                       :resolution/id resolution-id
                                       :code/id code-id
                                       :generation/id generation-id})]
    (event/append-event! db
                         {:session/id (:session/id sess)
                          :generation/id generation-id
                          :code/id code-id
                          :event/type :session/created
                          :prev/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sess))

(defn- register-code-image!
  "Register the code_images row the fail-closed pin check requires."
  [db code-id genome-id resolution-id]
  (identity/record-code-image! db {:code/id code-id
                                   :code/genome-id genome-id
                                   :code/resolution-id resolution-id
                                   :abi {:kernel 1 :genome 1 :intent 1 :tool 1}}))

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
    (register-code-image! db cid gid rid)
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
;; 2 — an unregistered identity FAILS CLOSED (no synthetic fallback)
;; ============================================================================

(deftest unregistered-identity-fails-closed
  (let [db (fresh-db)
        cas-store (fresh-cas)
        sess (create-pinned-session! db fake-genome fake-resolution fake-phenotype fake-gen)
        result (try
                 (hydrate/hydrate {:sqlite db :cas cas-store} (:session/id sess))
                 {:ok true}
                 (catch clojure.lang.ExceptionInfo e
                   {:error/type (:error/type (ex-data e))
                    :reason (:reason (ex-data e))}))]
    (testing "a pin with no identity row is a typed pin mismatch"
      (is (= :hydrate/pin-mismatch (:error/type result)))
      (is (= :code-image-row-missing (:reason result))))
    (testing "verify-pin! applies the same rule without building an executor"
      (let [vresult (try
                      (hydrate/verify-pin! {:sqlite db :cas cas-store}
                                           (:session/id sess))
                      {:ok true}
                      (catch clojure.lang.ExceptionInfo e
                        {:error/type (:error/type (ex-data e))
                         :reason (:reason (ex-data e))}))]
        (is (= :hydrate/pin-mismatch (:error/type vresult)))
        (is (= :code-image-row-missing (:reason vresult)))))
    (testing "a registered code image without a bundle is a bundle failure"
      (register-code-image! db fake-phenotype fake-genome fake-resolution)
      (is (= :hydrate/genome-bundle-missing
             (try (hydrate/hydrate {:sqlite db :cas cas-store} (:session/id sess))
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    (:error/type (ex-data e)))))))))

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
    ;; the WRONG code id has its own row, so authentication passes and the
    ;; re-compiled bundle identity is what disagrees
    (register-code-image! db wrong-cid gid rid)
    (let [sess (create-pinned-session! db gid rid wrong-cid "generation-mismatch")
          result (try
                   (hydrate/hydrate {:sqlite db :cas cas-store} (:session/id sess))
                   {:ok true}
                   (catch clojure.lang.ExceptionInfo e
                     {:error/type (:error/type (ex-data e))}))]
      (testing "hydration fails closed with :hydrate/pin-mismatch"
        (is (= :hydrate/pin-mismatch (:error/type result)))))))

;; ============================================================================
;; 4 — a bare sqlite db carries no CAS, so no bundle can be loaded
;; ============================================================================

(deftest bare-sqlite-db-fails-closed-without-a-bundle
  (let [db (fresh-db)
        _ (register-code-image! db fake-phenotype fake-genome fake-resolution)
        sess (create-pinned-session! db fake-genome fake-resolution fake-phenotype fake-gen)
        result (try
                 (hydrate/hydrate db (:session/id sess))
                 {:ok true}
                 (catch clojure.lang.ExceptionInfo e
                   {:error/type (:error/type (ex-data e))}))]
    (is (= :hydrate/genome-bundle-missing (:error/type result)))))

;; ============================================================================
;; 5 — a pin naming a deployment/execution whose row is missing fails closed
;; ============================================================================

(deftest missing-deployment-and-execution-rows-fail-closed
  (let [db (fresh-db)
        cas-store (fresh-cas)
        _ (register-code-image! db fake-phenotype fake-genome fake-resolution)
        _ (seed-identity! db fake-genome fake-resolution fake-phenotype fake-gen)
        failure-of (fn [sess]
                     (try
                       (hydrate/verify-pin! {:sqlite db :cas cas-store}
                                            (:session/id sess))
                       nil
                       (catch clojure.lang.ExceptionInfo e
                         {:error/type (:error/type (ex-data e))
                          :reason (:reason (ex-data e))})))
        pin-session (fn [code-id deployment-id execution-id]
                      (session/create-session!
                       db
                       (cond-> {:genome/id fake-genome
                                :resolution/id fake-resolution
                                :code/id code-id
                                :generation/id fake-gen}
                         deployment-id (assoc :deployment/id deployment-id)
                         execution-id (assoc :execution/id execution-id))))]
    (testing "an execution row that was never registered fails closed"
      (let [f (failure-of (pin-session fake-phenotype
                                       (str "sha256:" (apply str (repeat 64 "e")))
                                       (java.util.UUID/randomUUID)))]
        (is (= :hydrate/pin-mismatch (:error/type f)))
        (is (= :execution-row-missing (:reason f)))))
    (testing "with the execution row present the deployment row is checked"
      (let [did (str "sha256:" (apply str (repeat 64 "e")))
            eid (java.util.UUID/randomUUID)]
        (identity/record-deployment! db {:deployment/id did
                                         :code/id fake-phenotype
                                         :bindings []
                                         :authority []})
        (identity/record-execution! db {:execution/id eid
                                        :deployment/id did
                                        :code/id fake-phenotype})
        (let [f (failure-of (pin-session fake-phenotype
                                         (str "sha256:" (apply str (repeat 64 "a")))
                                         eid))]
          (is (= :hydrate/pin-mismatch (:error/type f)))
          (is (= :deployment-row-missing (:reason f))))))
    (testing "rows present but naming another code image is a code-image mismatch"
      (let [other-code (str "sha256:" (apply str (repeat 64 "f")))
            did (str "sha256:" (apply str (repeat 64 "b")))
            eid (java.util.UUID/randomUUID)]
        (register-code-image! db other-code fake-genome fake-resolution)
        (identity/record-deployment! db {:deployment/id did
                                         :code/id other-code
                                         :bindings []
                                         :authority []})
        (identity/record-execution! db {:execution/id eid
                                        :deployment/id did
                                        :code/id other-code})
        (let [f (failure-of (pin-session fake-phenotype did eid))]
          (is (= :hydrate/pin-mismatch (:error/type f)))
          (is (= :code-image-mismatch (:reason f))))))
    (testing "a code image row naming another genome is a code-image mismatch"
      (let [pin-code (str "sha256:" (apply str (repeat 64 "7")))
            other-genome (str "sha256:" (apply str (repeat 64 "9")))]
        ;; the artifacts row satisfies the sessions.phenotype_id FK (the
        ;; frozen CodeImageId column); the code_images row is what disagrees
        (artifact/ensure-artifact! db pin-code "application/edn" 0)
        (identity/record-code-image! db {:code/id pin-code
                                         :code/genome-id other-genome
                                         :code/resolution-id fake-resolution
                                         :abi {}})
        (let [f (failure-of (pin-session pin-code nil nil))]
          (is (= :hydrate/pin-mismatch (:error/type f)))
          (is (= :code-image-mismatch (:reason f))))))))

;; ============================================================================
;; lease hydration — an unparseable row must be DROPPED, never repaired
;; ============================================================================

(defn- insert-raw-capability!
  "Insert a `capabilities` row verbatim, bypassing insert-capability! so a
  legacy-shaped row (non-UUID id, NULL lease_edn, junk timestamps) can be
  reproduced exactly."
  [db row]
  (sqlite/with-db [conn db]
    (jdbc/insert! conn :capabilities (merge
                                      {:principal_type "session"
                                       :revoked 0
                                       :created_at now
                                       :resource_kind "tool"
                                       :resource_edn (pr-str {:kind :tool :id "fixture/echo"})
                                       :resource_id "fixture/echo"
                                       :actions "[\"invoke\"]"
                                       :constraints "{}"}
                                      row)))
  row)

(defn- load-leases
  "The private lease loader, reached by name (it is internal to
  hydrate and has no public entry point)."
  [db sid]
  ((deref (resolve 'evoclj.runtime.hydrate/load-persisted-leases)) db sid))

(deftest unparseable-lease-rows-are-dropped-not-repaired
  (let [db (fresh-db)
        sid (str (UUID/randomUUID))
        good-id (str (UUID/randomUUID))]
    (insert-raw-capability! db {:id good-id
                                :principal_id sid
                                :issued_at "2025-01-01T00:00:00Z"
                                :expires_at "2099-01-01T00:00:00Z"})
    (testing "a well-formed row still hydrates"
      (is (= 1 (count (load-leases db sid)))))
    (testing "a legacy row (non-UUID id, NULL lease_edn, unparseable
              timestamps) is dropped, NOT repaired into a live grant"
      (insert-raw-capability! db {:id "cap-legacy"
                                  :principal_id sid
                                  :lease_edn nil
                                  ;; lexically ordered so the table's
                                  ;; CHECK (expires_at > issued_at) passes,
                                  ;; yet neither parses as an Instant
                                  :issued_at "0000-not-a-timestamp"
                                  :expires_at "9999-not-a-timestamp"})
      (let [leases (load-leases db sid)]
        (is (= 1 (count leases))
            "exactly the good lease survives — the bad row is dropped, and
             the whole collection is NOT discarded either")
        (is (= good-id (str (:cap/id (first leases))))
            "the surviving lease is the real one, not a fabricated id")
        (is (every? #(not= "cap-legacy" (str (:cap/id %))) leases)
            "no lease was minted for the unreadable row")))))