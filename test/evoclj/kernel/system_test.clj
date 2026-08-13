(ns evoclj.kernel.system-test
  "Task 10.1 — host wiring tests.

  The tests build the host config map DIRECTLY with temp paths
  (dependency injection, Step 4): nothing here reads
  resources/system.edn, reads environment variables, or touches the
  repository's real stores. The config shape mirrors
  resources/system.edn 1:1 (same keys, same #ig/ref wiring), so what
  is exercised here is exactly what a host startup would build —
  only with paths under a throwaway temp root."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [evoclj.evolution.core :as evolution]
            [evoclj.evolution.diagnose :as diagnose]
            [evoclj.genome.types :as types]
            [evoclj.kernel.system :as sys]
            [evoclj.provider.registry :as registry]
            [evoclj.store.cas :as cas]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]
            [integrant.core :as ig])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.nio.charset StandardCharsets)))

;; --- temp helpers -----------------------------------------------------------------------------------------------------------------------

(defn- temp-dir
  "A fresh throwaway directory; the host never touches real stores."
  []
  (Files/createTempDirectory "evoclj-host-" (make-array FileAttribute 0)))

(defn- resolve-path
  "Join a temp root with a relative name and return an absolute string
  (the same normalization the host applies to relative config paths)."
  [root name]
  (str (.resolve root name)))

(def ^:private sha256-id
  "A format-valid sha256:<64 hex> id for seeded rows."
  (str "sha256:" (apply str (repeat 64 "7"))))

(def ^:private provider-catalog
  "The v0 fixture provider catalog (Task 2.1 Resolution shape)."
  {:reasoning/high {:provider :fixture
                    :provider-model "fixture-model-v1"
                    :adapter-version "1"}
   :reasoning/low {:provider :fixture
                   :provider-model "fixture-model-low"
                   :adapter-version "1"}
   :fast {:provider :fixture
          :provider-model "fixture-model-fast"
          :adapter-version "1"}})

(def ^:private default-profile
  "The normative :default-v1 eval profile as plain data (the host
  config carries profiles as data, never as code)."
  {:eval/profile-id :default-v1
   :evolution-set {:source :evals/evolution}
   :selection-set {:source :evals/selection :visibility :kernel-only}
   :audit-set {:source :evals/audit :visibility :operator-only}
   :repetitions 1
   :promotion {:strategy :paired-comparison
               :min-delta 0.05
               :max-cost-regression 1.10
               :max-complexity-regression 1.25}})

(defn- config-for
  "The host config map (mirrors resources/system.edn) with every path
  under a fresh temp `root`. The :promotion/system ids are injected
  here rather than derived, so the test never compiles the seed
  Genome (Step 4: constructors stay dependency-injected)."
  [root]
  (let [db (resolve-path root "evoclj.db")
        cas-root (resolve-path root "cas")
        seed (resolve-path root "seed")]
    {:store/sqlite db
     :store/cas {:root cas-root :verify false}
     :provider/registry
       {:providers [{:provider/type :fixture/echo}
                    {:provider/type :fixture/non-idempotent}]}
     :capability/broker
       {:registry (ig/ref :provider/registry)
        :leases []}
     :runtime/executor
       {:scheduler {:max-steps 1000}
        :store {:sqlite (ig/ref :store/sqlite)
                :cas (ig/ref :store/cas)}
        :dispatch (ig/ref :capability/broker)}
     :evolution/system
       {:store {:sqlite (ig/ref :store/sqlite)
                :cas (ig/ref :store/cas)}
        :provider-catalog provider-catalog
        :genome-root seed
        :candidates-dir (resolve-path root "candidates")
        :diagnostician {:task/success-threshold 1.0
                        :max-hypotheses 3
                        :confidence-band :medium}
        :mutator :none
        :budget-profile {:max-candidates 3}
        :programs-registry []}
     :eval/system
       {:store {:sqlite (ig/ref :store/sqlite)
                :cas (ig/ref :store/cas)}
        :provider/catalog provider-catalog
        :kernel/abi {:kernel 1 :genome 1 :intent 1 :tool 1}
        :profiles {"default-v1" default-profile}
        :genome/roots {"generation-1" seed}
        :dataset/roots {:evals/evolution (resolve-path root "evals-evolution")
                        :evals/selection (resolve-path root "evals-selection")
                        :evals/audit (resolve-path root "evals-audit")}
        :selection/cases {}
        :selection/fixtures {}
        :replay/cases {}
        :replay/fixtures {}}
     :promotion/system
       {:store {:sqlite (ig/ref :store/sqlite)
                :cas (ig/ref :store/cas)}
        :resolution/id sha256-id
        :event/session-id (str (random-uuid))}}))

(defn- seed-generation!
  "Insert the generation row sessions are pinned to (current = 1: the
  seed generation IS the CURRENT pointer, Database Invariant 6),
  mirroring the Task 6.3 e2e fixture. Returns the generation id."
  [db]
  (sqlite/exec! db ["INSERT INTO generations
                      (id, genome_id, resolution_id, parent_id, state, current, created_at)
                    VALUES ('generation-1', ?, ?, NULL, 'active', 1,
                            '2025-01-01T00:00:00Z')"
                    sha256-id sha256-id])
  "generation-1")

(defn- create-session!
  "Create a :created session pinned to the seeded generation via the
  public store API and return its persisted session map."
  [db]
  (session/create-session! db
                           {:generation/id "generation-1"
                            :genome/id sha256-id
                            :resolution/id sha256-id
                            :phenotype/id sha256-id}))

;; ============================================================================
;; Step 1 — init/halt with temporary DB/CAS
;; ============================================================================

(deftest host-inits-and-halts-with-temp-stores
  (let [system (sys/init (config-for (temp-dir)))]
    (testing "all eight Integrant-owned host components are present"
      (is (= #{:store/sqlite :store/cas :provider/registry
               :capability/broker :runtime/executor
               :evolution/system :eval/system :promotion/system}
             (set (keys system)))))
    (testing ":store/sqlite is usable — schema is migrated and queryable"
      (let [db (:store/sqlite system)]
        (is (seq (sqlite/query db
                               ["SELECT name FROM sqlite_master
                                 WHERE type = 'table' AND name = 'generations'"])))))
    (testing ":store/cas is usable — bytes round-trip by content hash"
      (let [cas-config (:store/cas system)
            id (cas/put-bytes! cas-config
                               (.getBytes "hello host" StandardCharsets/UTF_8)
                               {:media-type "text/plain"})]
        (is (= "hello host"
               (String. (cas/get-bytes cas-config (:artifact/id id))
                        StandardCharsets/UTF_8)))))
    (testing ":provider/registry is usable — catalog providers are registered"
      (let [reg (:provider/registry system)]
        (is (= :fixture/echo
               (:tool/id (:descriptor (registry/lookup reg :fixture/echo)))))
        (is (= :fixture/non-idempotent
               (:tool/id (:descriptor (registry/lookup reg :fixture/non-idempotent)))))))
    (testing ":capability/broker is a broker context wired to the registry"
      (let [broker (:capability/broker system)]
        (is (= (:provider/registry system) (:registry broker)))
        (is (= [] (:leases broker)))))
    (testing ":runtime/executor exposes the scheduler and can build an
              executor map from a phenotype"
      (let [executor (:runtime/executor system)]
        (is (fn? (:scheduler executor)))
        (is (= (:store/sqlite system)
               (get-in executor [:stores :sqlite])))
        (is (= (:capability/broker system)
               (:dispatch executor)))))
    (testing ":evolution/system is a valid evolution-system map"
      (let [evo (:evolution/system system)]
        (is (= (:store/sqlite system) (get-in evo [:store :sqlite])))
        (is (satisfies? diagnose/Diagnostician (:diagnostician evo)))
        (is (satisfies? evolution/Mutator (:mutator evo)))
        (is (string? (:candidates-dir evo)))))
    (testing ":eval/system is a valid evaluator map"
      (let [evaluator (:eval/system system)]
        (is (= (:store/sqlite system) (get-in evaluator [:store :sqlite])))
        (is (map? (:profiles evaluator)))
        (is (= {:kernel 1 :genome 1 :intent 1 :tool 1}
               (:kernel/abi evaluator)))))
    (testing ":promotion/system is a valid promotion-system map"
      (let [promo (:promotion/system system)]
        (is (= (:store/sqlite system) (get-in promo [:store :sqlite])))
        (is (types/resolution-id? (:resolution/id promo)))
        (is (types/session-id? (:event/session-id promo)))))
    (testing "halt! closes cleanly and returns nil"
      (is (nil? (sys/halt! system))))))

;; ============================================================================
;; Step 2 — halt! twice is safe
;; ============================================================================

(deftest halt-twice-is-safe
  (let [system (sys/init (config-for (temp-dir)))]
    (sys/halt! system)
    (testing "a second halt! on the same component map is a no-op, not an error"
      (is (nil? (sys/halt! system))))
    (testing "halt! on an already-halted fresh init is equally safe"
      (let [again (sys/init (config-for (temp-dir)))]
        (sys/halt! again)
        (is (nil? (sys/halt! again)))))))

;; ============================================================================
;; Step 3 — reinitializing reconstructs durable state from the stores
;; ============================================================================

(deftest reinit-reconstructs-durable-state-from-stores
  (let [root (temp-dir)
        cfg (config-for root)]
    ;; system 1: init, seed a generation, create a session and a CAS
    ;; artifact, halt
    (let [system (sys/init cfg)
          db (:store/sqlite system)
          cas-config (:store/cas system)
          artifact (cas/put-bytes! cas-config
                                   (.getBytes "durable artifact"
                                              StandardCharsets/UTF_8)
                                   {:media-type "text/plain"})]
      (seed-generation! db)
      (let [created (create-session! db)
            sid (:session/id created)]
        (is (uuid? sid))
        (testing "the session is visible while system 1 is live"
          (is (= "generation-1" (:generation/id (session/get-session db sid)))))
        (sys/halt! system)
        (testing "halt! does not destroy the on-disk SQLite store"
          (is (.exists (io/file (:store/sqlite cfg)))))
        ;; system 2: REINIT on the SAME paths — durable state must
        ;; come back from the stores, never from stale in-memory
        ;; session/generation objects
        (let [system2 (sys/init cfg)
              db2 (:store/sqlite system2)
              rebuilt (session/get-session db2 sid)]
          (testing "the session row survives the restart"
            (is (some? rebuilt))
            (is (= sid (:session/id rebuilt)))
            (is (= "generation-1" (:generation/id rebuilt)))
            (is (= sha256-id (:genome/id rebuilt)))
            (is (= :created (:state rebuilt))))
          (testing "the generation row survives the restart"
            (is (= [{:current 1}]
                   (sqlite/query db2
                                 ["SELECT current FROM generations
                                   WHERE id = 'generation-1'"]))))
          (testing "the CAS artifact survives the restart"
            (is (= "durable artifact"
                   (String. (cas/get-bytes (:store/cas system2)
                                           (:artifact/id artifact))
                            StandardCharsets/UTF_8))))
          (testing "the rebuilt component values are fresh, not shared with system 1"
            (is (not (identical? (:store/sqlite system) (:store/sqlite system2))))
            (is (not (identical? (:provider/registry system)
                                 (:provider/registry system2)))))
          (sys/halt! system2))))))
