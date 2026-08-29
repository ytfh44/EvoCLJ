(ns evoclj.store.session-test
  "component tests for session pinning and lifecycle transitions.

  Step 1: a session records immutable Genome/Resolution/Phenotype ids
  at creation. Step 2: illegal state transitions fail with the typed
  error :session/invalid-transition. Step 3: no update operation can
  change the pinned ids — the only write path is the compare-and-set
  state transition and it touches state alone. Step 4: the transition
  is a compare-and-set UPDATE, so two workers racing from the same
  state produce exactly one winner and one :session/invalid-transition.

  Fresh temp databases are migrated from the classpath migrations and
  deleted after every test."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private gen "generation-1")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))

(def ^:private db-paths (atom []))

(defn- temp-db-path
  "A throwaway SQLite file in the system temp dir."
  []
  (let [p (str (java.nio.file.Files/createTempFile
                "evoclj-session-" ".db"
                (make-array java.nio.file.attribute.FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- cleanup!
  "Delete every temp db file created during this run."
  []
  (doseq [p @db-paths]
    (java.nio.file.Files/deleteIfExists
     (java.nio.file.Paths/get p (make-array String 0))))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db
  "A migrated database spec backed by a fresh temp file."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- seed-generation!
  "Insert the generation row sessions are pinned to (once per db)."
  [db]
  (sqlite/with-db [conn db]
    ;; Fleet P5/F FK (011): generations/genome_id -> genomes -> artifacts
    (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" genome])
    (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" resolution])
    (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" phenotype])
    (jdbc/execute! conn ["INSERT OR IGNORE INTO genomes (id, created_at) VALUES (?, datetime('now'))" genome])
    (jdbc/insert! conn :generations
                  {:id gen
                   :genome_id genome
                   :resolution_id resolution
                   :parent_id nil
                   :state "active"
                   :current 0
                   :created_at now})))

(defn- session-request
  "A valid create-session! request; callers merge overrides."
  [& [overrides]]
  (merge {:genome/id genome
          :resolution/id resolution
          :phenotype/id phenotype
          :generation/id gen}
         overrides))

(defn- tx-error
  "The ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- deref-unwrap
  "Deref a future, rethrowing the cause of an ExecutionException so
  worker failures surface with their real type."
  [f]
  (try @f
       (catch java.util.concurrent.ExecutionException e
         (throw (or (.getCause e) e)))))

;; ============================================================================
;; Step 1 — a session records immutable pinned ids at creation
;; ============================================================================

(deftest session-pins-identity-at-creation
  (let [db (fresh-db)
        _ (seed-generation! db)
        s (session/create-session! db (session-request))
        sid (:session/id s)]
    (testing "the returned value is the full public Session map"
      (is (= s (session/get-session db sid))))
    (testing "pinned identity fields match the request"
      (is (uuid? sid))
      (is (= genome (:genome/id s)))
      (is (= resolution (:resolution/id s)))
      (is (= phenotype (:phenotype/id s)))
      (is (= gen (:generation/id s)))
      (is (= :created (:state s)))
      (is (instance? java.util.Date (:created-at s))))
    (testing "the pinned ids never change across every transition"
      (session/transition-session! db sid :created :resolving {})
      (session/transition-session! db sid :resolving :running {})
      (session/transition-session! db sid :running :waiting {})
      (let [s2 (session/get-session db sid)]
        (is (= [genome resolution phenotype gen]
               [(:genome/id s2) (:resolution/id s2)
                (:phenotype/id s2) (:generation/id s2)]))
        (is (= :waiting (:state s2)))))))

(deftest get-session-returns-nil-for-an-unknown-session
  (let [db (fresh-db)]
    (is (nil? (session/get-session db (random-uuid))))))

;; ============================================================================
;; Step 2 — illegal state transitions fail with :session/invalid-transition
;; ============================================================================

(deftest illegal-transitions-fail
  (let [db (fresh-db)
        _ (seed-generation! db)
        sid (:session/id (session/create-session! db (session-request)))
        invalid? (fn [expected new]
                   (= :session/invalid-transition
                      (-> (tx-error #(session/transition-session! db sid expected new {}))
                          ex-data :error/type)))]
    (testing "a state cannot be skipped"
      (is (invalid? :created :running)))
    (testing "a state cannot go backwards"
      (is (invalid? :resolving :created)))
    (testing "a self-transition is not an edge of the state machine"
      (is (invalid? :running :running)))
    (testing ":running cannot complete directly; :completed is reached via :waiting"
      (is (invalid? :running :completed)))
    (testing "a session cannot be created into a later state"
      (is (invalid? :created :completed))
      (is (invalid? :created :failed)))
    (testing "an unknown state is not a valid source or target"
      (is (invalid? :bogus :running))
      (is (invalid? :running :bogus)))
    (testing "an illegal transition leaves the stored state unchanged"
      (is (invalid? :created :running))
      (is (= :created (:state (session/get-session db sid)))))))

(deftest terminal-states-accept-nothing
  (let [db (fresh-db)
        _ (seed-generation! db)
        to-terminal (fn [terminal]
                      (let [sid (:session/id (session/create-session! db (session-request)))]
                        (session/transition-session! db sid :created :resolving {})
                        (session/transition-session! db sid :resolving :running {})
                        (is (= terminal (:state (session/transition-session!
                                                 db sid :running terminal {:reason terminal}))))
                        sid))]
    (doseq [t [:failed :cancelled :budget-exhausted]]
      (testing (str "terminal state " t " accepts no further transitions")
        (let [sid (to-terminal t)]
          (is (= :session/invalid-transition
                 (-> (tx-error #(session/transition-session! db sid t :running {}))
                     ex-data :error/type))))))
    (testing "a completed session accepts no further transitions"
      (let [sid (:session/id (session/create-session! db (session-request)))]
        (session/transition-session! db sid :created :resolving {})
        (session/transition-session! db sid :resolving :running {})
        (session/transition-session! db sid :running :waiting {})
        (is (= :completed (:state (session/transition-session!
                                   db sid :waiting :completed {:score 1.0}))))
        (is (= :session/invalid-transition
               (-> (tx-error #(session/transition-session! db sid :completed :waiting {}))
                   ex-data :error/type)))))))

(deftest transition-on-an-unknown-session-is-rejected
  (let [db (fresh-db)
        _ (seed-generation! db)]
    (is (= :store/session-not-found
           (-> (tx-error #(session/transition-session! db (random-uuid) :created :resolving {}))
               ex-data :error/type)))))

;; ============================================================================
;; Step 3 — no update operation can change the pinned ids
;; ============================================================================

(deftest no-update-api-can-change-pinned-ids
  (let [db (fresh-db)
        _ (seed-generation! db)
        sid (:session/id (session/create-session! db (session-request)))
        publics (set (map name (keys (ns-publics 'evoclj.store.session))))]
    (testing "the documented public API is present"
      (is (every? publics ["create-session!" "transition-session!" "get-session"])))
    (testing "no update/delete-style API exists to rewrite identity"
      (is (empty? (filter #(re-matches #".*(?:update!|delete!|insert!|remove!|drop!).*" %)
                          publics))))
    (testing "transitions never disturb the pinned ids"
      (session/transition-session! db sid :created :resolving {})
      (session/transition-session! db sid :resolving :running {})
      (let [s (session/get-session db sid)]
        (is (= [genome resolution phenotype]
               [(:genome/id s) (:resolution/id s) (:phenotype/id s)]))))))

;; ============================================================================
;; Step 4 — compare-and-set: concurrent workers, exactly one wins
;; ============================================================================

(deftest concurrent-cas-exactly-one-winner
  (let [db (fresh-db)
        _ (seed-generation! db)
        sid (:session/id (session/create-session! db (session-request)))
        gate (java.util.concurrent.CountDownLatch. 1)
        worker (fn []
                 (.await gate)
                 (try
                   (session/transition-session! db sid :created :resolving
                                                {:worker (str (Thread/currentThread))})
                   :won
                   (catch clojure.lang.ExceptionInfo e
                     (if (= :session/invalid-transition (:error/type (ex-data e)))
                       :lost
                       (throw e)))))
        t1 (future (worker))
        t2 (future (worker))]
    (.countDown gate)
    (let [outcomes (sort [(deref-unwrap t1) (deref-unwrap t2)])]
      (testing "exactly one worker wins; the loser sees an invalid transition"
        (is (= [:lost :won] outcomes)))
      (testing "the session ends in exactly the target state"
        (is (= :resolving (:state (session/get-session db sid))))))))

;; ============================================================================
;; The full state machine (normative diagram)
;; ============================================================================

(deftest full-state-machine-walk
  (let [db (fresh-db)
        _ (seed-generation! db)
        sid (:session/id (session/create-session! db (session-request)))]
    (testing ":created → :resolving → :running ↔ :waiting → :completed"
      (is (= :resolving (:state (session/transition-session! db sid :created :resolving {}))))
      (is (= :running (:state (session/transition-session! db sid :resolving :running {}))))
      (is (= :waiting (:state (session/transition-session! db sid :running :waiting {}))))
      (is (= :running (:state (session/transition-session! db sid :waiting :running {}))))
      (is (= :waiting (:state (session/transition-session! db sid :running :waiting {}))))
      (is (= :completed (:state (session/transition-session!
                                 db sid :waiting :completed {:score 1.0})))))))

;; ============================================================================
;; Input validation at the module boundary
;; ============================================================================

(deftest create-request-is-validated
  (let [db (fresh-db)
        _ (seed-generation! db)
        invalid? (fn [req]
                   (= :store/session-invalid
                      (-> (tx-error #(session/create-session! db req))
                          ex-data :error/type)))]
    (testing "unknown keys are rejected (closed trust boundary)"
      (is (invalid? (assoc (session-request) :bogus 1))))
    (testing "a malformed genome id is rejected"
      (is (invalid? (assoc (session-request) :genome/id "not-a-hash"))))
    (testing "a malformed resolution id is rejected"
      (is (invalid? (assoc (session-request) :resolution/id "not-a-hash"))))
    (testing "a malformed phenotype id is rejected"
      (is (invalid? (assoc (session-request) :phenotype/id "not-a-hash"))))
    (testing "a missing generation id is rejected"
      (is (invalid? (dissoc (session-request) :generation/id))))
    (testing "a malformed routing map is rejected"
      (is (invalid? (session-request {:routing {:bucket "not-an-int"}}))))
    (testing "an unknown generation fails loudly"
      (is (= :store/generation-not-found
             (-> (tx-error #(session/create-session! db (session-request {:generation/id "no-such-generation"})))
                 ex-data :error/type))))))

(deftest transition-data-must-be-edn-safe
  (let [db (fresh-db)
        _ (seed-generation! db)
        sid (:session/id (session/create-session! db (session-request)))]
    (testing "a function in the data payload is rejected"
      (is (= :store/session-invalid
             (-> (tx-error #(session/transition-session! db sid :created :resolving {:bad (fn [] 1)}))
                 ex-data :error/type))))
    (testing "nil and plain maps are accepted"
      (is (= :resolving (:state (session/transition-session! db sid :created :resolving nil)))))))

(deftest routing-is-persisted-with-the-allocation-version
  ;; component (additive migration 003-routing.sql): the :routing map
  ;; {:deployment-version ... :bucket ...} that decided the session's
  ;; generation is written at insert and read back by get-session, so
  ;; routing can be audited later. (component validated but did not
  ;; persist :routing — the schema had no columns and migrations were
  ;; out of scope; 003-routing.sql closed that gap.)
  (let [db (fresh-db)
        _ (seed-generation! db)
        s (session/create-session! db (session-request
                                       {:routing {:deployment-version "v1" :bucket 7}}))
        sid (:session/id s)]
    (testing "the routing decision round-trips through the store"
      (is (= {:deployment-version "v1" :bucket 7} (:routing s)))
      (is (= (:routing s) (:routing (session/get-session db sid)))))
    (testing "the routing decision survives every state transition"
      (session/transition-session! db sid :created :resolving {})
      (session/transition-session! db sid :resolving :running {})
      (is (= {:deployment-version "v1" :bucket 7}
             (:routing (session/get-session db sid)))))
    (testing "sessions created without routing keep a nil :routing"
      (let [s2 (session/create-session! db (session-request))]
        (is (nil? (:routing s2)))))))