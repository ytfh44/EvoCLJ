(ns evoclj.store.event-test
  "Task 5.3 tests for the append-only causal event log.

  Step 1: the per-session :event/seq is monotonic and allocated inside
  a transaction — sequential appends yield contiguous 1..n sequences,
  and concurrent-ish appends from two threads (SQLite serializes
  writers) never interleave or duplicate them. Step 2: a cause must
  reference an EARLIER event in the SAME session; root events
  (:session/created) are exempt and carry a nil cause. Step 3: the
  event namespace exposes no update/delete API — only append/read/
  verify functions. Step 4: query by session/sequence/type. Step 5:
  the hash-chain fields make tampering detectable — copying historical
  rows into a fresh database with a modified :event/type,
  :payload-ref, or :prev-hash fails verify-event-chain.

  Fresh temp databases are migrated from the classpath migrations and
  deleted after every test."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.security.redact :as redact]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private gen "generation-1")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private resolution "resolution-1")

(def ^:private db-paths (atom []))

(defn- temp-db-path
  "A throwaway SQLite file in the system temp dir."
  []
  (let [p (str (java.nio.file.Files/createTempFile
                "evoclj-event-" ".db"
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

(defn- seed-session!
  "Insert a generation row (once) and a session row; returns the
  session id (a #uuid). When `sid` is supplied, that exact session id
  is used instead of a fresh one (needed when copying historical rows
  into a second database)."
  ([db] (seed-session! db (random-uuid)))
  ([db sid]
   (sqlite/with-db [conn db]
     (when-not (first (jdbc/query conn ["SELECT id FROM generations WHERE id = ?" gen]))
       (jdbc/insert! conn :generations
                     {:id gen
                      :genome_id genome
                      :resolution_id resolution
                      :parent_id nil
                      :state "active"
                      :current 0
                      :created_at now}))
     (jdbc/insert! conn :sessions
                   {:id (str sid)
                    :generation_id gen
                    :genome_id genome
                    :resolution_id resolution
                    :phenotype_id phenotype
                    :state "created"
                    :created_at now})
     sid)))

(defn- base-event
  "An append-event! request skeleton; callers override :event/type and
  supply a real :cause/event-id for non-root events."
  [sid & [overrides]]
  (merge {:session/id sid
          :generation/id gen
          :phenotype/id phenotype
          :event/type :intent/proposed
          :cause/event-id nil
          :payload-ref nil
          :metadata {:source :event-test}}
         overrides))

(defn- event-error
  "The ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- insert-row!
  "Insert a raw events row (DB column map) into db, letting
  AUTOINCREMENT assign the id. FK-safe when rows are inserted in
  causal order (a cause always references an already-inserted, lower
  id)."
  [db row]
  (sqlite/exec! db
                ["INSERT INTO events
                    (session_id, event_seq, generation_id, phenotype_id,
                     event_type, cause_event_id, payload_ref, payload,
                     prev_hash, event_hash, created_at)
                  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                 (:session_id row) (:event_seq row) (:generation_id row)
                 (:phenotype_id row) (:event_type row) (:cause_event_id row)
                 (:payload_ref row) (:payload row) (:prev_hash row)
                 (:event_hash row) (:created_at row)]))

;; ============================================================================
;; Step 1 — per-session monotonic sequence allocation inside a transaction
;; ============================================================================

(deftest per-session-seq-is-monotonic-and-contiguous
  (let [db (fresh-db)
        sid (seed-session! db)
        e1 (event/append-event! db (base-event sid {:event/type :session/created}))
        e2 (event/append-event! db (base-event sid {:cause/event-id (:event/id e1)}))
        e3 (event/append-event! db (base-event sid {:cause/event-id (:event/id e2)}))]
    (is (= [1 2 3] (mapv :event/seq [e1 e2 e3])))
    (is (< (:event/seq e1) (:event/seq e2) (:event/seq e3)))
    (testing "a fresh session restarts its sequence at 1"
      (let [sid2 (seed-session! db)
            r (event/append-event! db (base-event sid2 {:event/type :session/created}))]
        (is (= 1 (:event/seq r)))))))

(deftest concurrent-appends-never-interleave-sequences
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))
        cause (:event/id root)
        per-thread 25
        worker (fn []
                 (dotimes [_ per-thread]
                   (event/append-event! db (base-event sid {:cause/event-id cause}))))
        t1 (future (worker))
        t2 (future (worker))]
    @t1
    @t2
    (let [events (event/events-for-session db sid)
          seqs (mapv :event/seq events)
          expected (range 1 (inc (inc (* 2 per-thread))))]
      (testing "1 root + 50 worker events = 51 unique contiguous seqs"
        (is (= (count expected) (count seqs)))
        (is (= (set expected) (set seqs)))
        (is (= (sort seqs) seqs))
        (is (= expected seqs)))
      (testing "the chain still verifies after concurrent appends"
        (is (= {:valid? true :events 51} (event/verify-event-chain db sid)))))))

;; ============================================================================
;; Step 2 — cause references: earlier same-session only; root events exempt
;; ============================================================================

(deftest root-event-is-exempt-from-cause-rule
  (let [db (fresh-db)
        sid (seed-session! db)
        e (event/append-event! db (base-event sid {:event/type :session/created}))]
    (is (= 1 (:event/seq e)))
    (is (nil? (:cause/event-id e)))
    (is (nil? (:prev-hash e)))))

(deftest non-root-event-requires-a-cause
  (let [db (fresh-db)
        sid (seed-session! db)]
    (event/append-event! db (base-event sid {:event/type :session/created}))
    (let [e (event-error #(event/append-event! db (base-event sid)))]
      (is (some? e))
      (is (= :store/event-invalid (:error/type (ex-data e)))))))

(deftest root-event-with-a-cause-rejected
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))]
    (let [e (event-error #(event/append-event! db
                                               (base-event sid {:event/type :session/created
                                                                :cause/event-id (:event/id root)})))]
      (is (some? e))
      (is (= :store/event-invalid (:error/type (ex-data e)))))))

(deftest earlier-same-session-cause-is-accepted
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))
        e2 (event/append-event! db (base-event sid {:cause/event-id (:event/id root)}))
        e3 (event/append-event! db (base-event sid {:cause/event-id (:event/id e2)}))]
    (is (= (:event/id root) (:cause/event-id e2)))
    (is (= (:event/id e2) (:cause/event-id e3)))
    (testing "a cause may reference any earlier event, not only the previous one"
      (let [e4 (event/append-event! db (base-event sid {:cause/event-id (:event/id root)}))]
        (is (= (:event/id root) (:cause/event-id e4)))))))

(deftest cause-must-reference-an-existing-event
  (let [db (fresh-db)
        sid (seed-session! db)]
    (event/append-event! db (base-event sid {:event/type :session/created}))
    ;; a "later" event cannot be referenced: it does not exist yet, so
    ;; any nonexistent id (past or future) is rejected
    (let [e (event-error #(event/append-event! db (base-event sid {:cause/event-id 99999})))]
      (is (some? e))
      (is (= :store/cause-not-found (:error/type (ex-data e)))))))

(deftest cause-must-be-in-the-same-session
  (let [db (fresh-db)
        sid1 (seed-session! db)
        sid2 (seed-session! db)
        root2 (event/append-event! db (base-event sid2 {:event/type :session/created}))]
    (event/append-event! db (base-event sid1 {:event/type :session/created}))
    (let [e (event-error #(event/append-event! db
                                               (base-event sid1 {:cause/event-id (:event/id root2)})))]
      (is (some? e))
      (is (= :store/cause-session-mismatch (:error/type (ex-data e)))))))

(deftest unknown-session-rejected
  (let [db (fresh-db)]
    (let [e (event-error #(event/append-event! db (base-event (random-uuid))))]
      (is (some? e))
      (is (= :store/session-not-found (:error/type (ex-data e)))))))

(deftest event-generation-must-match-session-pin
  (let [db (fresh-db)
        sid (seed-session! db)]
    (let [e (event-error #(event/append-event! db
                                               (base-event sid {:generation/id "other-generation"})))]
      (is (some? e))
      (is (= :store/event-invalid (:error/type (ex-data e)))))))

(deftest unknown-append-keys-rejected
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))]
    (let [e (event-error #(event/append-event! db
                                               (assoc (base-event sid {:cause/event-id (:event/id root)})
                                                      :bogus 1)))]
      (is (some? e))
      (is (= :store/event-invalid (:error/type (ex-data e)))))))

;; ============================================================================
;; Step 3 — no update/delete API exists in the event namespace
;; ============================================================================

(deftest no-update-or-delete-api-exists
  (let [publics (set (map name (keys (ns-publics 'evoclj.store.event))))]
    (testing "no mutating API beyond append"
      (is (empty? (filter #(re-matches #".*(?:update!|delete!|insert!|remove!|drop!).*" %)
                          publics))))
    (testing "the append/read/verify API is present"
      (is (every? publics
                  ["append-event!" "events-for-session" "get-event-by-seq"
                   "get-event-by-id" "events-by-type" "verify-event-chain"
                   "root-event-types"])))))

;; ============================================================================
;; Step 4 — queries by session / sequence / type
;; ============================================================================

(deftest query-by-session-seq-and-type
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))
        p1 (event/append-event! db (base-event sid {:cause/event-id (:event/id root)}))
        a1 (event/append-event! db (base-event sid {:event/type :intent/authorized
                                                    :cause/event-id (:event/id p1)}))
        p2 (event/append-event! db (base-event sid {:cause/event-id (:event/id a1)}))]
    (testing "all events of the session, ascending"
      (is (= [1 2 3 4] (mapv :event/seq (event/events-for-session db sid)))))
    (testing "by sequence"
      (is (= (:event/id a1) (:event/id (event/get-event-by-seq db sid 3))))
      (is (nil? (event/get-event-by-seq db sid 99))))
    (testing "by global event id"
      (is (= (:event/id p1) (:event/id (event/get-event-by-id db (:event/id p1)))))
      (is (nil? (event/get-event-by-id db 99999))))
    (testing "by type"
      (is (= [2 4] (mapv :event/seq (event/events-by-type db sid :intent/proposed))))
      (is (= [1] (mapv :event/seq (event/events-by-type db sid :session/created))))
      (is (= [] (event/events-by-type db sid :node/started))))))

;; ============================================================================
;; Step 5 — hash-chain tamper evidence
;; ============================================================================

(deftest returned-event-satisfies-public-contract
  (let [db (fresh-db)
        sid (seed-session! db)
        e (event/append-event! db (base-event sid {:event/type :session/created
                                                   :metadata {:k :v}}))]
    (is (int? (:event/id e)))
    (is (= 1 (:event/seq e)))
    (is (uuid? (:session/id e)))
    (is (= gen (:generation/id e)))
    (is (= phenotype (:phenotype/id e)))
    (is (= :session/created (:event/type e)))
    (is (nil? (:cause/event-id e)))
    (is (nil? (:payload-ref e)))
    (is (nil? (:prev-hash e)))
    (is (re-matches #"^sha256:[0-9a-f]{64}$" (:event-hash e)))
    (is (instance? java.util.Date (:created-at e)))
    (is (= {:k :v} (:metadata e)))))

(deftest prev-hash-links-the-chain
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))
        e2 (event/append-event! db (base-event sid {:cause/event-id (:event/id root)}))
        e3 (event/append-event! db (base-event sid {:cause/event-id (:event/id e2)}))]
    (is (nil? (:prev-hash root)))
    (is (= (:event-hash root) (:prev-hash e2)))
    (is (= (:event-hash e2) (:prev-hash e3)))
    (is (re-matches #"^sha256:[0-9a-f]{64}$" (:event-hash e3)))))

(deftest verify-passes-an-untampered-chain
  (let [db (fresh-db)
        sid (seed-session! db)]
    (is (= {:valid? true :events 0} (event/verify-event-chain db sid)))
    (event/append-event! db (base-event sid {:event/type :session/created}))
    (dotimes [_ 5]
      (let [evs (event/events-for-session db sid)
            cause (:event/id (last evs))]
        (event/append-event! db (base-event sid {:cause/event-id cause}))))
    (is (= {:valid? true :events 6} (event/verify-event-chain db sid)))))

(deftest tampered-copied-row-fails-verification
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))
        ev2 (event/append-event! db (base-event sid {:cause/event-id (:event/id root)}))
        ev3 (event/append-event! db (base-event sid {:cause/event-id (:event/id ev2)}))
        rows (sqlite/query db ["SELECT * FROM events ORDER BY event_seq"])]
    (is (= 3 (count rows)))
    (testing "an untampered copy verifies"
      (let [db2 (fresh-db)
            _ (seed-session! db2 sid)
            _ (doseq [r rows] (insert-row! db2 r))]
        (is (= {:valid? true :events 3} (event/verify-event-chain db2 sid)))))
    (testing "changing :event/type in a copied row breaks the hash"
      (let [db2 (fresh-db)
            _ (seed-session! db2 sid)
            tampered (mapv #(if (= (:event_seq %) (:event/seq ev2))
                              (assoc % :event_type ":node/started")
                              %)
                           rows)]
        (doseq [r tampered] (insert-row! db2 r))
        (let [v (event/verify-event-chain db2 sid)]
          (is (false? (:valid? v)))
          (is (= :event/hash-mismatch (:reason v)))
          (is (= (:event/seq ev2) (:event/seq v))))))
    (testing "changing :payload-ref in a copied row breaks the hash"
      (let [db2 (fresh-db)
            _ (seed-session! db2 sid)
            tampered (mapv #(if (= (:event_seq %) (:event/seq ev2))
                              (assoc % :payload_ref (str "sha256:" (apply str (repeat 64 "c"))))
                              %)
                           rows)]
        (doseq [r tampered] (insert-row! db2 r))
        (let [v (event/verify-event-chain db2 sid)]
          (is (false? (:valid? v)))
          (is (= :event/hash-mismatch (:reason v))))))
    (testing "breaking the prev-hash linkage fails verification"
      (let [db2 (fresh-db)
            _ (seed-session! db2 sid)
            tampered (mapv #(if (= (:event_seq %) (:event/seq ev3))
                              (assoc % :prev_hash (str "sha256:" (apply str (repeat 64 "d"))))
                              %)
                           rows)]
        (doseq [r tampered] (insert-row! db2 r))
        (let [v (event/verify-event-chain db2 sid)]
          (is (false? (:valid? v)))
          (is (= :event/prev-hash-mismatch (:reason v)))
          (is (= (:event/seq ev3) (:event/seq v))))))))

(deftest same-leaf-cross-namespace-type-swap-fails-verification
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))
        ev2 (event/append-event! db (base-event sid {:event/type :intent/completed
                                                     :cause/event-id (:event/id root)}))
        ev3 (event/append-event! db (base-event sid {:cause/event-id (:event/id ev2)}))
        rows (sqlite/query db ["SELECT * FROM events ORDER BY event_seq"])]
    (is (= 3 (count rows)))
    (testing "same-leaf cross-namespace swap (:intent/completed -> :node/completed) breaks the hash"
      (let [db2 (fresh-db)
            _ (seed-session! db2 sid)
            tampered (mapv #(if (= (:event_seq %) (:event/seq ev2))
                              (assoc % :event_type ":node/completed")
                              %)
                           rows)]
        (doseq [r tampered] (insert-row! db2 r))
        (let [v (event/verify-event-chain db2 sid)]
          (is (false? (:valid? v)))
          (is (= :event/hash-mismatch (:reason v)))
          (is (= (:event/seq ev2) (:event/seq v))))))))

;; ============================================================================
;; Task A1 — redaction on the event write path (F7)
;; ============================================================================

(def ^:private bearer-spec
  "A :pattern redaction spec matching Authorization bearer tokens."
  {:redact/kind :pattern
   :redact/pattern #"Bearer\s+[A-Za-z0-9._~+/=-]+"})

(def ^:private api-key-spec
  "A :key-path redaction spec replacing the api key under :credentials."
  {:redact/kind :key-path
   :redact/paths [[:credentials :api-key]]})

(deftest append-with-specs-redacts-payload-and-chain-verifies
  (let [db (fresh-db)
        sid (seed-session! db)
        specs [bearer-spec api-key-spec]
        root (event/append-event!
              db
              (base-event sid
                          {:event/type :session/created
                           :metadata {:source :event-test
                                      :auth {:header (str "Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.super-secret-token")}
                                      :credentials {:api-key "sk-live-abc123"}
                                      :public 1}})
              specs)
        ev2 (event/append-event!
             db
             (base-event sid
                         {:cause/event-id (:event/id root)
                          :metadata {:credentials {:api-key "sk-live-abc123"}}})
             specs)]
    (testing "pattern redaction replaced the embedded bearer token"
      (is (not (re-find #"super-secret-token" (pr-str (:metadata root)))))
      (is (re-find #"\[REDACTED\]" (pr-str (:metadata root)))))
    (testing "key-path redaction replaced the api key in persisted metadata"
      (is (= "[REDACTED]" (get-in (:metadata root) [:credentials :api-key])))
      (is (= "[REDACTED]" (get-in (:metadata ev2) [:credentials :api-key]))))
    (testing "non-secret values pass through untouched"
      (is (= 1 (:public (:metadata root))))
      (is (= :event-test (:source (:metadata root)))))
    (testing "the hash chain stays valid after redacted appends"
      (is (= {:valid? true :events 2} (event/verify-event-chain db sid))))))

(deftest redaction-is-idempotent
  (let [db (fresh-db)
        sid (seed-session! db)
        specs [bearer-spec api-key-spec]
        ev (event/append-event!
            db
            (base-event sid
                        {:event/type :session/created
                         :metadata {:auth {:token "Bearer tok-secret-123"}
                                    :credentials {:api-key "sk-live-abc123"}}})
            specs)]
    (testing "re-redacting the persisted event leaves its metadata unchanged"
      (is (= (:metadata ev)
             (:metadata (redact/redact-event ev specs)))))
    (testing "a marker already present before append survives redaction"
      (let [ev2 (event/append-event!
                 db
                 (base-event sid
                             {:cause/event-id (:event/id ev)
                              :metadata {:already "[REDACTED]"
                                         :credentials {:api-key "sk-live-xyz"}}})
                 specs)]
        (is (= "[REDACTED]" (:already (:metadata ev2))))
        (is (= "[REDACTED]" (get-in (:metadata ev2) [:credentials :api-key])))))))

(deftest invalid-redaction-specs-throw-before-any-write
  (let [db (fresh-db)
        sid (seed-session! db)
        e (event-error #(event/append-event!
                         db
                         (base-event sid {:event/type :session/created})
                         :not-sequential))]
    (is (some? e))
    (is (= :security/redact-invalid (:error/type (ex-data e))))
    (is (= :not-sequential (:reason (ex-data e)))))
  (testing "a spec violating the closed RedactSpecSchema is rejected"
    (let [db (fresh-db)
          sid (seed-session! db)
          e (event-error #(event/append-event!
                           db
                           (base-event sid {:event/type :session/created})
                           [{}]))]
      (is (some? e))
      (is (= :security/redact-invalid (:error/type (ex-data e))))
      (is (= :spec-invalid (:reason (ex-data e))))
      (testing "the failed append left no row and consumed no sequence"
        (is (= [] (event/events-for-session db sid)))))))

(deftest no-specs-path-is-unchanged
  (let [db (fresh-db)
        sid (seed-session! db)
        metadata {:source :event-test
                  :credentials {:api-key "sk-live-abc123"}
                  :auth {:header "Authorization: Bearer tok-secret-123"}}
        e (event/append-event! db (base-event sid {:event/type :session/created
                                                   :metadata metadata}))]
    (testing "the two-arity call stores metadata verbatim, byte for byte"
      (is (= metadata (:metadata e)))
      (is (= (pr-str metadata)
             (:payload (first (sqlite/query db ["SELECT payload FROM events WHERE id = ?"
                                                (:event/id e)]))))))
    (testing "an explicit nil specs arity behaves identically"
      (let [ev2 (event/append-event!
                 db
                 (base-event sid {:cause/event-id (:event/id e)
                                  :metadata metadata})
                 nil)]
        (is (= metadata (:metadata ev2)))
        (is (= 2 (:event/seq ev2)))))
    (testing "the chain verifies and hashes keep the canonical form"
      (is (= {:valid? true :events 2} (event/verify-event-chain db sid)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:event-hash e))))))
