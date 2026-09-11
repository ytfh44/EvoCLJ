(ns evoclj.store.event-test
  "E1 tests for the append-only event log: prev (linear) vs causal-links (graph).

  Step 1: per-session :event/seq monotonic. Step 2: :prev/event-id must be
  the immediate predecessor in the SAME session (nil only for root
  :session/created); :causal-links may cross sessions. Step 3: no update/delete.
  Step 4: query. Step 5: hash-chain tamper detection. F7 redaction unchanged.

  Fresh temp databases are migrated and deleted after every test."
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
  []
  (let [f (java.io.File/createTempFile "evoclj-event-test-" ".sqlite")
        p (.getAbsolutePath f)]
    (.delete f)
    (swap! db-paths conj p)
    p))

(defn- cleanup!
  []
  (doseq [p @db-paths]
    (try (.delete (java.io.File. p)) (catch Exception _ nil)))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db
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
       (try (jdbc/insert! conn :artifacts {:hash genome :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
       (try (jdbc/insert! conn :artifacts {:hash resolution :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
       (try (jdbc/insert! conn :artifacts {:hash phenotype :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
       (try (jdbc/insert! conn :genomes {:id genome :created_at now}) (catch Exception _ nil))
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
  supply a real :prev/event-id for non-root events. :causal-links defaults to #{}."
  [sid & [overrides]]
  (merge {:session/id sid
          :generation/id gen
          :phenotype/id phenotype
          :event/type :intent/proposed
          :prev/event-id nil
          :causal-links #{}
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
  causal order. For E1 we insert both cause_event_id and prev_event_id
  (same value) so legacy readers stay coherent."
  [db row]
  (sqlite/exec! db
                ["INSERT INTO events
                    (session_id, event_seq, generation_id, phenotype_id,
                     event_type, cause_event_id, prev_event_id, payload_ref, payload,
                     prev_hash, event_hash, created_at)
                  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                 (:session_id row) (:event_seq row) (:generation_id row)
                 (:phenotype_id row) (:event_type row) (:cause_event_id row) (:prev_event_id row)
                 (:payload_ref row) (:payload row) (:prev_hash row)
                 (:event_hash row) (:created_at row)]))

(defn- copy-links!
  "Copy every causal_links row from src-db into db. Event ids align
  because both databases receive the copied event rows in the same order
  into fresh AUTOINCREMENT sequences."
  [src-db db]
  (doseq [link-row (sqlite/query src-db ["SELECT from_event_id, to_event_id, link_type, created_at FROM causal_links"])]
    (sqlite/exec! db ["INSERT OR IGNORE INTO causal_links (from_event_id, to_event_id, link_type, created_at) VALUES (?, ?, ?, ?)"
                      (:from_event_id link-row) (:to_event_id link-row) (:link_type link-row) (:created_at link-row)])))

;; ============================================================================
;; Step 1 — per-session monotonic sequence allocation inside a transaction
;; ============================================================================

(deftest per-session-seq-is-monotonic-and-contiguous
  (let [db (fresh-db)
        sid (seed-session! db)
        e1 (event/append-event! db (base-event sid {:event/type :session/created}))
        e2 (event/append-event! db (base-event sid {:prev/event-id (:event/id e1)}))
        e3 (event/append-event! db (base-event sid {:prev/event-id (:event/id e2)}))]
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
        per-thread 25
        ;; Under the strict predecessor rule, racing writers may read the
        ;; same latest event before either commits; the loser retries with
        ;; the fresh predecessor (compare-and-swap style). Bounded retries:
        ;; every rejection means the peer committed, so the loop converges.
        worker (fn []
                 (dotimes [_ per-thread]
                   (loop [attempt 0]
                     (let [latest (last (event/events-for-session db sid))
                           outcome (try
                                     (event/append-event! db (base-event sid {:prev/event-id (:event/id latest)}))
                                     (catch clojure.lang.ExceptionInfo rejected rejected))]
                       (when (instance? clojure.lang.ExceptionInfo outcome)
                         (if (and (= :store/prev-not-immediate (:error/type (ex-data outcome)))
                                  (< attempt 100))
                           (recur (inc attempt))
                           (throw outcome)))))))]
    (let [t1 (future (worker))
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
          (is (= {:valid? true :events 51} (event/verify-event-chain db sid))))))))

;; ============================================================================
;; Step 2 — prev references: immediate predecessor same-session; causal-links cross
;; ============================================================================

(deftest root-event-is-exempt-from-prev-rule
  (let [db (fresh-db)
        sid (seed-session! db)
        e (event/append-event! db (base-event sid {:event/type :session/created}))]
    (is (= 1 (:event/seq e)))
    (is (nil? (:prev/event-id e)))
    (is (empty? (:causal-links e)))
    (is (nil? (:prev-hash e)))))

(deftest non-root-event-requires-a-prev
  (let [db (fresh-db)
        sid (seed-session! db)]
    (event/append-event! db (base-event sid {:event/type :session/created}))
    (let [e (event-error #(event/append-event! db (base-event sid)))]
      (is (some? e))
      (is (= :store/event-invalid (:error/type (ex-data e)))))))

(deftest root-event-with-a-prev-rejected
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))]
    (let [e (event-error #(event/append-event! db
                                               (base-event sid {:event/type :session/created
                                                                :prev/event-id (:event/id root)})))]
      (is (some? e))
      (is (= :store/event-invalid (:error/type (ex-data e)))))))

(deftest forked-predecessor-append-is-rejected
  (let [db (fresh-db)
        sid (seed-session! db)
        root-event (event/append-event! db (base-event sid {:event/type :session/created}))
        middle-event (event/append-event! db (base-event sid {:prev/event-id (:event/id root-event)}))]
    (testing "an append whose prev skips the immediate predecessor is rejected at append time"
      (let [forked-attempt (event-error #(event/append-event! db (base-event sid {:prev/event-id (:event/id root-event)})))]
        (is (some? forked-attempt))
        (is (= :store/prev-not-immediate (:error/type (ex-data forked-attempt))))))
    (testing "the rejected fork writes nothing and the chain still verifies"
      (is (= [1 2] (mapv :event/seq (event/events-for-session db sid))))
      (is (= {:valid? true :events 2} (event/verify-event-chain db sid))))
    (testing "the legitimate immediate-predecessor append still passes"
      (let [closing-event (event/append-event! db (base-event sid {:prev/event-id (:event/id middle-event)}))]
        (is (= (:event/id middle-event) (:prev/event-id closing-event)))
        (is (= 3 (:event/seq closing-event)))
        (is (= {:valid? true :events 3} (event/verify-event-chain db sid)))))))

(deftest prev-must-reference-an-existing-event
  (let [db (fresh-db)
        sid (seed-session! db)]
    (event/append-event! db (base-event sid {:event/type :session/created}))
    (let [e (event-error #(event/append-event! db (base-event sid {:prev/event-id 99999})))]
      (is (some? e))
      (is (= :store/cause-not-found (:error/type (ex-data e)))))))

(deftest prev-must-be-in-the-same-session
  (let [db (fresh-db)
        sid1 (seed-session! db)
        sid2 (seed-session! db)
        root2 (event/append-event! db (base-event sid2 {:event/type :session/created}))]
    (event/append-event! db (base-event sid1 {:event/type :session/created}))
    (let [e (event-error #(event/append-event! db
                                               (base-event sid1 {:prev/event-id (:event/id root2)})))]
      (is (some? e))
      (is (= :store/cause-session-mismatch (:error/type (ex-data e)))))))

(deftest causal-links-may-cross-sessions
  (let [db (fresh-db)
        sid1 (seed-session! db)
        sid2 (seed-session! db)
        root1 (event/append-event! db (base-event sid1 {:event/type :session/created}))
        root2 (event/append-event! db (base-event sid2 {:event/type :session/created}))
        e2 (event/append-event! db (base-event sid1 {:prev/event-id (:event/id root1)}))
        ;; cross-session causal link: parent event e2 causes child event
        child-e2 (event/append-event! db (base-event sid2 {:prev/event-id (:event/id root2)
                                                            :causal-links #{{:from (:event/id e2) :type :test/cross}}}))]
    (is (= (:event/id root2) (:prev/event-id child-e2)))
    (is (= #{{:from (:event/id e2) :type :test/cross}} (:causal-links child-e2)))
    (testing "causal link is queryable"
      (is (= #{{:from (:event/id e2) :type :test/cross}} (event/get-causal-links db (:event/id child-e2)))))
    (testing "prev chain still linear and verifies"
      (is (= {:valid? true :events 2} (event/verify-event-chain db sid1)))
      (is (= {:valid? true :events 2} (event/verify-event-chain db sid2))))))

(deftest causal-links-from-must-exist
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))]
    (let [e (event-error #(event/append-event! db
                                               (base-event sid {:prev/event-id (:event/id root)
                                                                :causal-links #{{:from 99999 :type :test/missing}}})))]
      (is (some? e))
      (is (= :store/causal-link-not-found (:error/type (ex-data e)))))))

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
                                               (assoc (base-event sid {:prev/event-id (:event/id root)})
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
                   "root-event-types" "get-causal-links"])))))

;; ============================================================================
;; Step 4 — queries by session / sequence / type
;; ============================================================================

(deftest query-by-session-seq-and-type
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))
        p1 (event/append-event! db (base-event sid {:prev/event-id (:event/id root)}))
        a1 (event/append-event! db (base-event sid {:event/type :intent/authorized
                                                    :prev/event-id (:event/id p1)}))
        p2 (event/append-event! db (base-event sid {:prev/event-id (:event/id a1)}))]
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

(deftest latest-event-id-is-the-session-tip
  (let [db (fresh-db)
        sid (seed-session! db)
        other (seed-session! db)]
    (testing "a session with no events has no tip"
      (is (nil? (event/latest-event-id db sid)))
      (sqlite/with-db [spec db]
        (is (nil? (event/latest-event-id-on-conn (:connection spec) sid)))))
    (testing "the tip is the LAST event appended, not the first"
      (let [root (event/append-event! db (base-event sid {:event/type :session/created}))
            _ (is (= (:event/id root) (event/latest-event-id db sid)))
            e2 (event/append-event! db (base-event sid {:prev/event-id (:event/id root)}))
            _ (is (= (:event/id e2) (event/latest-event-id db sid)))
            e3 (event/append-event! db (base-event sid {:prev/event-id (:event/id e2)}))]
        (is (= (:event/id e3) (event/latest-event-id db sid)))
        (is (= 3 (:event/seq (event/get-event-by-id db (event/latest-event-id db sid)))))))
    (testing "another session's tip does not leak into this session's"
      (let [other-root (event/append-event! db (base-event other {:event/type :session/created}))]
        (is (= 3 (:event/seq (event/get-event-by-id db (event/latest-event-id db sid)))))
        (is (= (:event/id other-root) (event/latest-event-id db other)))))
    (testing "the conn arity reports the same tip as the spec arity"
      (sqlite/with-db [spec db]
        (is (= (event/latest-event-id db sid)
               (event/latest-event-id-on-conn (:connection spec) sid)))))))

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
    (is (nil? (:prev/event-id e)))
    (is (nil? (:cause/event-id e)))
    (is (empty? (:causal-links e)))
    (is (nil? (:payload-ref e)))
    (is (nil? (:prev-hash e)))
    (is (re-matches #"^sha256:[0-9a-f]{64}$" (:event-hash e)))
    (is (instance? java.util.Date (:created-at e)))
    (is (= {:k :v} (:metadata e)))))

(deftest prev-hash-links-the-chain
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))
        e2 (event/append-event! db (base-event sid {:prev/event-id (:event/id root)}))
        e3 (event/append-event! db (base-event sid {:prev/event-id (:event/id e2)}))]
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
        (event/append-event! db (base-event sid {:prev/event-id cause}))))
    (is (= {:valid? true :events 6} (event/verify-event-chain db sid)))))

(deftest tampered-copied-row-fails-verification
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))
        ev2 (event/append-event! db (base-event sid {:prev/event-id (:event/id root)}))
        ev3 (event/append-event! db (base-event sid {:prev/event-id (:event/id ev2)}))
        rows (sqlite/query db ["SELECT * FROM events ORDER BY event_seq"])]
    (is (= 3 (count rows)))
    (testing "an untampered copy verifies"
      (let [db2 (fresh-db)
            _ (seed-session! db2 sid)
            _ (doseq [r rows] (insert-row! db2 (assoc r :prev_event_id (:prev_event_id r) :cause_event_id (:prev_event_id r))))]
        (is (= {:valid? true :events 3} (event/verify-event-chain db2 sid)))))
    (testing "changing :event/type in a copied row breaks the hash"
      (let [db2 (fresh-db)
            _ (seed-session! db2 sid)
            tampered (mapv #(if (= (:event_seq %) (:event/seq ev2))
                              (assoc % :event_type ":node/started")
                              %)
                           rows)]
        (doseq [r tampered] (insert-row! db2 (assoc r :prev_event_id (:prev_event_id r) :cause_event_id (:cause_event_id r))))
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
        (doseq [r tampered] (insert-row! db2 (assoc r :prev_event_id (:prev_event_id r) :cause_event_id (:cause_event_id r))))
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
        (doseq [r tampered] (insert-row! db2 (assoc r :prev_event_id (:prev_event_id r) :cause_event_id (:cause_event_id r))))
        (let [v (event/verify-event-chain db2 sid)]
          (is (false? (:valid? v)))
          (is (= :event/prev-hash-mismatch (:reason v)))
          (is (= (:event/seq ev3) (:event/seq v))))))))

(deftest same-leaf-cross-namespace-type-swap-fails-verification
  (let [db (fresh-db)
        sid (seed-session! db)
        root (event/append-event! db (base-event sid {:event/type :session/created}))
        ev2 (event/append-event! db (base-event sid {:event/type :intent/completed
                                                     :prev/event-id (:event/id root)}))
        ev3 (event/append-event! db (base-event sid {:prev/event-id (:event/id ev2)}))
        rows (sqlite/query db ["SELECT * FROM events ORDER BY event_seq"])]
    (is (= 3 (count rows)))
    (testing "same-leaf cross-namespace swap (:intent/completed -> :node/completed) breaks the hash"
      (let [db2 (fresh-db)
            _ (seed-session! db2 sid)
            tampered (mapv #(if (= (:event_seq %) (:event/seq ev2))
                              (assoc % :event_type ":node/completed")
                              %)
                           rows)]
        (doseq [r tampered] (insert-row! db2 (assoc r :prev_event_id (:prev_event_id r) :cause_event_id (:cause_event_id r))))
        (let [v (event/verify-event-chain db2 sid)]
          (is (false? (:valid? v)))
          (is (= :event/hash-mismatch (:reason v)))
          (is (= (:event/seq ev2) (:event/seq v))))))))

(deftest tampered-metadata-fails-verification
  (let [db (fresh-db)
        sid (seed-session! db)
        root-event (event/append-event! db (base-event sid {:event/type :session/created}))
        middle-event (event/append-event! db (base-event sid {:prev/event-id (:event/id root-event)
                                                              :metadata {:role :middle :count 41}}))
        _ (event/append-event! db (base-event sid {:prev/event-id (:event/id middle-event)}))
        rows (sqlite/query db ["SELECT * FROM events ORDER BY event_seq"])]
    (is (= 3 (count rows)))
    (testing "rewriting the stored metadata bytes breaks the hash"
      (let [db-copy (fresh-db)
            _ (seed-session! db-copy sid)
            tampered (mapv #(if (= (:event_seq %) (:event/seq middle-event))
                              (assoc % :payload (pr-str {:role :middle :count 42}))
                              %)
                           rows)]
        (doseq [row tampered] (insert-row! db-copy row))
        (let [verdict (event/verify-event-chain db-copy sid)]
          (is (false? (:valid? verdict)))
          (is (= :event/hash-mismatch (:reason verdict)))
          (is (= (:event/seq middle-event) (:event/seq verdict))))))))

(deftest tampered-causal-links-fail-verification
  (let [db (fresh-db)
        sid (seed-session! db)
        root-event (event/append-event! db (base-event sid {:event/type :session/created}))
        linked-event (event/append-event! db (base-event sid {:prev/event-id (:event/id root-event)
                                                              :causal-links #{{:from (:event/id root-event) :type :test/derives}}}))
        rows (sqlite/query db ["SELECT * FROM events ORDER BY event_seq"])]
    (testing "the intact chain (links stored alongside rows) verifies"
      (is (= #{{:from (:event/id root-event) :type :test/derives}} (:causal-links linked-event)))
      (is (= {:valid? true :events 2} (event/verify-event-chain db sid))))
    (testing "copying rows without their links fails verification"
      (let [db-copy (fresh-db)
            _ (seed-session! db-copy sid)]
        (doseq [row rows] (insert-row! db-copy row))
        (let [verdict (event/verify-event-chain db-copy sid)]
          (is (false? (:valid? verdict)))
          (is (= :event/hash-mismatch (:reason verdict)))
          (is (= (:event/seq linked-event) (:event/seq verdict))))))
    (testing "copying rows with their links verifies"
      (let [db-copy (fresh-db)
            _ (seed-session! db-copy sid)]
        (doseq [row rows] (insert-row! db-copy row))
        (copy-links! db db-copy)
        (is (= {:valid? true :events 2} (event/verify-event-chain db-copy sid)))))
    (testing "an added link on an otherwise valid row fails verification"
      (let [db-copy (fresh-db)
            _ (seed-session! db-copy sid)]
        (doseq [row rows] (insert-row! db-copy row))
        (copy-links! db db-copy)
        (sqlite/exec! db-copy ["INSERT OR IGNORE INTO causal_links (from_event_id, to_event_id, link_type, created_at) VALUES (?, ?, ?, ?)"
                               (:event/id root-event) (:event/id linked-event) "test/forged" now])
        (let [verdict (event/verify-event-chain db-copy sid)]
          (is (false? (:valid? verdict)))
          (is (= :event/hash-mismatch (:reason verdict)))
          (is (= (:event/seq linked-event) (:event/seq verdict))))))))

(deftest tampered-generation-and-phenotype-fail-verification
  (let [db (fresh-db)
        sid (seed-session! db)
        root-event (event/append-event! db (base-event sid {:event/type :session/created}))
        middle-event (event/append-event! db (base-event sid {:prev/event-id (:event/id root-event)}))
        rows (sqlite/query db ["SELECT * FROM events ORDER BY event_seq"])]
    (is (= [1 2] (mapv :event_seq rows)))
    (testing "rewriting the stored phenotype identity breaks the hash"
      (let [db-copy (fresh-db)
            _ (seed-session! db-copy sid)
            tampered (mapv #(if (= (:event_seq %) (:event/seq middle-event))
                              (assoc % :phenotype_id (str "sha256:" (apply str (repeat 64 "f"))))
                              %)
                           rows)]
        (doseq [row tampered] (insert-row! db-copy row))
        (let [verdict (event/verify-event-chain db-copy sid)]
          (is (false? (:valid? verdict)))
          (is (= :event/hash-mismatch (:reason verdict)))
          (is (= (:event/seq middle-event) (:event/seq verdict))))))
    (testing "repointing the stored generation breaks the hash"
      (let [db-copy (fresh-db)
            _ (seed-session! db-copy sid)
            _ (sqlite/exec! db-copy ["INSERT INTO generations (id, genome_id, resolution_id, parent_id, state, current, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
                                     "generation-decoy" genome resolution nil "active" 0 now])
            tampered (mapv #(if (= (:event_seq %) (:event/seq middle-event))
                              (assoc % :generation_id "generation-decoy")
                              %)
                           rows)]
        (doseq [row tampered] (insert-row! db-copy row))
        (let [verdict (event/verify-event-chain db-copy sid)]
          (is (false? (:valid? verdict)))
          (is (= :event/hash-mismatch (:reason verdict)))
          (is (= (:event/seq middle-event) (:event/seq verdict))))))))

;; ============================================================================
;; component — redaction on the event write path (F7)
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
                         {:prev/event-id (:event/id root)
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
                             {:prev/event-id (:event/id ev)
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
                 (base-event sid {:prev/event-id (:event/id e)
                                  :metadata metadata})
                 nil)]
        (is (= metadata (:metadata ev2)))
        (is (= 2 (:event/seq ev2)))))
    (testing "the chain verifies and hashes keep the canonical form"
      (is (= {:valid? true :events 2} (event/verify-event-chain db sid)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:event-hash e))))))

(deftest subagent-result-via-causal-links
  (let [db (fresh-db)
        sid-parent (seed-session! db)
        sid-child (seed-session! db)
        root-p (event/append-event! db (base-event sid-parent {:event/type :session/created}))
        root-c (event/append-event! db (base-event sid-child {:event/type :session/created}))
        ;; child terminal
        child-ev (event/append-event! db (base-event sid-child {:prev/event-id (:event/id root-c)}))
        ;; parent result with prev linear and causal link to child terminal
        parent-ev (event/append-event! db (base-event sid-parent {:prev/event-id (:event/id root-p)
                                                                   :causal-links #{{:from (:event/id child-ev) :type :subagent/result}}}))]
    (is (= (:event/id root-p) (:prev/event-id parent-ev)))
    (is (= #{{:from (:event/id child-ev) :type :subagent/result}} (:causal-links parent-ev)))
    (is (= {:valid? true :events 2} (event/verify-event-chain db sid-parent)))
    (is (= {:valid? true :events 2} (event/verify-event-chain db sid-child)))))

;; ============================================================================
;; GC-22 — metadata validation rejects non-data WITHOUT materializing it
;; ============================================================================

(defrecord MetadataProbe [marker])

(deftest metadata-with-lazy-seq-value-is-rejected-unrealized
  (let [db (fresh-db)
        sid (seed-session! db)
        touched (atom false)
        poison (map (fn [x] (reset! touched true) x) (range 5))
        e (event-error #(event/append-event! db (base-event sid {:event/type :session/created
                                                                 :metadata {:source :event-test :items poison}})))]
    (testing "lazy seq metadata is rejected with the typed error"
      (is (some? e))
      (is (= :store/event-invalid (:error/type (ex-data e)))))
    (testing "validation never realized the seq"
      (is (false? (realized? poison)))
      (is (false? @touched)))
    (testing "nothing was written"
      (is (= [] (event/events-for-session db sid))))))

(deftest metadata-with-record-value-is-rejected
  (let [db (fresh-db)
        sid (seed-session! db)
        e (event-error #(event/append-event! db (base-event sid {:event/type :session/created
                                                                 :metadata {:source :event-test
                                                                            :probe (->MetadataProbe :marked)}})))]
    (testing "a record inside metadata is rejected with the typed error"
      (is (some? e))
      (is (= :store/event-invalid (:error/type (ex-data e)))))
    (testing "nothing was written"
      (is (= [] (event/events-for-session db sid))))))

(deftest metadata-with-deeply-nested-lazy-seq-is-rejected
  (let [db (fresh-db)
        sid (seed-session! db)
        poison (map inc (range 3))
        e (event-error #(event/append-event! db (base-event sid {:event/type :session/created
                                                                 :metadata {:level-one {:level-two {:level-three poison}}}})))]
    (testing "a lazy seq three levels deep is rejected with the typed error"
      (is (some? e))
      (is (= :store/event-invalid (:error/type (ex-data e)))))
    (testing "validation never realized the nested seq"
      (is (false? (realized? poison))))
    (testing "nothing was written"
      (is (= [] (event/events-for-session db sid))))))

(deftest metadata-with-function-or-atom-is-rejected
  (let [db (fresh-db)
        sid (seed-session! db)]
    (testing "a function value is rejected"
      (let [e (event-error #(event/append-event! db (base-event sid {:event/type :session/created
                                                                     :metadata {:handler (fn [] :boom)}})))]
        (is (some? e))
        (is (= :store/event-invalid (:error/type (ex-data e))))))
    (testing "an atom value is rejected"
      (let [pending (atom 1)
            e (event-error #(event/append-event! db (base-event sid {:event/type :session/created
                                                                     :metadata {:counter pending}})))]
        (is (some? e))
        (is (= :store/event-invalid (:error/type (ex-data e))))
        (is (= 1 @pending) "validation never dereferenced the atom")))
    (testing "nothing was written"
      (is (= [] (event/events-for-session db sid))))))

(deftest large-valid-nested-metadata-is-accepted
  (let [db (fresh-db)
        sid (seed-session! db)
        metadata {:source :event-test
                  :count 42
                  :ratio 1/3
                  :price 2.5M
                  :enabled true
                  :missing nil
                  :label "hello"
                  :initial \a
                  :route 'agent/run
                  :correlation #uuid "550e8400-e29b-41d4-a716-446655440000"
                  :at #inst "2020-01-01T00:00:00.000-00:00"
                  :tags #{:alpha :beta}
                  :trail '(1 2 {:depth [3 4]})
                  :nested {:level-one {:level-two {:level-three [1 2 {:four #{5}}]}}}
                  :rows (vec (map (fn [n] {:index n :name (str "row-" n) :flags #{:kept}}) (range 200)))}
        e (event/append-event! db (base-event sid {:event/type :session/created
                                                   :metadata metadata}))]
    (testing "a large valid nested plain-data map appends"
      (is (= metadata (:metadata e))))
    (testing "the stored payload reads back identically and the chain verifies"
      (is (= metadata (:metadata (first (event/events-for-session db sid)))))
      (is (= {:valid? true :events 1} (event/verify-event-chain db sid))))))
