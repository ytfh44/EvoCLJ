(ns evoclj.store.command-test
  "A1 — CommandSchema (store/command.clj) + continuation EDN round-trip.
   A2 — durable outbox: create-command!, fetch, idempotency, and
   create-command-with-event! atomicity (command + :command/submitted same tx)."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.command :as cmd]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

(defn- valid-command
  "A minimal valid command map. Caller may assoc overrides."
  ([] (valid-command {}))
  ([overrides]
   (merge {:cmd/id (random-uuid)
           :cmd/type :tool/invoke
           :cmd/state :queued
           :cmd/idempotency-key (str "idem-" (random-uuid))
           :cmd/payload-ref (str "sha256:" (apply str (repeat 64 "a")))
           :cmd/owner-session-id (random-uuid)
           :cmd/created-at (java.util.Date.)}
          overrides)))

;; --- valid command passes ----------------------------------------------------

(deftest valid-command-passes
  (testing "a fully populated valid command validates and round-trips via validate-command"
    (let [c (valid-command)]
      (is (cmd/command? c) "command? predicate must accept a valid command")
      (is (= c (cmd/validate-command c)) "validate-command must return the value unchanged")
      (is (nil? (cmd/explain-command c)) "explain must be nil for a valid command")))
  (testing "optional fields are accepted when present"
    (let [c (valid-command {:cmd/parent-cmd-id (random-uuid)
                            :cmd/continuation-edn {:step 1 :cursor "abc"}
                            :cmd/deadline (java.util.Date. (inc (System/currentTimeMillis)))})]
      (is (cmd/command? c))
      (is (= c (cmd/validate-command c)))))
  (testing "all six states are valid"
    (doseq [s [:queued :running :succeeded :failed :timed-out :cancelled]]
      (is (cmd/command-state? s) (str s " must be a valid state"))
      (is (cmd/command? (valid-command {:cmd/state s})) (str s " must validate")))))

;; --- illegal state string -> fails with malli error -------------------------

(deftest illegal-state-string-fails
  (testing "state as a string (not a keyword enum) is rejected"
    (let [c (valid-command {:cmd/state "queued"})]
      (is (not (cmd/command? c)) "string state must not validate")
      (is (thrown? clojure.lang.ExceptionInfo (cmd/validate-command c))
          "validate-command must throw on string state")
      (try
        (cmd/validate-command c)
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :store/command-invalid (:error/type (ex-data e))))
          (is (some? (:errors (ex-data e))) "humanized errors must be present")))))
  (testing "unknown keyword state is rejected"
    (let [c (valid-command {:cmd/state :unknown-state})]
      (is (not (cmd/command? c)))
      (is (thrown? clojure.lang.ExceptionInfo (cmd/validate-command c))))))

;; --- missing idempotency_key -> fails --------------------------------------

(deftest missing-idempotency-key-fails
  (testing "omitting :cmd/idempotency-key fails schema validation"
    (let [c (dissoc (valid-command) :cmd/idempotency-key)]
      (is (not (cmd/command? c)) "missing idempotency-key must not validate")
      (is (thrown? clojure.lang.ExceptionInfo (cmd/validate-command c)))
      (try
        (cmd/validate-command c)
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :store/command-invalid (:error/type (ex-data e))))
          (let [errors (:errors (ex-data e))]
            (is (some? errors)))))))
  (testing "empty idempotency-key is rejected (non-empty constraint)"
    (let [c (valid-command {:cmd/idempotency-key ""})]
      (is (not (cmd/command? c))))))

;; --- missing payload_ref -> fails ------------------------------------------

(deftest missing-payload-ref-fails
  (testing "omitting :cmd/payload-ref fails"
    (let [c (dissoc (valid-command) :cmd/payload-ref)]
      (is (not (cmd/command? c)))
      (is (thrown? clojure.lang.ExceptionInfo (cmd/validate-command c)))
      (try
        (cmd/validate-command c)
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :store/command-invalid (:error/type (ex-data e))))))))
  (testing "non-sha256 payload-ref is rejected"
    (let [c (valid-command {:cmd/payload-ref "not-a-hash"})]
      (is (not (cmd/command? c)))
      (is (thrown? clojure.lang.ExceptionInfo (cmd/validate-command c)))))
  (testing "payload_ref must be sha256: + 64 hex"
    (let [good (str "sha256:" (apply str (repeat 64 "f")))
          bad  (str "sha256:" (apply str (repeat 63 "f")))]
      (is (cmd/command? (valid-command {:cmd/payload-ref good})))
      (is (not (cmd/command? (valid-command {:cmd/payload-ref bad})))))))

;; --- continuation EDN round-trip -------------------------------------------

(deftest continuation-edn-round-trips
  (testing "stored EDN continuation round-trips via pr-str / edn/read-string"
    (let [continuation {:step 42 :cursor "abc" :nested {:a [1 2 3] :b #{:x :y}}}
          c (valid-command {:cmd/continuation-edn continuation})
          _ (is (cmd/command? c) "command with EDN continuation must validate")
          stored (pr-str continuation)
          restored (edn/read-string stored)]
      (is (= continuation restored) "EDN must round-trip through pr-str")))
  (testing "nil continuation is absent — not stored as nil EDN"
    (let [c (valid-command)]
      (is (not (contains? c :cmd/continuation-edn)))
      (is (cmd/command? c))))
  (testing "various EDN shapes survive round-trip"
    (doseq [v [nil 42 "hello" [:a :b :c] {:x 1} #{1 2} '(1 2 3)]]
      (let [s (pr-str v)
            r (edn/read-string s)]
        (is (= v r) (str "round-trip failed for " (pr-str v))))))
  (testing "continuation stored as TEXT in DB would be retrieved as string then parsed"
    (let [original {:agent/spawn {:phenotype "ph1"}}
          as-text (pr-str original)
          from-db (edn/read-string as-text)]
      (is (= original from-db)))))

;; ---------------------------------------------------------------------------
;; A2 — DB helpers (temp file + migrate + session seed)
;; ---------------------------------------------------------------------------

(def ^:private db-paths (atom []))

(defn- temp-db-path
  []
  (let [f (java.io.File/createTempFile "command-test-" ".db")]
    (.deleteOnExit f)
    (let [p (.getAbsolutePath f)]
      (swap! db-paths conj p)
      p)))

(defn- cleanup!
  []
  (doseq [p @db-paths]
    (try (io/delete-file p true) (catch Exception _ nil)))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db
  []
  (let [db (temp-db-path)]
    (migrate/migrate! db)
    db))

(def ^:private gen-id "generation-1")
(def ^:private genome-id (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private phenotype-id (str "sha256:" (apply str (repeat 64 "c"))))

(defn- seed-session!
  "Insert a generation + session suitable for command FK + event chain.
   Returns session-id UUID. Seeds the mandatory :session/created root
   event so :command/submitted (non-root) has a valid cause."
  [db]
  (let [sid (random-uuid)
        now "2025-01-01T00:00:00Z"]
    ;; P5/F: ensure FK targets for generations/sessions (artifacts + genomes)
    (sqlite/with-db [conn db]
      (when-not (first (jdbc/query conn ["SELECT id FROM generations WHERE id = ?" gen-id]))
        (try (jdbc/insert! conn :artifacts {:hash genome-id :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
        (try (jdbc/insert! conn :artifacts {:hash resolution-id :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
        (try (jdbc/insert! conn :artifacts {:hash phenotype-id :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
        (try (jdbc/insert! conn :genomes {:id genome-id :created_at now}) (catch Exception _ nil))
        (jdbc/insert! conn :generations
                      {:id gen-id
                       :genome_id genome-id
                       :resolution_id resolution-id
                       :parent_id nil
                       :state "active"
                       :current 1
                       :created_at now}))
      (jdbc/insert! conn :sessions
                    {:id (str sid)
                     :generation_id gen-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :phenotype_id phenotype-id
                     :state "created"
                     :created_at now
                     :updated_at nil}))
    ;; seed the root :session/created event via the public API (ensures hash chain)
    (event/append-event! db
                         {:session/id sid
                          :generation/id gen-id
                          :phenotype/id phenotype-id
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}
                          :created-at (java.util.Date/from (java.time.Instant/parse now))})
    sid))

(defn- command-for
  "Build a valid command map pinned to `owner-sid`, with optional overrides.
   Uses the :cmd/* keys as well as plain-key tolerance is tested at schema level."
  [owner-sid overrides]
  (merge {:cmd/id (random-uuid)
          :cmd/type :tool/invoke
          :cmd/state :queued
          :cmd/idempotency-key (str "idem-" (random-uuid))
          :cmd/payload-ref (str "sha256:" (apply str (repeat 64 "a")))
          :cmd/owner-session-id owner-sid
          :cmd/created-at (java.util.Date.)}
         overrides))

;; ---------------------------------------------------------------------------
;; 1) create-command! inserts and fetch returns same
;; ---------------------------------------------------------------------------

(deftest create-command-inserts-and-fetch-returns-same
  (testing "create-command! persists and fetch-command retrieves the same fields"
    (let [db (fresh-db)
          sid (seed-session! db)
          orig (command-for sid {:cmd/continuation-edn {:step 1}
                                 :cmd/deadline (java.util.Date. (+ (System/currentTimeMillis) 60000))})
          returned (cmd/create-command! db orig)
          fetched (cmd/fetch-command db (:cmd/id orig))]
      (is (= (:cmd/id orig) (:cmd/id returned)) "returned id must match input")
      (is (some? fetched) "fetch must find the row")
      (is (= (:cmd/id orig) (:cmd/id fetched)))
      (is (= (:cmd/type orig) (:cmd/type fetched)))
      (is (= (:cmd/state orig) (:cmd/state fetched)))
      (is (= (:cmd/idempotency-key orig) (:cmd/idempotency-key fetched)))
      (is (= (:cmd/payload-ref orig) (:cmd/payload-ref fetched)))
      (is (= (:cmd/owner-session-id orig) (:cmd/owner-session-id fetched)))
      (is (= (:cmd/continuation-edn orig) (:cmd/continuation-edn fetched)) "EDN must round-trip")
      (is (= (.getTime ^java.util.Date (:cmd/deadline orig))
             (.getTime ^java.util.Date (:cmd/deadline fetched))) "deadline must round-trip")
      ;; list-commands filter by owner
      (let [all (cmd/list-commands db {:owner-session-id sid})]
        (is (= 1 (count all)) "list-commands filtered by owner should return one")
        (is (= (:cmd/id orig) (:cmd/id (first all))))))))

;; ---------------------------------------------------------------------------
;; 2) duplicate idempotency_key -> :store/duplicate-command
;; ---------------------------------------------------------------------------

(deftest duplicate-idempotency-key-throws-typed-error
  (testing "second insert with same idempotency_key throws :store/duplicate-command"
    (let [db (fresh-db)
          sid (seed-session! db)
          idem (str "idem-dup-" (random-uuid))
          c1 (command-for sid {:cmd/idempotency-key idem})
          c2 (command-for sid {:cmd/idempotency-key idem})]
      (cmd/create-command! db c1)
      (try
        (cmd/create-command! db c2)
        (is false "should have thrown :store/duplicate-command")
        (catch clojure.lang.ExceptionInfo e
          (is (= :store/duplicate-command (:error/type (ex-data e))) "typed duplicate error")
          (is (= idem (:idempotency-key (ex-data e))) "ex-data must carry the colliding key")))))
  (testing "different idempotency keys do not collide"
    (let [db (fresh-db)
          sid (seed-session! db)
          c1 (command-for sid {})
          c2 (command-for sid {})]
      (cmd/create-command! db c1)
      (cmd/create-command! db c2)
      (is (= 2 (count (cmd/list-commands db {:owner-session-id sid}))) "two distinct commands should persist"))))

;; ---------------------------------------------------------------------------
;; 3) create-command-with-event! inserts both rows atomically
;; ---------------------------------------------------------------------------

(deftest create-command-with-event-inserts-both-rows-atomically
  (testing "outbox transaction persists both the command row and the :command/submitted event"
    (let [db (fresh-db)
          sid (seed-session! db)
          cmd (command-for sid {})
          {:keys [command event]} (cmd/create-command-with-event! db cmd nil)]
      (is (= (:cmd/id cmd) (:cmd/id command)) "returned command must be the inserted one")
      (is (some? (cmd/fetch-command db (:cmd/id cmd))) "command row must exist after outbox")
      (is (= :command/submitted (:event/type event)) "event must be :command/submitted")
      (is (= sid (:session/id event)) "event must be anchored to owner session")
      (is (= (str (:cmd/id cmd)) (get-in event [:metadata :command/id])) "event metadata should carry command id")
      ;; verify via event store read
      (let [evs (event/events-for-session db sid)]
        (is (= 2 (count evs)) "session should have root + command/submitted")
        (is (= :command/submitted (:event/type (second evs))))
        (is (= {:valid? true :events 2} (event/verify-event-chain db sid)) "hash chain must verify"))))
  (testing "create-command-with-event! with explicit event override still links command"
    (let [db (fresh-db)
          sid (seed-session! db)
          cmd (command-for sid {})
          result (cmd/create-command-with-event! db cmd {:event/type :command/submitted :metadata {:command/id (str (:cmd/id cmd)) :extra "x"}})]
      (is (= "x" (get-in (:event result) [:metadata :extra])) "explicit metadata should be preserved"))))

;; ---------------------------------------------------------------------------
;; 4) on event failure, command row not inserted (rollback)
;; ---------------------------------------------------------------------------

(deftest outbox-rollback-on-event-failure
  (testing "when the event insert fails, the command row is rolled back"
    (let [db (fresh-db)
          sid (seed-session! db)
          cmd (command-for sid {})
          bad-event {:event/type :command/submitted
                     :cause/event-id 999999
                     :metadata {:command/id (str (:cmd/id cmd))}}]
      (try
        (cmd/create-command-with-event! db cmd bad-event)
        (is false "bad event should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (contains? #{:store/cause-not-found :store/event-invalid} (:error/type (ex-data e)))
              (str "expected cause/event validation error, got " (:error/type (ex-data e))))))
      (is (nil? (cmd/fetch-command db (:cmd/id cmd))) "command must NOT be persisted after event failure (rollback)")
      (is (= 1 (count (event/events-for-session db sid))) "only the root event should remain after rollback")
      (is (= {:valid? true :events 1} (event/verify-event-chain db sid)) "chain still valid after rolled-back outbox"))))
