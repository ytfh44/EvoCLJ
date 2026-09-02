(ns evoclj.promotion.promotion-outbox-test
  "Fleet P4 — promotion outbox atomicity (DAG P4).

  Closes promotion gap: promotion must atomically move CURRENT and append
  event in same transaction (outbox pattern). This suite proves:

  - Happy path: promoted CURRENT move + :promotion/promoted event + outbox
    row commit together in one transaction; stale path also atomic.
  - Rollback atomic: a mid-transaction throw (failpoint) AFTER the event
    would have been inserted rolls back EVERY write — no promotion row,
    no generation, no CURRENT move, no event, no outbox. This is the
    outbox pattern guarantee: either both durable or neither.
  - Outbox FK: promotion_outbox FK-locks promotion and event; a missing
    promotion or event cannot be linked.

  Fresh temp databases are migrated from the classpath migrations
  (including 010-promotion-outbox) and deleted after every test; each
  fixture candidate gets its own CAS genome body, finalized evaluation,
  and operator session (same pattern as promote-test)."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.promotion.current :as current]
            [evoclj.promotion.promote :as promote]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- shared fixtures (mirrors promote-test) --------------------------------

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private seed-gen "generation-1")
(def ^:private retired-gen "generation-0")
(def ^:private parent-genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private parent-resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private new-resolution (str "sha256:" (apply str (repeat 64 "d"))))

(def ^:private db-paths (atom []))
(def ^:private cas-roots (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-outbox-" ".db" (make-array FileAttribute 0)))]
    (swap! db-paths conj p) p))

(defn- temp-cas-root []
  (let [p (str (Files/createTempDirectory "evoclj-outbox-cas-" (make-array FileAttribute 0)))]
    (swap! cas-roots conj p) p))

(defn- delete-tree! [p]
  (let [path (Paths/get p (make-array String 0))]
    (when (Files/exists path (make-array java.nio.file.LinkOption 0))
      (with-open [stream (Files/walk path (make-array java.nio.file.FileVisitOption 0))]
        (doseq [q (reverse (iterator-seq (.iterator stream)))]
          (Files/deleteIfExists q))))))

(defn- cleanup! []
  (doseq [p @db-paths] (Files/deleteIfExists (Paths/get p (make-array String 0))))
  (doseq [p @cas-roots] (delete-tree! p))
  (reset! db-paths []) (reset! cas-roots []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db) db))

(defn- seed-generation! [db]
  (sqlite/with-db [conn db]
    ;; Fleet P5/F + 011: FK targets for generations and sessions (artifacts/genomes before generations/sessions)
    (try (jdbc/insert! conn :artifacts {:hash parent-genome :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :artifacts {:hash parent-resolution :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :artifacts {:hash phenotype :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :genomes {:id parent-genome :created_at now}) (catch Exception _ nil))
    (jdbc/insert! conn :generations {:id seed-gen :genome_id parent-genome :resolution_id parent-resolution :parent_id nil :state "active" :current 1 :created_at now})))

(defn- add-retired-generation! [db]
  (sqlite/with-db [conn db]
    (try (jdbc/insert! conn :artifacts {:hash parent-genome :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :artifacts {:hash parent-resolution :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :artifacts {:hash phenotype :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :genomes {:id parent-genome :created_at now}) (catch Exception _ nil))
    (jdbc/insert! conn :generations {:id retired-gen :genome_id parent-genome :resolution_id parent-resolution :parent_id nil :state "retired" :current 0 :created_at now})))

(defn- add-mutation! [conn]
  (let [mutation-id (random-uuid) eid (str "sha256:" (apply str (repeat 64 "e")))]
    (try (jdbc/insert! conn :artifacts {:hash parent-genome :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :artifacts {:hash eid :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :genomes {:id parent-genome :created_at now}) (catch Exception _ nil))
    (jdbc/insert! conn :mutations {:id (str mutation-id) :parent_genome_id parent-genome :hypothesis_id (str (random-uuid)) :evidence_id eid :risk "parameter" :ops (pr-str []) :expected_effect (pr-str {}) :created_at now})
    mutation-id))

(defn- add-candidate! [db candidate-id parent-generation-id genome-id]
  (sqlite/with-db [conn db]
    (let [mutation-id (add-mutation! conn) eid (str "sha256:" (apply str (repeat 64 "e")))]
      (try (jdbc/insert! conn :artifacts {:hash genome-id :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
      (try (jdbc/insert! conn :artifacts {:hash eid :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
      (try (jdbc/insert! conn :genomes {:id genome-id :created_at now}) (catch Exception _ nil))
      (try (jdbc/insert! conn :genomes {:id parent-genome :created_at now}) (catch Exception _ nil))
      (jdbc/insert! conn :candidates {:id (str candidate-id) :parent_generation_id parent-generation-id :parent_genome_id parent-genome :genome_id genome-id :mutation_id (str mutation-id) :evidence_id eid :risk "parameter" :state "eligible" :created_at now})))
  candidate-id)

(defn- add-evaluation! [db evaluation-id candidate-id parent-generation-id eligibility]
  (sqlite/with-db [conn db]
    (jdbc/insert! conn :eval_runs {:id (str evaluation-id) :candidate_id (str candidate-id) :parent_generation_id parent-generation-id :profile_id ":default" :gates (pr-str []) :paired_results_ref nil :summary (pr-str {:hard {} :utility {} :cost {} :complexity {}}) :eligibility (pr-str eligibility) :status "finalized" :created_at now}))
  evaluation-id)

(defn- operator-session! [db]
  (let [sid (:session/id (session/create-session! db {:genome/id parent-genome :resolution/id parent-resolution :phenotype/id phenotype :generation/id seed-gen}))]
    (event/append-event! db {:session/id sid :generation/id seed-gen :phenotype/id phenotype :event/type :session/created :prev/event-id nil :payload-ref nil :metadata {}})
    sid))

(defn- promotion-fixture
  ([] (promotion-fixture {}))
  ([{:keys [n-candidates eligibility parent-generation genome-body]}]
   (let [db (fresh-db) cas-root (temp-cas-root) cas (cas/->cas cas-root)
         _ (seed-generation! db)
         _ (sqlite/with-db [conn db] (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, ?, ?, ?)" new-resolution "application/edn" 0 now]))
         _ (when (and parent-generation (not= parent-generation seed-gen)) (add-retired-generation! db))
         parent-gen-id (or parent-generation seed-gen)
         elig (or eligibility {:eligible? true :reasons []})
         candidates (mapv (fn [i] (let [candidate-id (random-uuid) evaluation-id (random-uuid) genome-id (:artifact/id (cas/put-bytes! cas (.getBytes (or genome-body (str "candidate genome body " i)) StandardCharsets/UTF_8) {})) sid (operator-session! db)] (add-candidate! db candidate-id parent-gen-id genome-id) (add-evaluation! db evaluation-id candidate-id parent-gen-id elig) {:candidate/id candidate-id :evaluation/id evaluation-id :candidate/genome-id genome-id :event/session-id sid})) (range (or n-candidates 1)))
         first-c (first candidates)]
     {:db db :cas cas :generation/id seed-gen :resolution/id new-resolution :candidate/id (:candidate/id first-c) :evaluation/id (:evaluation/id first-c) :candidate/genome-id (:candidate/genome-id first-c) :event/session-id (:event/session-id first-c) :candidates candidates})))

(defn- promotion-system [fx] {:store {:sqlite (:db fx) :cas (:cas fx)} :resolution/id (:resolution/id fx) :event/session-id (:event/session-id fx)})
(defn- system-for [fx c] {:store {:sqlite (:db fx) :cas (:cas fx)} :resolution/id (:resolution/id fx) :event/session-id (:event/session-id c)})
(defn- promote-request [fx] {:candidate-id (:candidate/id fx) :evaluation-id (:evaluation/id fx) :expected-parent-generation (:generation/id fx)})

(defn- current-rows [db] (sqlite/query db ["SELECT * FROM generations WHERE current = 1"]))
(defn- promotion-rows [db] (sqlite/query db ["SELECT * FROM promotions"]))
(defn- outbox-rows [db] (sqlite/query db ["SELECT * FROM promotion_outbox"]))
(defn- event-rows-for [db session-id] (event/events-for-session db session-id))
(defn- candidate-row [db cid] (first (sqlite/query db ["SELECT * FROM candidates WHERE id = ?" (str cid)])))
(defn- gen-row [db gid] (first (sqlite/query db ["SELECT * FROM generations WHERE id = ?" gid])))
(defn- tx-error [f] (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

;; ============================================================================
;; Atomic happy path — promoted event and outbox commit together
;; ============================================================================

(deftest outbox-happy-path-promoted-atomic
  (let [fx (promotion-fixture) db (:db fx)
        result (promote/promote! (promotion-system fx) (promote-request fx))]
    (testing "promotion succeeds"
      (is (= :promoted (:status result))))
    (testing "CURRENT moved and exactly one current"
      (is (= 1 (count (current-rows db))))
      (is (= (:to result) (:id (first (current-rows db))))))
    (testing "promotions row exists"
      (is (= 1 (count (promotion-rows db))))
      (is (= (:to result) (:to_generation_id (first (promotion-rows db))))))
    (testing "promotion event exists and is hash-linked"
      (let [events (event/events-by-type db (:event/session-id fx) :promotion/promoted)]
        (is (= 1 (count events)))
        (is (= (:to result) (get-in (first events) [:metadata :to])))
        (is (= (:generation/id fx) (get-in (first events) [:metadata :from])))))
    (testing "outbox row FK-links promotion and event atomically in same commit"
      (let [rows (outbox-rows db)]
        (is (= 1 (count rows)))
        (let [row (first rows)]
          (is (= (str (:event/session-id fx)) (:session_id row)))
          (is (= "promotion/promoted" (:event_type row)))
          (is (= (str (:evaluation/id fx)) (:evaluation_id (first (promotion-rows db)))))
          (is (= (:id (first (promotion-rows db))) (:promotion_id row)))
          (is (= (:event/id (first (event/events-by-type db (:event/session-id fx) :promotion/promoted))) (:event_id row)))
          (is (= 0 (:dispatched row))))))
    (testing "event chain verifies"
      (is (:valid? (event/verify-event-chain db (:event/session-id fx)))))
    (testing "candidate and generation states correct"
      (is (= "promoted" (:state (candidate-row db (:candidate/id fx)))))
      (is (= "active" (:state (gen-row db (:to result)))))
      (is (= "retired" (:state (gen-row db (:generation/id fx))))))))

(deftest outbox-stale-atomic
  (let [fx (promotion-fixture {:parent-generation retired-gen}) db (:db fx)
        result (promote/promote! (promotion-system fx) (promote-request fx))]
    (testing "stale result"
      (is (= :stale (:status result)))
      (is (= seed-gen (:current result))))
    (testing "no promotion row and no new generation"
      (is (empty? (promotion-rows db)))
      (is (nil? (:to result)) "stale has no :to")
      (is (= seed-gen (:id (first (current-rows db))))))
    (testing "stale event + outbox committed atomically alongside candidate state"
      (is (= "stale" (:state (candidate-row db (:candidate/id fx)))))
      (let [events (event/events-by-type db (:event/session-id fx) :promotion/stale)]
        (is (= 1 (count events)))
        (is (= (:generation/id fx) (get-in (first events) [:metadata :expected]))))
      (let [rows (outbox-rows db)]
        (is (= 1 (count rows)))
        (is (nil? (:promotion_id (first rows))) "stale has no promotion_id")
        (is (= "promotion/stale" (:event_type (first rows))))))))

;; ============================================================================
;; Rollback atomic — failpoint AFTER event insert rolls back everything
;; ============================================================================

(deftest outbox-rollback-promoted-event-and-promotion-atomic
  (let [fx (promotion-fixture) db (:db fx)
        system (assoc (promotion-system fx) :failpoint (fn [] (throw (ex-info "injected after event" {:error/type :test/injected}))))
        e (tx-error #(promote/promote! system (promote-request fx)))]
    (testing "injected failure propagates"
      (is (= :test/injected (:error/type (ex-data e)))))
    (testing "EXACTLY ONE active CURRENT remains — the seed (rolled back)"
      (let [rows (current-rows db)]
        (is (= 1 (count rows)))
        (is (= seed-gen (:id (first rows))))
        (is (= "active" (:state (first rows))))))
    (testing "every write rolled back — no promotion, no generation, no candidate change"
      (is (empty? (promotion-rows db)))
      (is (= "eligible" (:state (candidate-row db (:candidate/id fx)))))
      (is (= "active" (:state (gen-row db seed-gen))))
      (is (= 1 (count (sqlite/query db ["SELECT id FROM generations"])))))
    (testing "no event or outbox survived the rollback (atomic)"
      (is (empty? (event/events-by-type db (:event/session-id fx) :promotion/promoted)))
      (is (empty? (outbox-rows db)))
      ;; only the original :session/created event remains
      (is (= 1 (count (event-rows-for db (:event/session-id fx)))))
      (is (= :session/created (:event/type (first (event-rows-for db (:event/session-id fx)))))))
    (testing "event chain still valid after rollback"
      (is (:valid? (event/verify-event-chain db (:event/session-id fx)))))))

(deftest outbox-rollback-stale-event-atomic
  (let [fx (promotion-fixture {:parent-generation retired-gen}) db (:db fx)
        system (assoc (promotion-system fx) :failpoint (fn [] (throw (ex-info "injected stale" {:error/type :test/injected}))))
        e (tx-error #(promote/promote! system (promote-request fx)))]
    (testing "stale failpoint propagates"
      (is (= :test/injected (:error/type (ex-data e)))))
    (testing "candidate not marked stale, no event, no outbox"
      (is (= "eligible" (:state (candidate-row db (:candidate/id fx)))))
      (is (empty? (event/events-by-type db (:event/session-id fx) :promotion/stale)))
      (is (empty? (outbox-rows db)))
      (is (= seed-gen (:id (first (current-rows db))))))))

;; ============================================================================
;; Outbox FK enforcement — bogus FK cannot be inserted even raw
;; ============================================================================

(deftest outbox-fk-enforced
  (let [fx (promotion-fixture) db (:db fx) _ (promote/promote! (promotion-system fx) (promote-request fx))
        promo-id (:id (first (promotion-rows db)))
        ev-id (:event/id (first (event/events-by-type db (:event/session-id fx) :promotion/promoted)))]
    (testing "outbox row already links correct FKs"
      (is (= promo-id (:promotion_id (first (outbox-rows db)))))
      (is (= ev-id (:event_id (first (outbox-rows db))))))
    (testing "bogus promotion_id FK violation fails"
      (is (thrown? Exception
                   (sqlite/exec! db ["INSERT INTO promotion_outbox (id, promotion_id, session_id, event_id, event_type, event_seq, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
                                      (str (random-uuid)) "bogus-promo" (str (:event/session-id fx)) ev-id "promotion/promoted" 999 "2025-01-01T00:00:00Z"]))))
    (testing "bogus event_id FK violation fails"
      (is (thrown? Exception
                   (sqlite/exec! db ["INSERT INTO promotion_outbox (id, promotion_id, session_id, event_id, event_type, event_seq, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
                                      (str (random-uuid)) promo-id (str (:event/session-id fx)) 99999 "promotion/promoted" 999 "2025-01-01T00:00:00Z"]))))))