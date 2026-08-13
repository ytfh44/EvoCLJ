(ns evoclj.promotion.canary-test
  "Task 9.3 tests for canary routing of NEW sessions.

  Step 1: routing is a deterministic pure function of the session-routing
  key — a stable sha256-based bucket (reusing evoclj.genome.hash
  conventions), never a mutable global random source. Step 2: a session
  already created for G42 stays pinned to G42 after the allocation
  changes — the pin lives in the store row; routing only affects NEW
  sessions. Step 3: over a large deterministic key fixture (10,000 keys)
  the canary generation receives approximately the declared allocation
  (asserted within a ±2% band). Step 4: the allocation version and the
  bucket are persisted with each session decision so routing can be
  audited later.

  Fresh temp databases are migrated from the classpath migrations
  (001-init.sql + 003-routing.sql) and deleted after every test."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.genome.hash :as genome-hash]
            [evoclj.promotion.canary :as canary]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]))

;; --- fixtures ---------------------------------------------------------------

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private g42 "G42")
(def ^:private g43 "G43")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))

(def ^:private db-paths (atom []))

(defn- temp-db-path
  "A throwaway SQLite file in the system temp dir."
  []
  (let [p (str (java.nio.file.Files/createTempFile
                "evoclj-canary-" ".db"
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

(defn- seed-generations!
  "Insert the G42 and G43 generation rows sessions can be pinned to."
  [db]
  (sqlite/with-db [conn db]
    (doseq [g [g42 g43]]
      (jdbc/insert! conn :generations
                    {:id g
                     :genome_id genome
                     :resolution_id resolution
                     :parent_id nil
                     :state "active"
                     :current 0
                     :created_at now}))))

(defn- deployment-state
  "The Task 9.3 deployment-state shape; callers merge overrides:

      {:current-generation \"G42\"
       :canary {:generation \"G43\"
                :allocation 0.10
                :ladder [0.10 0.25 0.50 1.0]   ; normative ladder
                :version \"v1\"}               ; allocation version
       :active? true}"
  [& [overrides]]
  (merge {:current-generation g42
          :canary {:generation g43
                   :allocation 0.10
                   :ladder [0.10 0.25 0.50 1.0]
                   :version "v1"}
          :active? true}
         overrides))

(defn- key-fixture
  "The 10,000 deterministic session-routing keys."
  []
  (mapv #(format "session-key-%05d" %) (range 10000)))

(defn- canary-share
  "The fraction of `keys` routed to the canary generation."
  [ds keys]
  (let [n (count (filter #(= g43 (canary/select-generation-for-new-session ds %))
                         keys))]
    (double (/ n (count keys)))))

(defn- session-request
  "A valid create-session! request built from a routing decision."
  [decision]
  (merge {:genome/id genome
          :resolution/id resolution
          :phenotype/id phenotype}
         (select-keys decision [:generation/id :routing])))

(defn- error-type
  "The :error/type of the ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))

;; ============================================================================
;; Step 1 — deterministic routing by stable hash, never mutable global
;;          random state
;; ============================================================================

(deftest routing-is-a-deterministic-pure-function
  (let [ds (deployment-state)
        key "session-abc-42"]
    (testing "the same key and deployment state always choose the same generation"
      (is (= (canary/select-generation-for-new-session ds key)
             (canary/select-generation-for-new-session ds key))))
    (testing "100 repeated routings of one key are identical (no random source)"
      (is (apply = (repeatedly 100 #(canary/select-generation-for-new-session ds key)))))
    (testing "the bucket is a pure function of the key, always in [0, 1)"
      (doseq [k ["a" "b" "c" "session-key-00001" ""]]
        (let [b (canary/routing-bucket k)]
          (is (<= 0.0 b))
          (is (< b 1.0))
          (is (= b (canary/routing-bucket k))))))
    (testing "the bucket matches the plan formula: (mod (bigint (subs digest 0 16) 16) 10000) / 10000.0"
      (let [hex (subs (genome-hash/text-digest key) 7)
            expected (/ (mod (biginteger (java.math.BigInteger. (subs hex 0 16) 16)) 10000)
                        10000.0)]
        (is (= expected (canary/routing-bucket key)))))))

;; ============================================================================
;; Step 2 — a session selected for G42 remains pinned to G42 after the
;;          allocation changes
;; ============================================================================

(deftest existing-sessions-stay-pinned-after-allocation-changes
  (let [db (fresh-db)
        _ (seed-generations! db)
        key "session-key-00000"
        decision (canary/routing-decision (deployment-state) key)
        s1 (session/create-session! db (session-request decision))
        sid1 (:session/id s1)]
    (testing "the first session was created pinned to the routed generation"
      (is (= g42 (:generation/id decision)))
      (is (= g42 (:generation/id s1))))
    (testing "after the allocation rises to 100%, the old session is untouched"
      (let [ds2 (deployment-state {:canary {:generation g43
                                            :allocation 1.0
                                            :ladder [0.10 0.25 0.50 1.0]
                                            :version "v1"}})
            s1-after (session/get-session db sid1)
            s2 (session/create-session! db (session-request
                                            (canary/routing-decision ds2 key)))]
        (testing "the pin lives in the store row: generation and routing never move"
          (is (= g42 (:generation/id s1-after)))
          (is (= (:routing decision) (:routing s1-after))))
        (testing "only NEW sessions follow the new allocation"
          (is (= g43 (:generation/id s2))))))))

;; ============================================================================
;; Step 3 — the canary receives only the declared allocation over a large
;;          deterministic key fixture
;; ============================================================================

(deftest canary-receives-declared-allocation-over-large-fixture
  (let [keys (key-fixture)]
    (doseq [[allocation label] [[0.0 "0%"] [0.10 "10%"] [0.25 "25%"]
                                [0.50 "50%"] [1.0 "100%"]]]
      (testing (str "at " label " allocation the canary share is within ±2%")
        (let [ds (deployment-state {:canary {:generation g43
                                             :allocation allocation
                                             :ladder [0.10 0.25 0.50 1.0]
                                             :version "v1"}})
              share (canary-share ds keys)
              expected (double allocation)]
          (is (<= (Math/abs (- share expected)) 0.02)
              (format "expected %.3f ±0.02, got %.3f" expected share)))))))

;; ============================================================================
;; Step 4 — the allocation version is persisted with each session decision
;; ============================================================================

(deftest routing-is-persisted-with-the-allocation-version
  (let [db (fresh-db)
        _ (seed-generations! db)
        decision (canary/routing-decision (deployment-state) "session-key-00000")
        s (session/create-session! db (session-request decision))]
    (testing "the full routing decision round-trips through the store"
      (is (= {:generation/id (:generation/id s)
              :routing (:routing s)}
             decision)))
    (testing "the deployment version is the allocation version at decision time"
      (is (= "v1" (get-in s [:routing :deployment-version]))))
    (testing "the bucket is an integer in [0, 10000)"
      (is (int? (get-in s [:routing :bucket])))
      (is (<= 0 (get-in s [:routing :bucket] 9999) 9999)))
    (testing "a later allocation version is recorded for NEW sessions only"
      (let [ds2 (deployment-state {:canary {:generation g43
                                            :allocation 0.50
                                            :ladder [0.10 0.25 0.50 1.0]
                                            :version "v2"}})
            s2 (session/create-session! db (session-request
                                            (canary/routing-decision ds2 "session-key-00000")))]
        (is (= "v2" (get-in s2 [:routing :deployment-version])))
        (is (= "v1" (get-in s [:routing :deployment-version])))))))

;; ============================================================================
;; Routing edge cases: no deployment state, inactive canary, absent canary
;; ============================================================================

(deftest absent-or-inactive-canary-routes-everything-to-current
  (testing "no deployment state at all → no canary information (caller falls back to CURRENT)"
    (is (nil? (canary/select-generation-for-new-session nil "any-key"))))
  (testing ":active? false → every key routes to the current generation"
    (let [ds (deployment-state {:active? false})]
      (is (every? #(= g42 (canary/select-generation-for-new-session ds %))
                  (key-fixture)))))
  (testing "no :canary config → every key routes to the current generation"
    (let [ds (deployment-state {:canary nil})]
      (is (every? #(= g42 (canary/select-generation-for-new-session ds %))
                  (key-fixture))))))

;; ============================================================================
;; Input validation at the module boundary (fail closed)
;; ============================================================================

(deftest malformed-deployment-state-fails-closed
  (testing "unknown keys are rejected (closed trust boundary)"
    (is (= :promotion/routing-invalid
           (error-type #(canary/select-generation-for-new-session
                         (assoc (deployment-state) :bogus 1) "k")))))
  (testing "a missing current generation is rejected"
    (is (= :promotion/routing-invalid
           (error-type #(canary/select-generation-for-new-session
                         (dissoc (deployment-state) :current-generation) "k")))))
  (testing "an out-of-range allocation is rejected"
    (is (= :promotion/routing-invalid
           (error-type #(canary/select-generation-for-new-session
                         (deployment-state {:canary {:generation g43
                                                     :allocation 1.5
                                                     :ladder [0.10 0.25 0.50 1.0]
                                                     :version "v1"}})
                         "k")))))
  (testing "a missing canary version is rejected"
    (is (= :promotion/routing-invalid
           (error-type #(canary/select-generation-for-new-session
                         (deployment-state {:canary {:generation g43
                                                     :allocation 0.10
                                                     :ladder [0.10 0.25 0.50 1.0]}})
                         "k")))))
  (testing "a non-string routing key is rejected"
    (is (= :promotion/routing-invalid
           (error-type #(canary/select-generation-for-new-session (deployment-state) 42))))))
