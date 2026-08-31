(ns evoclj.store.command-test
  "A1 — CommandSchema (store/command.clj) + continuation EDN round-trip."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.command :as cmd]))

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
