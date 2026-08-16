(ns evoclj.capability.broker-test
  "Tests for the pure capability broker decision (Task 4.4).

  authorize is the NORMATIVE broker entry point of Milestone 4: a
  pure function of plain data that composes the policy module
  (evoclj.capability.policy) with the lease model
  (evoclj.capability.lease / .schema) into a deterministic
  {:decision :allow :lease-id ...} / {:decision :deny :reason ...}
  decision. It performs no I/O, so authorization can be tested
  without invoking any provider effect (Task 4.4 acceptance); the
  only providers touched are the Task 4.3 fixtures used to PRODUCE
  normalized requests — exactly the 4.5 dispatcher pipeline minus the
  effect.

  Step 1 asserts allow with an exact capability and deny with no
  lease, including deterministic lease selection when several leases
  exist and order-independence of the lease collection. Step 2
  asserts the Global Constraint 9 rule at the broker level: a tool
  that is REGISTERED and EXPOSED (its provider can normalize a real
  request for it) is still denied with :capability/missing until a
  lease grants it. Step 3 asserts each stable deny reason — expired
  window, wrong phenotype, wrong action, exhausted :max-calls, and a
  canonical resource outside the granted scope — plus the call-budget
  and window boundaries, and that filesystem scope is decided on the
  canonical resolved path. Step 4 asserts the decision is a pure
  deterministic function of its inputs: lease order never changes a
  decision, deny reasons are the documented keywords, decisions
  round-trip through EDN, and malformed input is rejected rather than
  silently judged. Step 5 asserts monotonicity: removing leases from
  an authorization input can never turn a prior deny into an allow —
  exhaustively over a fixed deny config's lease subsets and over
  seeded-randomized lease subsets of a mixed pool. Step 6 adds the
  S3 (roadmap) per-model and per-tool lease denial cases: an exact
  model grant allows model A and denies model B with
  :capability/scope-denied, a wildcard \"<provider>/*\" model grant
  covers only models inside the provider prefix, a model grant never
  covers a tool resource (and vice versa), and the window-expiry and
  call-budget edges hold for BOTH tool and model leases — each case
  asserts the stable deny code."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.capability.broker :as broker]
            [evoclj.intent.core :as intent]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private phenotype-p2
  "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
(def ^:private cause-event-id 42)
(def ^:private budget {:wall-ms 1000})

(def ^:private issued-at (java.util.Date. 1700000000000))
(def ^:private expires-at (java.util.Date. 1700003600000)) ; issued-at + 1h
(def ^:private in-window (java.util.Date. 1700001800000))
(def ^:private after-expiry (java.util.Date. 1700003600001))

(def ^:private echo-cap-id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")

(def ^:private echo-intent
  (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                    {:tool/id :fixture/echo :args {:text "hi"}}
                    budget))

(def ^:private echo-request
  {:tool/id :fixture/echo
   :resource {:kind :tool :id :fixture/echo}
   :args {:text "hi"}})

(defn- lease
  "A valid :fixture/echo lease for phenotype-p1, optionally with
  assoc-style overrides."
  [& kvs]
  (let [base {:cap/id echo-cap-id
              :subject {:phenotype/id phenotype-p1}
              :resource {:kind :tool :id :fixture/echo}
              :actions #{:invoke}
              :constraints {:max-calls 10}
              :issued-at issued-at
              :expires-at expires-at}]
    (if (seq kvs) (apply assoc base kvs) base)))

(defn- decision
  "Run the broker decision for echo-request with the given leases,
  usage, and instant (defaults: no usage consumed, in-window
  instant)."
  ([leases] (decision leases {} in-window))
  ([leases usage] (decision leases usage in-window))
  ([leases usage now]
   (broker/authorize {:intent echo-intent
                      :normalized-request echo-request
                      :leases leases
                      :usage usage
                      :now now})))

;; --- S3 (roadmap) model fixtures -------------------------------------------

(def ^:private model-a-id "deepseek/deepseek-v4-flash")
(def ^:private model-b-id "anthropic/claude-sonnet-4-5")

(def ^:private model-cap-id #uuid "99999999-9999-4999-8999-999999999999")

(defn- model-lease
  "A valid model lease for phenotype-p1 on model-a-id, optionally
  with assoc-style overrides."
  [& kvs]
  (let [base {:cap/id model-cap-id
              :subject {:phenotype/id phenotype-p1}
              :resource {:kind :model :id model-a-id}
              :actions #{:invoke}
              :constraints {:max-calls 10}
              :issued-at issued-at
              :expires-at expires-at}]
    (if (seq kvs) (apply assoc base kvs) base)))

(defn- model-decision
  "Run the broker decision for a model-call intent on model-id with
  the given leases, usage, and instant (defaults: no usage consumed,
  in-window instant)."
  ([leases model-id] (model-decision leases model-id {}))
  ([leases model-id usage] (model-decision leases model-id usage in-window))
  ([leases model-id usage now]
   (broker/authorize {:intent (intent/model-call
                               session-id phenotype-p1 :node/planner
                               cause-event-id
                               {:model/id model-id
                                :messages [{:role :user :content "hi"}]}
                               budget)
                      :normalized-request {:model/id model-id
                                           :resource {:kind :model :id model-id}
                                           :messages [{:role :user :content "hi"}]}
                      :leases leases
                      :usage usage
                      :now now})))

(defn- allow-decision? [d] (= :allow (:decision d)))

(defn- schema-invalid?
  "True when the thunk f throws :capability/schema-invalid."
  [f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e
         (= :capability/schema-invalid (:error/type (ex-data e))))))

;; ============================================================================
;; Step 1 — allow with exact capability, deny with no lease
;; ============================================================================

(deftest allow-with-exact-capability
  (testing "the exact grant allows and reports its lease id"
    (let [d (decision [(lease)])]
      (is (allow-decision? d))
      (is (= echo-cap-id (:lease-id d)))))
  (testing "the decision is a plain immutable EDN value"
    (let [d (decision [(lease)])]
      (is (= d (edn/read-string (pr-str d))))))
  (testing "when several leases exist, the covering one allows and is reported"
    (let [other (lease :cap/id #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"
                       :subject {:phenotype/id phenotype-p2})
          d (decision [(lease) other])]
      (is (allow-decision? d))
      (is (= echo-cap-id (:lease-id d)))))
  (testing "lease collection order never changes the decision"
    (let [a (lease :cap/id #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc")
          b (lease :cap/id #uuid "dddddddd-dddd-4ddd-8ddd-dddddddddddd")
          c (lease)]
      (is (= (decision [a b c]) (decision [c b a])))
      (is (= (decision [b (lease)]) (decision [(lease) b]))))))

(deftest deny-with-no-lease
  (testing "no lease means deny with the documented missing-capability reason"
    (is (= {:decision :deny :reason :capability/missing} (decision []))))
  (testing "nil leases and nil usage default to no grant / no usage"
    (is (= {:decision :deny :reason :capability/missing}
           (broker/authorize {:intent echo-intent
                              :normalized-request echo-request
                              :leases nil :usage nil :now in-window})))))

;; ============================================================================
;; Step 2 — exposed-but-ungranted tools are never authorized
;; ============================================================================

(deftest exposed-but-ungranted-tool-is-denied
  (testing "registering and exposing a tool never grants it (Global Constraint 9)"
    (let [reg (registry/create-registry)
          provider (fixture/echo-provider)]
      (registry/register! reg provider)
      (is (some? (registry/lookup reg :fixture/echo)) "the tool is exposed")
      (let [normalized (proto/normalize-request provider echo-intent)]
        (is (= {:kind :tool :id :fixture/echo} (:resource normalized))
            "a request for the exposed tool normalizes to a real resource")
        (is (= {:decision :deny :reason :capability/missing}
               (broker/authorize {:intent echo-intent
                                  :normalized-request normalized
                                  :leases []
                                  :usage {}
                                  :now in-window}))
            "a visible, requestable tool is denied until a lease grants it")))))

;; ============================================================================
;; Step 3 — deterministic deny reasons
;; ============================================================================

(deftest deterministic-deny-reasons
  (testing "an expired lease denies with :capability/expired"
    (is (= :capability/expired (:reason (decision [(lease)] {} after-expiry)))))
  (testing "a lease for another phenotype denies with :capability/subject-mismatch"
    (is (= :capability/subject-mismatch
           (:reason (decision [(lease :subject {:phenotype/id phenotype-p2})])))))
  (testing "a grant that does not include the requested action denies with :capability/action-denied"
    (is (= :capability/action-denied
           (:reason (decision [(lease :actions #{:read})]))))
    (is (= :capability/action-denied
           (:reason (decision [(lease :actions #{:read :delete})])))))
  (testing "a covering action but an out-of-scope resource denies with :capability/scope-denied"
    (is (= :capability/scope-denied
           (:reason (decision [(lease :resource {:kind :tool :id :fixture/other})])))))
  (testing "an exhausted :max-calls denies with :capability/budget-exceeded"
    (is (= :capability/budget-exceeded
           (:reason (decision [(lease :constraints {:max-calls 2})] {echo-cap-id 2})))))
  (testing "the call-budget boundary is exact: consumed < max-calls allows"
    (is (allow-decision? (decision [(lease :constraints {:max-calls 2})] {echo-cap-id 1})))
    (is (allow-decision? (decision [(lease :constraints {:max-calls 2})] {echo-cap-id 0})))
    (is (allow-decision? (decision [(lease :constraints {:max-calls 10})] {echo-cap-id 9})))
    (is (= :capability/budget-exceeded
           (:reason (decision [(lease :constraints {:max-calls 10})] {echo-cap-id 10})))))
  (testing "an absent :max-calls is unlimited"
    (is (allow-decision? (decision [(lease :constraints {})] {echo-cap-id 999}))))
  (testing "the window boundaries hold at the broker level"
    (is (allow-decision? (decision [(lease)] {} issued-at)))
    (is (= :capability/expired (:reason (decision [(lease)] {} expires-at))))))

(deftest filesystem-scope-is-decided-on-canonical-paths
  (let [fs-lease {:cap/id #uuid "eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee"
                  :subject {:phenotype/id phenotype-p1}
                  :resource {:kind :filesystem :path "/protected/work"}
                  :actions #{:invoke}
                  :constraints {:max-calls 10}
                  :issued-at issued-at
                  :expires-at expires-at}
        fs-intent (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                                    {:tool/id :fixture/path-resolve
                                     :args {:path "a/../secret"}}
                                    budget)
        escape-intent (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                                        {:tool/id :fixture/path-resolve
                                         :args {:path "/etc/passwd"}}
                                        budget)]
    (testing "a traversal inside the granted root normalizes INSIDE the scope and is allowed"
      (let [normalized (proto/normalize-request (fixture/path-resolve-provider) fs-intent)]
        (is (= {:kind :filesystem :path "/protected/work/secret"} (:resource normalized)))
        (is (allow-decision?
             (broker/authorize {:intent fs-intent :normalized-request normalized
                                :leases [fs-lease] :usage {} :now in-window})))))
    (testing "an absolute path outside the root denies with :capability/scope-denied"
      (let [normalized (proto/normalize-request (fixture/path-resolve-provider) escape-intent)]
        (is (= {:kind :filesystem :path "/etc/passwd"} (:resource normalized)))
        (is (= :capability/scope-denied
               (:reason (broker/authorize {:intent escape-intent
                                           :normalized-request normalized
                                           :leases [fs-lease] :usage {} :now in-window}))))))
    (testing "a tool grant never covers a filesystem request"
      (is (= :capability/scope-denied
             (:reason (broker/authorize {:intent fs-intent
                                         :normalized-request
                                         {:tool/id :fixture/path-resolve
                                          :resource {:kind :filesystem
                                                     :path "/protected/work/secret"}}
                                         :leases [(lease)] :usage {} :now in-window})))))))

;; ============================================================================
;; Step 4 — deterministic decision, schema-gated inputs
;; ============================================================================

(deftest decision-is-deterministic-and-schema-gated
  (testing "reordering a deny set never changes the reported reason"
    (let [p1 (lease :cap/id #uuid "11111111-1111-4111-8111-111111111111"
                    :resource {:kind :tool :id :fixture/other})
          p2 (lease :cap/id #uuid "22222222-2222-4222-8222-222222222222"
                    :subject {:phenotype/id phenotype-p2})]
      (is (= (decision [p1 p2] {}) (decision [p2 p1] {})))
      (is (not (allow-decision? (decision [p1 p2] {}))))))
  (testing "deny decisions round-trip through EDN"
    (let [d (decision [(lease :actions #{:read})])]
      (is (= d (edn/read-string (pr-str d))))))
  (testing "a malformed normalized request is rejected, never silently judged"
    (is (schema-invalid?
         #(broker/authorize {:intent echo-intent
                             :normalized-request {}
                             :leases [(lease)] :usage {} :now in-window})))
    (is (schema-invalid?
         #(broker/authorize {:intent echo-intent
                             :normalized-request echo-request
                             :leases "not-a-collection" :usage {} :now in-window}))))
  (testing "malformed usage, instant, and lease are rejected"
    (is (schema-invalid?
         #(broker/authorize {:intent echo-intent :normalized-request echo-request
                             :leases [(lease)] :usage {echo-cap-id -1} :now in-window})))
    (is (schema-invalid?
         #(broker/authorize {:intent echo-intent :normalized-request echo-request
                             :leases [(lease)] :usage {} :now 1700001800000})))
    (is (schema-invalid?
         #(broker/authorize {:intent echo-intent :normalized-request echo-request
                             :leases [(dissoc (lease) :expires-at)] :usage {} :now in-window}))))
  (testing "a malformed intent is rejected with the intent error type"
    (let [e (try (broker/authorize {:intent {:intent/type :intent/tool-call}
                                    :normalized-request echo-request
                                    :leases [] :usage {} :now in-window})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (= :intent/schema-invalid (:error/type (ex-data e)))))))

;; ============================================================================
;; Step 5 — removing leases never turns a deny into an allow
;; ============================================================================

(defn- all-subsets
  "Every subset of coll, enumerated by bitmask (the empty subset
  first)."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (for [mask (range (bit-shift-left 1 n))]
      (into [] (keep-indexed (fn [i x] (when (bit-test mask i) x)) v)))))

(defn- random-subset
  "A uniformly random subset of coll, drawn with the given (fixed-seed)
  java.util.Random so the property run is reproducible."
  [rnd coll]
  (into [] (filter (fn [_] (.nextBoolean rnd)) coll)))

(deftest removing-leases-never-turns-deny-into-allow
  (let [covering (lease)
        deny-pool [(lease :cap/id #uuid "33333333-3333-4333-8333-333333333333"
                          :resource {:kind :tool :id :fixture/other})
                   (lease :cap/id #uuid "44444444-4444-4444-8444-444444444444"
                          :subject {:phenotype/id phenotype-p2})
                   (lease :cap/id #uuid "55555555-5555-4555-8555-555555555555"
                          :actions #{:read})
                   (lease :cap/id #uuid "66666666-6666-4666-8666-666666666666"
                          :constraints {:max-calls 0})
                   (lease :cap/id #uuid "77777777-7777-4777-8777-777777777777"
                          :issued-at (java.util.Date. 1000000000000)
                          :expires-at (java.util.Date. 1000003600000))]
        full-deny (decision deny-pool {})]
    (testing "base configurations: with-lease allows, no-lease denies"
      (is (allow-decision? (decision [covering])))
      (is (= {:decision :deny :reason :capability/missing} (decision []))))
    (testing "a fixed deny config denies with a deterministic reason"
      (is (not (allow-decision? full-deny)))
      (is (= :capability/budget-exceeded (:reason full-deny))
          "the furthest-progress lease determines the reason")
      (is (= full-deny (decision (reverse deny-pool) {}))
          "lease order never changes a deny decision"))
    (testing "removing leases can never turn the fixed deny into an allow (exhaustive subsets)"
      (doseq [s (all-subsets deny-pool)]
        (is (not (allow-decision? (decision s {})))
            (str "subset must deny: " (pr-str s)))))
    (testing "randomized lease subsets of the fixed deny config also all deny (seeded)"
      (let [rnd (java.util.Random. 7)]
        (dotimes [_ 100]
          (is (not (allow-decision? (decision (random-subset rnd deny-pool) {})))))))
    (testing "randomized lease subsets of a mixed pool are monotone (seeded)"
      (let [pool (conj deny-pool covering)
            full (decision pool {})
            rnd (java.util.Random. 42)]
        (is (allow-decision? full))
        (is (= echo-cap-id (:lease-id full)))
        (dotimes [_ 200]
          (let [s (random-subset rnd pool)
                ds (decision s {})]
            (when (allow-decision? ds)
              (is (= echo-cap-id (:lease-id ds))
                  "an allow always comes from the covering lease")
              (is (allow-decision? full)
                  "adding leases back can never revoke an allow"))))))))

;; ============================================================================
;; Step 6 — S3: per-model and per-tool lease denial cases (roadmap S3)
;; ============================================================================

(deftest per-model-lease-denial
  (testing "model A allowed / model B denied under an exact model lease"
    (let [d-a (model-decision [(model-lease)] model-a-id)
          d-b (model-decision [(model-lease)] model-b-id)]
      (is (allow-decision? d-a))
      (is (= model-cap-id (:lease-id d-a)))
      (is (= :capability/scope-denied (:reason d-b))
          "a lease for model A never grants model B")))
  (testing "a wildcard model lease covers models inside the provider prefix, denies outside"
    (let [wild (model-lease :resource {:kind :model :id "deepseek/*"})]
      (is (allow-decision? (model-decision [wild] model-a-id)))
      (is (= :capability/scope-denied (:reason (model-decision [wild] model-b-id)))
          "a different provider is outside the wildcard prefix")))
  (testing "a model lease never covers a tool resource, and vice versa (kind mismatch)"
    (is (= :capability/scope-denied (:reason (decision [(model-lease)])))
        "a model lease cannot authorize a tool call")
    (is (= :capability/scope-denied (:reason (model-decision [(lease)] model-a-id)))
        "a tool lease cannot authorize a model call")))

(deftest per-tool-lease-denial
  (testing "tool X allowed / tool Y denied under an exact tool lease"
    (is (allow-decision? (decision [(lease)]))
        "a lease for :fixture/echo allows :fixture/echo")
    (let [path-intent (intent/tool-call session-id phenotype-p1 :node/tool
                                        cause-event-id
                                        {:tool/id :fixture/path-resolve
                                         :args {:path "a"}}
                                        budget)
          path-request {:tool/id :fixture/path-resolve
                        :resource {:kind :tool :id :fixture/path-resolve}
                        :args {:path "a"}}]
      (is (= :capability/scope-denied
             (:reason (broker/authorize {:intent path-intent
                                         :normalized-request path-request
                                         :leases [(lease)]
                                         :usage {}
                                         :now in-window})))
          "a lease for :fixture/echo never grants :fixture/path-resolve"))))

(deftest window-expiry-denies-with-stable-code
  (testing "a tool lease is valid at :issued-at and denies with :capability/expired at and after :expires-at"
    (is (allow-decision? (decision [(lease)] {} issued-at)))
    (is (= :capability/expired (:reason (decision [(lease)] {} expires-at))))
    (is (= :capability/expired (:reason (decision [(lease)] {} after-expiry)))))
  (testing "a model lease follows the same window contract"
    (is (allow-decision? (model-decision [(model-lease)] model-a-id {} issued-at)))
    (is (= :capability/expired (:reason (model-decision [(model-lease)] model-a-id {} expires-at))))
    (is (= :capability/expired (:reason (model-decision [(model-lease)] model-a-id {} after-expiry))))))

(deftest call-budget-edge-exactly-at-max
  (testing "the tool lease admits the call AT max-1 and denies AT max with :capability/budget-exceeded"
    (is (allow-decision? (decision [(lease :constraints {:max-calls 2})] {echo-cap-id 1})))
    (is (= :capability/budget-exceeded
           (:reason (decision [(lease :constraints {:max-calls 2})] {echo-cap-id 2}))))
    (is (= :capability/budget-exceeded
           (:reason (decision [(lease :constraints {:max-calls 2})] {echo-cap-id 3})))))
  (testing "the model lease has the same exact edge"
    (is (allow-decision? (model-decision [(model-lease :constraints {:max-calls 2})]
                                         model-a-id {model-cap-id 1})))
    (is (= :capability/budget-exceeded
           (:reason (model-decision [(model-lease :constraints {:max-calls 2})]
                                    model-a-id {model-cap-id 2}))))
    (is (= :capability/budget-exceeded
           (:reason (model-decision [(model-lease :constraints {:max-calls 2})]
                                    model-a-id {model-cap-id 3}))))))
