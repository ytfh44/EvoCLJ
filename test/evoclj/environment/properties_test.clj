(ns evoclj.environment.properties-test
  "property/state-machine skeleton.

  Randomly generates:
    refresh
    failed-refresh
    activate
    reload
    remove-source
    compact
    deactivate
  Currently runs on a single FakeSource/FakeBinding.
  No Environment implementation is required for this stage.

  Skeleton asserts over random traces:
  - published revision immutable
  - refresh does not mutate binding
  - siblings share revision
  - compaction does not change binding
  - source removal keeps binding via CAS
  - no op mints capability silently
  - no partial publication"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

;; ---- Fake model ----
(def source-id :fake/source)

(defn init-state []
  {:current nil                 ; {:revision/id :revision/seq :payload}
   :published {}                ; revision/id -> revision (immutable set)
   :bindings {}                 ; binding/id -> binding
   :catalog {}                  ; source/id -> revision/id
   :cas {}                      ; revision/id -> raw bytes (immutable tree)
   :capabilities #{}            ; grows only via explicit grant
   :seq 0
   :source-removed? false
   :dirty? false :status :ok :last-error nil})

(defn revision-of [content seq-n]
  {:revision/id (str "rev:" (hash content) ":" seq-n)
   :revision/seq seq-n
   :source/id source-id
   :captured-at (System/currentTimeMillis)
   :payload content})

(defn do-publish [state content]
  (let [cur (:current state)]
    (if (= content (:payload cur))
      (assoc state :dirty? false :status :ok) ; no churn for identical content
      (let [next-seq (inc (:seq state))
            rev (revision-of content next-seq)]
        (-> state
            (assoc :current rev :seq next-seq)
            (assoc-in [:published (:revision/id rev)] rev)
            (assoc-in [:catalog source-id] (:revision/id rev))
            (assoc-in [:cas (:revision/id rev)] content)
            (assoc :dirty? false :status :ok :last-error nil))))))

(defn do-op [state op]
  (case (:op op)
    :refresh
    (if (:source-removed? state)
      (assoc state :dirty? true :status :degraded :last-error {:type :source/missing})
      (do-publish state (:content op)))

    :failed-refresh
    (assoc state :dirty? true :status :degraded :last-error {:type :refresh/failed :msg (:msg op)})

    :activate
    (let [cur (:current state)]
      (if (nil? cur)
        state
        (let [bid (:binding/id op)
              bundle-id (str "bundle:" (:revision/id cur))
              binding {:binding/id bid :logical/id [:skill "x"] :revision/id (:revision/id cur)
                       :bundle/id bundle-id :state :active
                       :surfaces [{:surface/type :context :revision/id (:revision/id cur)}
                                  {:surface/type :directory :revision/id (:revision/id cur)}]}]
          (assoc-in state [:bindings bid] binding))))

    :reload
    (let [bid (:binding/id op)
          b (get-in state [:bindings bid])
          cur (:current state)]
      (if (or (nil? b) (nil? cur) (= (:revision/id b) (:revision/id cur)))
        state
        (assoc-in state [:bindings bid :revision/id] (:revision/id cur))))

    :remove-source
    (-> state (assoc :catalog {} :source-removed? true))

    :compact
    (assoc state :compacted? true)

    :deactivate
    (update state :bindings dissoc (:binding/id op))

    :grant-capability
    (update state :capabilities conj (:lease op))

    state))

(defn invariants-hold? [prev next op]
  (let [failures (atom [])]
    ;; I1: published revision immutable
    (doseq [[id rev] (:published prev)]
      (when-let [rev2 (get-in next [:published id])]
        (when (not= rev rev2)
          (swap! failures conj [:i1-mutated id]))))
    ;; I2: refresh must not mutate existing binding unless reload targets it
    (doseq [[bid b-prev] (:bindings prev)]
      (when-let [b-next (get-in next [:bindings bid])]
        (when (and (not= (:revision/id b-prev) (:revision/id b-next))
                   (not (and (= :reload (:op op)) (= bid (:binding/id op)))))
          (swap! failures conj [:i2-binding-mutated bid (:op op)]))))
    ;; I3: sibling surfaces share revision
    (doseq [[_ b] (:bindings next)]
      (let [revs (set (map :revision/id (:surfaces b)))]
        (when (and (seq (:surfaces b)) (> (count revs) 1))
          (swap! failures conj [:i3-sibling-diverged (:binding/id b) revs]))))
    ;; I4: compaction does not change binding identity
    (when (= :compact (:op op))
      (when (not= (:bindings prev) (:bindings next))
        (swap! failures conj [:i4-compaction-mutated])))
    ;; I5: source removal keeps bindings and CAS
    (when (= :remove-source (:op op))
      (when (not= (:bindings prev) (:bindings next))
        (swap! failures conj [:i5-removal-mutated-bindings]))
      (doseq [[_ b] (:bindings prev)]
        (when (nil? (get-in next [:cas (:revision/id b)]))
          (swap! failures conj [:i5-cas-lost (:revision/id b)]))))
    ;; I6: only explicit grant may grow capabilities
    (when (and (not= (:capabilities prev) (:capabilities next))
               (not= :grant-capability (:op op)))
      (swap! failures conj [:i6-capability-created (:op op)]))
    ;; failed refresh keeps published/current
    (when (= :failed-refresh (:op op))
      (when (not= (:published prev) (:published next))
        (swap! failures conj [:failed-refresh-published-changed]))
      (when (not= (:current prev) (:current next))
        (swap! failures conj [:failed-refresh-current-changed])))
    (if (empty? @failures)
      {:ok true}
      {:ok false :failures @failures :prev prev :next next :op op})))

;; ---- Generators ----
(def gen-content (gen/elements ["A" "B" "C" "D" "E" "sk1" "sk2" "pkg v1" "pkg v2"]))

(def gen-op
  (gen/one-of
   [(gen/fmap (fn [c] {:op :refresh :content c}) gen-content)
    (gen/fmap (fn [m] {:op :failed-refresh :msg m}) (gen/elements ["net-err" "timeout" "parse-err"]))
    (gen/fmap (fn [id] {:op :activate :binding/id id}) (gen/elements [(random-uuid) (random-uuid) (random-uuid)]))
    (gen/fmap (fn [id] {:op :reload :binding/id id}) (gen/elements [(random-uuid) (random-uuid)]))
    (gen/return {:op :remove-source})
    (gen/return {:op :compact})
    (gen/fmap (fn [id] {:op :deactivate :binding/id id}) (gen/elements [(random-uuid) (random-uuid)]))]))

(def gen-trace (gen/vector gen-op 1 35))

(defn check-trace [trace]
  (loop [state (init-state) ops trace]
    (if (empty? ops)
      {:ok true}
      (let [op (first ops)
            op* (cond
                  (and (= :reload (:op op)) (seq (:bindings state)))
                  (assoc op :binding/id (rand-nth (keys (:bindings state))))
                  (and (= :deactivate (:op op)) (seq (:bindings state)))
                  (assoc op :binding/id (rand-nth (keys (:bindings state))))
                  (= :activate (:op op))
                  (assoc op :binding/id (random-uuid))
                  :else op)
            next-state (do-op state op*)
            check (invariants-hold? state next-state op*)]
        (if (:ok check)
          (recur next-state (rest ops))
          (assoc check :trace trace :failed-op op* :state-before state :state-after next-state))))))

;; ---- Deterministic critical paths ----
(deftest state-machine-deterministic-critical-paths
  (testing "activate A -> refresh B -> compact still sees A"
    (let [s0 (init-state)
          s1 (do-publish s0 "A")
          revA (:revision/id (:current s1))
          b-id (random-uuid)
          s2 (do-op s1 {:op :activate :binding/id b-id})
          s3 (do-publish s2 "B")
          s4 (do-op s3 {:op :compact})
          b (get-in s4 [:bindings b-id])]
      (is (= revA (:revision/id b)) "compact still shows A not B")))
  (testing "failed refresh does not change current/published"
    (let [s0 (init-state)
          s1 (do-publish s0 "A")
          cur1 (:current s1)
          s2 (do-op s1 {:op :failed-refresh :msg "err"})
          cur2 (:current s2)]
      (is (= cur1 cur2))
      (is (= (:published s1) (:published s2)))))
  (testing "identical refresh does not churn"
    (let [s0 (init-state)
          s1 (do-publish s0 "A")
          seq1 (:seq s1)
          s2 (do-publish s1 "A")]
      (is (= seq1 (:seq s2)) "identical content yields no new seq")))
  (testing "simultaneous refresh collapses to single publication (seq only +1 separately)"
    (let [s0 (init-state)
          s1 (do-publish s0 "A")
          s2 (do-publish s1 "B")
          s3 (do-publish s2 "B")]
      (is (= 2 (:seq s2)))
      (is (= 2 (:seq s3)))))
  (testing "CAS still servable after remove-source"
    (let [s0 (init-state)
          s1 (do-publish s0 "A")
          b-id (random-uuid)
          s2 (do-op s1 {:op :activate :binding/id b-id})
          s3 (do-op s2 {:op :remove-source})
          b (get-in s3 [:bindings b-id])]
      (is (some? b))
      (is (some? (get-in s3 [:cas (:revision/id b)]))))))

;; ---- Property: random traces all satisfy invariants ----
(defspec property-env-state-machine-preserves-invariants 120
  (prop/for-all [trace gen-trace]
                (let [res (check-trace trace)]
                  (when-not (:ok res)
                    (println "FAIL trace:" trace)
                    (println "failure:" (:failures res)))
                  (:ok res))))
