(ns evoclj.eval.static
  "Kernel-owned deterministic rule registry and guarded generated-invariant
  publication. Candidate data can never register, replace, disable, or execute
  a suite. Active invariants are closed descriptors only; executable behavior is
  always supplied by an already registered kernel rule."
  (:require [evoclj.evolution.invariant :as invariant]
            [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err]))

(def ^:private suite-types #{:unit :property})
(def ^:private protected-gates
  #{:G0-parse :G1-schema-abi :G2-static-policy :G3-deterministic-suites})
(def ^:private suites (atom []))
(def ^:private active (atom {}))

(defn- check-suite-shape! [suite]
  (when-not (map? suite)
    (throw (err/error :eval/suite-invalid "a suite must be a map" {:reason :not-a-map :value (err/sanitize suite)})))
  (doseq [[k present?] [[:suite/id (contains? suite :suite/id)]
                        [:suite/type (contains? suite :suite/type)]
                        [:check (contains? suite :check)]]]
    (when-not present?
      (throw (err/error :eval/suite-invalid "suite is missing a required key" {:reason :missing-key :key k :value (err/sanitize suite)}))))
  (when-not (keyword? (:suite/id suite))
    (throw (err/error :eval/suite-invalid ":suite/id must be a keyword" {:reason :bad-suite-id :value (err/sanitize (:suite/id suite))})))
  (when-not (contains? suite-types (:suite/type suite))
    (throw (err/error :eval/suite-invalid ":suite/type must be :unit or :property" {:reason :bad-suite-type :value (err/sanitize (:suite/type suite))})))
  (when-not (fn? (:check suite))
    (throw (err/error :eval/suite-invalid ":check must be a deterministic function" {:reason :bad-check :value (err/sanitize (:check suite))}))))

(defn register-suite! [suite]
  (check-suite-shape! suite)
  (when (some #(= (:suite/id suite) (:suite/id %)) @suites)
    (throw (err/error :eval/suite-invalid "a suite with this id is already registered" {:reason :duplicate-id :suite/id (:suite/id suite)})))
  (swap! suites conj suite)
  suite)

(defn registered-suites [] @suites)

(defn registry-revision
  "Deterministic digest of the kernel rule registry."
  []
  (hash/text-digest (pr-str (mapv #(select-keys % [:suite/id :suite/type]) @suites))))

(defn clear-suites! []
  (reset! suites [])
  (reset! active {})
  nil)

(defn- kernel-suite [rule-id]
  (some #(when (= rule-id (:suite/id %)) %) @suites))

(defn- descriptor-key [d]
  [(:invariant/id d) (:version d)])

(defn publish-active-invariant!
  "Kernel-private publication boundary. The durable activation transaction
  must have committed first. This function accepts only a closed descriptor,
  never source code or a function, and never mutates the suite registry."
  [descriptor]
  (when-not (map? descriptor)
    (throw (err/error :invariant/activation-invalid "activation descriptor must be a map" {:reason :not-a-map})))
  (when-not (true? (:activation/committed? descriptor))
    (throw (err/error :invariant/activation-invalid "activation is not durably committed" {:reason :not-committed})))
  (let [p (invariant/validate-predicate! (:predicate descriptor))
        id (:invariant/id descriptor)
        version (:version descriptor)
        revision (:registry/revision descriptor)]
    (when-not (and (or (string? id) (keyword? id)) (pos-int? version))
      (throw (err/error :invariant/activation-invalid "activation descriptor identity is malformed" {:reason :identity-invalid})))
    (when-not (= revision (registry-revision))
      (throw (err/error :invariant/activation-invalid "kernel rule registry revision is stale" {:reason :registry-revision-stale :expected (registry-revision) :actual revision})))
    (when (and (= :kernel-rule (:predicate/type p))
               (contains? protected-gates (:rule/id p)))
      (throw (err/error :invariant/activation-invalid "generated invariants cannot mutate G0-G3" {:reason :protected-gate :rule/id (:rule/id p)})))
    (when (and (= :kernel-rule (:predicate/type p)) (nil? (kernel-suite (:rule/id p))))
      (throw (err/error :invariant/activation-invalid "kernel rule is not registered" {:reason :unknown-kernel-rule :rule/id (:rule/id p)})))
    (let [k (descriptor-key descriptor)
          prior (get @active k)]
      (let [prior-digest (get-in prior [:predicate :predicate/digest])]
        (when (and prior (not= prior-digest (:predicate/digest p)))
          (throw (err/error :invariant/activation-conflict "activation version already names another predicate"
                            {:reason :version-conflict :key k
                             :prior-digest prior-digest
                             :predicate-digest (:predicate/digest p)}))))
      (swap! active assoc k (assoc descriptor :predicate p :published? true))
      (get @active k))))

(defn active-invariants []
  (->> @active vals (sort-by (juxt :invariant/id :version)) vec))

(defn- path-value [ctx path]
  (reduce (fn [v k]
            (cond
              (map? v) (get v k ::missing)
              (vector? v) (if (and (integer? k) (< -1 k (count v))) (nth v k) ::missing)
              :else ::missing)) ctx path))

(declare dsl-match?)
(defn- dsl-match? [ctx dsl]
  (let [op (:op dsl)]
    (cond
      (= :and op) (every? #(dsl-match? ctx %) (:args dsl))
      (= :or op) (some #(dsl-match? ctx %) (:args dsl))
      (= :not op) (not (dsl-match? ctx (:arg dsl)))
      (= :equals op) (= (path-value ctx (:path dsl)) (:value dsl))
      (= :in op) (some #{(path-value ctx (:path dsl))} (:values dsl))
      (= :present op) (not= ::missing (path-value ctx (:path dsl)))
      (= :absent op) (= ::missing (path-value ctx (:path dsl)))
      :else false)))

(defn run-active-invariants
  "Evaluate active descriptors using only kernel suites or the closed DSL.
  Any failure is a hard kernel failure; absent active descriptors preserve the
  pre-feature behavior."
  [ctx]
  (mapv (fn [d]
          (let [p (:predicate d)
                outcome (if (= :kernel-rule (:predicate/type p))
                          ((:check (kernel-suite (:rule/id p))) ctx)
                          (when-not (dsl-match? ctx (:dsl p)) {:predicate p}))]
            (when outcome
              (throw (err/error :kernel/invariant-failure
                                "active invariant rejected kernel state"
                                {:invariant/id (:invariant/id d)
                                 :version (:version d)
                                 :predicate/digest (:predicate/digest p)
                                 :failure (err/sanitize outcome)})))
            {:invariant/id (:invariant/id d) :version (:version d) :status :pass}))
        (active-invariants)))

(defn clear-active-invariants! []
  (reset! active {})
  nil)
(defn disable-active-invariant!
  "Remove a published descriptor only after a durable disable decision.
  Historical proposal, approval, and activation rows remain untouched."
  [proposal-id]
  (swap! active (fn [m]
                  (into {} (remove (fn [[_ d]] (= (str proposal-id) (str (:proposal/id d)))) m))))
  nil)
