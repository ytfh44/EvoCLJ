(ns evoclj.evolution.invariant
  "Pure, closed data and transitions for self-generated invariants.

  This namespace deliberately has no registry, IO, database, CAS, or executable
  generated code. A proposal is evidence; only the kernel may interpret an
  already-registered rule or the small declarative predicate language."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err]))

(def statuses #{:proposed :rejected :approved :active :disabled :quarantined})
(def run-kinds #{:counterexample :replay :adversarial})
(def scopes #{:session :generation :candidate :effect :tool :model :runtime})
(def risks #{:low :medium :high :critical})

(def transitions
  {:proposed #{:rejected :approved :quarantined}
   :rejected #{}
   :approved #{:active :rejected}
   :active #{:disabled :quarantined}
   :disabled #{}
   :quarantined #{}})

(def ^:private digest-re #"^sha256:[0-9a-f]{64}$")
(def ^:private forbidden-keys
  #{:fn :function :code :source :eval :eval-string :form :var :class :object})
(def ^:private dsl-ops #{:and :or :not :equals :in :present :absent})

(defn- fail! [reason message data]
  (throw (err/error :invariant/invalid message (assoc data :reason reason))))

(defn- digest? [x]
  (and (string? x) (boolean (re-matches digest-re x))))

(defn- ref? [x]
  (or (digest? x)
      (keyword? x)
      (uuid? x)
      (and (string? x) (not (str/blank? x)))))

(defn- refs! [key xs]
  (when-not (and (sequential? xs) (every? ref? xs))
    (fail! :malformed-ref (str key " must be a sequence of non-empty refs") {:key key :value (err/sanitize xs)}))
  (vec xs))

(defn- safe-value? [x depth]
  (if (> depth 16)
    false
    (cond
      (or (nil? x) (boolean? x) (number? x) (string? x) (keyword? x) (uuid? x)) true
      (vector? x) (every? #(safe-value? % (inc depth)) x)
      (set? x) (every? #(safe-value? % (inc depth)) x)
      (map? x) (and (every? #(or (keyword? %) (string? %)) (keys x))
                 (every? #(safe-value? % (inc depth)) (vals x)))
      :else false)))

(defn- reject-forbidden! [x]
  (when (and (map? x) (seq (set/intersection forbidden-keys (set (keys x)))))
    (fail! :executable-predicate "predicate contains executable or opaque data" {:value (err/sanitize x)}))
  (cond
    (map? x) (doseq [[_ v] x] (reject-forbidden! v))
    (sequential? x) (doseq [v x] (reject-forbidden! v))
    :else nil)
  x)

(defn- path-valid? [p]
  (and (sequential? p) (seq p) (every? #(or (keyword? %) (string? %) (integer? %)) p)))

(defn- validate-dsl! [dsl]
  (reject-forbidden! dsl)
  (when-not (map? dsl)
    (fail! :dsl-invalid "declarative predicate must be a map" {:value (err/sanitize dsl)}))
  (let [op (:op dsl)]
    (when-not (contains? dsl-ops op)
      (fail! :dsl-op-unknown "declarative predicate operation is not allowed" {:op op}))
    (when-not (safe-value? dsl 0)
      (fail! :dsl-opaque "declarative predicate contains an unsupported value" {:value (err/sanitize dsl)}))
    (cond
      (contains? #{:and :or} op)
      (when-not (and (sequential? (:args dsl)) (seq (:args dsl)))
        (fail! :dsl-invalid "and/or require non-empty args" {:value (err/sanitize dsl)}))
      (= :not op)
      (when-not (map? (:arg dsl))
        (fail! :dsl-invalid "not requires one predicate" {:value (err/sanitize dsl)}))
      (contains? #{:equals :in} op)
      (do
        (when-not (path-valid? (:path dsl))
          (fail! :dsl-invalid "equals/in requires a non-empty path" {:value (err/sanitize dsl)}))
        (when (and (= op :in) (not (sequential? (:values dsl))))
          (fail! :dsl-invalid "in requires values" {:value (err/sanitize dsl)})))
      (contains? #{:present :absent} op)
      (when-not (path-valid? (:path dsl))
        (fail! :dsl-invalid "present/absent requires a non-empty path" {:value (err/sanitize dsl)}))))
  dsl)

(defn validate-predicate!
  "Validate and return a closed predicate descriptor. No Clojure function,
  source, eval string, or unknown kernel rule can cross this boundary; rule
  existence is checked by the kernel activation seam."
  [predicate]
  (reject-forbidden! predicate)
  (when-not (map? predicate)
    (fail! :predicate-invalid "predicate must be a map" {:value (err/sanitize predicate)}))
  (let [kind (or (:predicate/type predicate) (:type predicate))]
    (cond
      (= :kernel-rule kind)
      (when-not (keyword? (:rule/id predicate))
        (fail! :rule-invalid "kernel rule predicate requires a keyword :rule/id" {:value (err/sanitize predicate)}))
      (= :dsl kind)
      (do
        (validate-dsl! (:dsl predicate))
        (when (and (:digest predicate) (not (digest? (:digest predicate))))
          (fail! :digest-invalid "predicate digest must be a sha256 digest" {:value (err/sanitize (:digest predicate))})))
      :else
      (fail! :predicate-kind-unknown "predicate type is not supported" {:predicate/type kind})))
  (when-not (safe-value? predicate 0)
    (fail! :predicate-opaque "predicate is not closed EDN data" {:value (err/sanitize predicate)}))
  (assoc predicate
         :predicate/type (or (:predicate/type predicate) (:type predicate))
         :predicate/digest
         (or (:predicate/digest predicate)
             (:digest predicate)
             (hash/text-digest (pr-str (dissoc predicate :predicate/digest :digest :type))))))

(defn proposal
  "Validate and normalize a proposal. Evidence and validation decisions are
  immutable records; status is always :proposed at creation."
  [p]
  (when-not (map? p)
    (fail! :proposal-invalid "proposal must be a map" {:value (err/sanitize p)}))
  (reject-forbidden! p)
  (doseq [k [:proposer :scope :risk :version :registry/revision]]
    (when-not (contains? p k) (fail! :missing-key "proposal is missing a required key" {:key k})))
  (when-not (ref? (:proposer p)) (fail! :proposer-invalid "proposer must be a stable identity" {:value (err/sanitize (:proposer p))}))
  (when-not (contains? scopes (:scope p)) (fail! :scope-unknown "unknown invariant scope" {:scope (:scope p)}))
  (when-not (contains? risks (:risk p)) (fail! :risk-invalid "risk must be one of the closed risk levels" {:risk (:risk p)}))
  (when-not (and (integer? (:version p)) (pos? (:version p))) (fail! :version-invalid "version must be a positive integer" {:version (:version p)}))
  (when-not (ref? (:registry/revision p)) (fail! :revision-invalid "registry revision must be a stable ref" {:value (err/sanitize (:registry/revision p))}))
  (let [predicate (validate-predicate! (:predicate p))
        evidence (refs! :evidence/refs (or (:evidence/refs p) []))
        replay (refs! :replay/refs (or (:replay/refs p) []))
        adversarial (refs! :adversarial/refs (or (:adversarial/refs p) []))]
    (when (empty? evidence) (fail! :missing-evidence "proposal requires evidence refs" {}))
    (when (empty? replay) (fail! :missing-replay "proposal requires historical replay refs" {}))
    (when (empty? adversarial) (fail! :missing-adversarial "proposal requires adversarial refs" {}))
    (-> p
        (assoc :predicate predicate
               :evidence/refs evidence
               :replay/refs replay
               :adversarial/refs adversarial
               :reviewer (or (:reviewer p) nil)
               :status :proposed)
        (dissoc :fn :function :code :source :eval :eval-string))))

(defn transition
  "Pure admissible status transition. Returns the new status."
  [from to]
  (when-not (contains? statuses from) (fail! :status-unknown "unknown invariant status" {:status from}))
  (when-not (contains? statuses to) (fail! :status-unknown "unknown invariant status" {:status to}))
  (when-not (contains? (get transitions from) to)
    (fail! :transition-invalid "invariant status transition is not admissible" {:from from :to to}))
  to)

(defn run
  "Normalize immutable evaluation evidence. Fresh-model runs never qualify."
  [r]
  (let [r (cond-> r
            (and (map? r) (contains? r :result/ref) (not (contains? r :result-ref)))
            (assoc :result-ref (:result/ref r)))]
    (when-not (map? r) (fail! :run-invalid "run must be a map" {:value (err/sanitize r)}))
    (doseq [k [:run/id :proposal/id :kind :result-ref]]
      (when-not (contains? r k) (fail! :missing-key "run is missing a required key" {:key k})))
  (when-not (ref? (:run/id r)) (fail! :ref-invalid "run id is malformed" {:key :run/id}))
  (when-not (ref? (:proposal/id r)) (fail! :ref-invalid "proposal id is malformed" {:key :proposal/id}))
  (when-not (contains? run-kinds (:kind r)) (fail! :run-kind-invalid "unknown run kind" {:kind (:kind r)}))
  (when-not (ref? (:result-ref r)) (fail! :malformed-ref "run result ref is malformed" {:value (err/sanitize (:result-ref r))}))
  (when-not (safe-value? r 0) (fail! :run-opaque "run contains non-EDN data" {:value (err/sanitize r)}))
  (assoc r :activation-qualified? (and (= :recorded (:model/policy r))
                                       (true? (:deterministic? r))
                                       (not (true? (:fresh-model? r)))
                                       (true? (:passed? r))))))

(defn approval
  "Create an immutable approval decision. It requires distinct proposer and
  reviewer, complete evidence, deterministic replay, and adversarial G3 pass.
  Fresh-model evidence is never activation-qualified unless a signed explicit
  exception is supplied."
  [p a runs]
  (let [p (proposal p)]
    (when-not (map? a) (fail! :approval-invalid "approval must be a map" {:value (err/sanitize a)}))
    (doseq [k [:reviewer :replay/ref :adversarial/ref]]
      (when-not (contains? a k) (fail! :missing-key "approval is missing a required key" {:key k})))
    (when-not (ref? (:reviewer a)) (fail! :reviewer-invalid "reviewer must be a stable identity" {}))
    (when (= (str (:reviewer a)) (str (:proposer p))) (fail! :reviewer-conflict "proposer cannot approve its own invariant" {}))
    (let [replay (filter #(= :replay (:kind %)) runs)
          adversarial (filter #(= :adversarial (:kind %)) runs)
          counterexample? (some #(= :counterexample (:kind %)) runs)
          deterministic? (some #(and (:activation-qualified? %) (:passed? %)) replay)
          adversarial? (some #(and (:activation-qualified? %) (:passed? %)
                                   (= :adversarial (:kind %))
                                   (= :g3 (:gate %))) adversarial)
          signed-exception? (and (map? (:signed-exception a))
                                 (ref? (get-in a [:signed-exception :signature]))
                                 (true? (get-in a [:signed-exception :allow-fresh?])))]
      (when-not (and (seq (:evidence/refs p)) deterministic? (or adversarial? signed-exception?) (not counterexample?))
        (fail! :evidence-insufficient "approval requires passing deterministic replay and adversarial G3 evidence with no passing counterexample" {:deterministic? deterministic? :adversarial? adversarial? :counterexample? counterexample?}))
      (assoc a :proposal/id (or (:proposal/id p) (:proposal/id a))
             :status :approved
             :activation-qualified? (and deterministic? (or adversarial? signed-exception?))
             :decision/digest (hash/text-digest (pr-str (dissoc a :decision/digest)))))))

(defn activation-qualified?
  [p decisions]
  (and (= :approved (:status (last decisions)))
       (true? (:activation-qualified? (last decisions)))
       (= :approved (:status p))))

(defn disable
  [proposal-id reviewer reason]
  (when-not (and (ref? proposal-id) (ref? reviewer) (not (str/blank? (str reason))))
    (fail! :disable-invalid "disable requires proposal, reviewer, and reason" {}))
  {:proposal/id proposal-id :reviewer reviewer :reason (str reason) :status :disabled})
