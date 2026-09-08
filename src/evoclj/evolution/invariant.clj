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

(def ^:private result-envelope-keys
  #{:proposal/id :predicate/digest :registry/revision :run/kind
    :target/digest :evaluation/id :candidate/id
    :gate/id :status :details-ref :details/ref :model/policy
    :deterministic? :fresh-model? :passed?})

(def ^:private result-required-keys
  #{:proposal/id :predicate/digest :registry/revision :run/kind
    :gate/id :status :details-ref :model/policy
    :deterministic? :fresh-model? :passed?})

(defn- result-envelope
  "Validate the closed, persisted gate-result envelope. Qualification is
  derived only from this envelope, never from sibling run keys."
  [result]
  (when-not (map? result) (fail! :result-invalid "run result must be a map" {:value (err/sanitize result)}))
  (when-not (= result-envelope-keys (set (keys result)))
    (fail! :result-invalid "run result contains an unknown key" {:value (err/sanitize result)}))
  (when-not (every? #(contains? result %) result-required-keys)
    (fail! :result-invalid "run result is missing a required envelope field" {:value (err/sanitize result)}))
  (when-not (or (ref? (:evaluation/id result)) (ref? (:candidate/id result)) (digest? (:target/digest result)))
    (fail! :identity-invalid "run result requires an evaluation, candidate, or stable target identity" {:value (err/sanitize result)}))
  (let [gate (:gate/id result)
        details (or (:details-ref result) (:details/ref result))]
    (when-not (= :G3-deterministic-suites gate)
      (fail! :gate-invalid "qualification requires the G3 deterministic-suites gate" {:gate gate}))
    (when-not (contains? run-kinds (:run/kind result))
      (fail! :run-kind-invalid "result envelope run kind is unsupported" {:run/kind (:run/kind result)}))
    (when-not (and (keyword? (:status result))
                   (contains? #{:pass :fail :error :not-run} (:status result)))
      (fail! :status-invalid "result envelope status is unsupported" {:status (:status result)}))
    (when-not (every? boolean? (map #(get result %) [:deterministic? :fresh-model? :passed?]))
      (fail! :claim-invalid "result envelope qualification fields must be booleans" {}))
    (when-not (keyword? (:model/policy result))
      (fail! :claim-invalid "result envelope model policy must be a keyword" {}))
    (when (and (= :pass (:status result)) (not (:passed? result)))
      (fail! :status-inconsistent "a passing result must have passed? true" {}))
    (when (and (contains? #{:fail :error :not-run} (:status result)) (:passed? result))
      (fail! :status-inconsistent "a failed, errored, or not-run result cannot have passed? true" {}))
    (when-not (ref? details)
      (fail! :details-ref-invalid "gate result requires a details artifact ref" {:value (err/sanitize details)}))
    (assoc result :details-ref details)))

(defn run
  "Normalize immutable evaluation evidence. Security claims are read only
  from the closed result envelope."
  [r]
  (when-not (map? r) (fail! :run-invalid "run must be a map" {:value (err/sanitize r)}))
  (doseq [k [:run/id :proposal/id :kind :result-ref :result]]
    (when-not (contains? r k) (fail! :missing-key "run is missing a required key" {:key k})))
  (when-not (ref? (:run/id r)) (fail! :ref-invalid "run id is malformed" {:key :run/id}))
  (when-not (ref? (:proposal/id r)) (fail! :ref-invalid "proposal id is malformed" {:key :proposal/id}))
  (when-not (contains? run-kinds (:kind r)) (fail! :run-kind-invalid "unknown run kind" {:kind (:kind r)}))
  (when-not (ref? (:result-ref r)) (fail! :malformed-ref "run result ref is malformed" {:value (err/sanitize (:result-ref r))}))
  (let [result (result-envelope (:result r))]
    (when-not (= (:proposal/id r) (:proposal/id result))
      (fail! :proposal-mismatch "result envelope is bound to another proposal" {}))
    (when-not (= (:kind r) (:run/kind result))
      (fail! :run-kind-mismatch "result envelope is bound to another run kind" {}))
    (when-not (safe-value? r 0) (fail! :run-opaque "run contains non-EDN data" {:value (err/sanitize r)}))
    (assoc r :result result :gate (:gate/id result)
           :model/policy (:model/policy result)
           :deterministic? (:deterministic? result)
           :fresh-model? (:fresh-model? result)
           :passed? (:passed? result)
           :activation-qualified? (and (= :recorded-only (:model/policy result))
                                      (= :pass (:status result))
                                      (true? (:deterministic? result))
                                      (not (true? (:fresh-model? result)))
                                      (true? (:passed? result))))))

(defn- selected-run [runs kind ref]
  (some #(when (and (= kind (:kind %))
                    (= ref (:result-ref %))) %) runs))

(defn approval
  "Create an immutable approval decision. Selected refs must be exact proposal
  refs and exact stored run/result digests."
  [p a runs]
  (let [p (proposal p)]
    (when-not (map? a) (fail! :approval-invalid "approval must be a map" {:value (err/sanitize a)}))
    (doseq [k [:reviewer :replay/ref :adversarial/ref]]
      (when-not (contains? a k) (fail! :missing-key "approval is missing a required key" {:key k})))
    (when-not (ref? (:reviewer a)) (fail! :reviewer-invalid "reviewer must be a stable identity" {}))
    (when (= (str (:reviewer a)) (str (:proposer p))) (fail! :reviewer-conflict "proposer cannot approve its own invariant" {}))
    (let [replay-ref (:replay/ref a)
          adversarial-ref (:adversarial/ref a)
          replay (selected-run runs :replay replay-ref)
          adversarial (selected-run runs :adversarial adversarial-ref)
          counterexample? (some #(= :counterexample (:kind %)) runs)]
      (when (= replay-ref adversarial-ref)
        (fail! :evidence-ref-reused "replay and adversarial evidence refs must be distinct" {}))
      (when-not (some #{replay-ref} (:replay/refs p))
        (fail! :replay-ref-mismatch "approval replay ref is not one of the proposal replay refs" {}))
      (when-not (some #{adversarial-ref} (:adversarial/refs p))
        (fail! :adversarial-ref-mismatch "approval adversarial ref is not one of the proposal adversarial refs" {}))
      (doseq [[label run] [[:replay replay] [:adversarial adversarial]]]
        (when run
          (when-not (= (:proposal/id p) (get-in run [:result :proposal/id]))
            (fail! :proposal-mismatch "selected evidence belongs to another proposal" {:evidence label}))
          (when-not (= (get-in p [:predicate :predicate/digest]) (get-in run [:result :predicate/digest]))
            (fail! :predicate-mismatch "selected evidence belongs to another predicate" {:evidence label}))
          (when-not (= (:registry/revision p) (get-in run [:result :registry/revision]))
            (fail! :registry-revision-mismatch "selected evidence belongs to another registry revision" {:evidence label}))))
      (when-not (and replay adversarial (:activation-qualified? replay) (:activation-qualified? adversarial)
                     (= :G3-deterministic-suites (get-in replay [:result :gate/id]))
                     (= :G3-deterministic-suites (get-in adversarial [:result :gate/id]))
                     (not (:fresh-model? replay)) (not (:fresh-model? adversarial))
                     (not counterexample?))
        (fail! :evidence-insufficient "approval requires the selected passing deterministic replay and adversarial G3 evidence results" {}))
      (assoc a :proposal/id (or (:proposal/id p) (:proposal/id a))
             :status :approved :activation-qualified? true
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
