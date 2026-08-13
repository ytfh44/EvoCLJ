(ns evoclj.eval.profile
  "Evaluation profiles (Task 8.1).

  An evaluation profile is the NORMATIVE declaration of how one
  evaluation run is provisioned. It names the three physically
  separated datasets by :source (a dataset registry keyword, resolved
  to a filesystem root by evoclj.eval.dataset — never a loader
  handle), pins the selection set to kernel-only visibility and the
  audit set to operator-only visibility, declares how many times each
  selection case is repeated, and names the promotion strategy the
  profile is evaluated under:

      {:eval/profile-id :default-v1
       :evolution-set   {:source ...}                       ; evidence
       :selection-set   {:source ... :visibility :kernel-only}   ; evaluator
       :audit-set       {:source ... :visibility :operator-only} ; operator
       :repetitions     1
       :promotion       {:strategy ...}}

  THE ISOLATION CONTRACT (Global Constraints 11, 12, 23): the profile
  carries SOURCE KEYWORDS ONLY — it never carries a dataset loader, a
  case body, or any handle into the selection/audit datasets. Physical
  separation lives in evoclj.eval.dataset (three distinct roots), and
  this namespace only asserts the shape. Selection and audit
  visibility (:kernel-only / :operator-only) are part of the profile
  shape because they document which subsystem may ever consume those
  datasets; the enforcement boundary itself is architectural (see the
  dataset namespace and its tests).

  Error contract (Global Constraint 22 — plain serializable data):
  :eval/profile-invalid (closed-map contract violation, Malli
  explanations)."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.kernel.error :as err]))

;; --- the normative profile schema -------------------------------------------

(def DatasetSetSchema
  "A dataset set within a profile: a :source keyword (a dataset
  registry key resolved by evoclj.eval.dataset) plus, for the
  selection and audit sets, the visibility declaration that scopes
  who may consume it."
  [:map {:closed true}
   [:source keyword?]
   [:visibility {:optional true} keyword?]])

(def EvalProfileSchema
  "The normative Task 8.1 profile contract (closed). :source values
  are dataset registry keywords, not paths and never loader handles;
  :repetitions is the number of times each selection case runs in a
  paired comparison; :promotion names the promotion strategy this
  profile is evaluated under."
  [:map {:closed true}
   [:eval/profile-id keyword?]
   [:evolution-set DatasetSetSchema]
   [:selection-set [:map {:closed true}
                    [:source keyword?]
                    [:visibility [:= :kernel-only]]]]
   [:audit-set [:map {:closed true}
                [:source keyword?]
                [:visibility [:= :operator-only]]]]
   [:repetitions pos-int?]
   [:promotion [:map {:closed true}
                [:strategy keyword?]]]])

;; --- the default profile -----------------------------------------------------

(def default-v1
  "The normative :default-v1 profile. The three :source keywords
  resolve — via evoclj.eval.dataset/dataset-root — to the three
  physically separated dataset roots at the repository root:
  evals/evolution, evals/selection, evals/audit. Selection cases run
  once per paired comparison; promotion is a paired comparison of
  parent vs. candidate on the same case set and environment fixture
  (Global Constraint 13)."
  {:eval/profile-id :default-v1
   :evolution-set {:source :evals/evolution}
   :selection-set {:source :evals/selection :visibility :kernel-only}
   :audit-set {:source :evals/audit :visibility :operator-only}
   :repetitions 1
   :promotion {:strategy :paired-comparison}})

;; --- boundary validation -----------------------------------------------------

(defn profile?
  "True when `x` satisfies the normative EvalProfileSchema (closed map
  contract)."
  [x]
  (boolean (m/validate EvalProfileSchema x)))

(defn validate-profile!
  "Validate `x` against the normative profile contract. Returns `x`
  on success; throws :eval/profile-invalid with humanized Malli
  explanations otherwise."
  [x]
  (when-let [expl (m/explain EvalProfileSchema x)]
    (throw (err/error :eval/profile-invalid
                      "value does not satisfy the evaluation profile contract"
                      {:errors (me/humanize expl)})))
  x)

(defn dataset-sources
  "The three :source keywords a profile resolves to, keyed by dataset
  role: {:evolution <source> :selection <source> :audit <source>}.
  The three sources MUST be distinct — each dataset lives in its own
  physical root (Global Constraint 11)."
  [profile]
  (validate-profile! profile)
  (let [sources {:evolution (get-in profile [:evolution-set :source])
                 :selection (get-in profile [:selection-set :source])
                 :audit (get-in profile [:audit-set :source])}]
    (when-not (apply distinct? (vals sources))
      (throw (err/error :eval/profile-invalid
                        "the profile must name three physically distinct dataset sources"
                        {:sources sources})))
    sources))
