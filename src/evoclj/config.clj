(ns evoclj.config
  "Single validated configuration contract for EvoCLJ (foundation F5).

  Historically configuration was scattered across CLI flags and ad-hoc
  evaluator maps. This namespace establishes ONE envelope — a fixed set
  of top-level sections (`:config/version`, `:config/profiles`,
  `:config/model-routing`, `:config/budget`, `:config/judge`,
  `:config/retention`, `:config/evolution-loop`,
  `:config/canary`) — with defaults and a single, consistent merge
  + validation path. The routing/budget/retention sections are
  intentionally OPEN (:map): their concrete keys arrive with their
  features. The :config/judge section (roadmap V5) is VALIDATED — its
  three model-call keys (:temperature, :system-prompt, :max-tokens)
  are checked when present — yet stays OPEN so host-wiring keys
  (e.g. :model/:type/:model/id) still pass. The CONTRACT here is the
  envelope, the defaults, and the merge semantics. `ConfigSchema` is a
  closed map, so any unknown top-level key is rejected at trust
  boundaries (Global Constraint 22: only validated, EDN-safe data
  crosses module boundaries).

  The namespace also carries the \"gated policy\" essence (Global
  Constraints 19/24 — the dual-control substrate): policy changes are
  *proposable records* that cannot approve themselves. `propose-policy`
  creates a `:policy/status :proposed` record; `transition-policy`
  advances it ONLY on the edges `:proposed -> :approved` and
  `:proposed -> :rejected`. `:approved` and `:rejected` are terminal:
  no record already sitting in either state may transition again, and
  a record may not re-propose itself. Transitions are PURE — no
  persistence, no store writes. Recording a proposal is separate from
  applying it; persistence and the human/machine approval seam arrive
  with the features that consume this substrate.

  Error contract (typed via evoclj.kernel.error/error):
    :config/invalid            — violation of ConfigSchema or unparseable
                                 input (non-map, bad EDN, unknown top-level
                                 key, non-map section).
    :config/profile-not-found  — resolve-profile on an absent profile key.
    :config/invalid-transition — transition-policy on a disallowed edge.
  All :config/invalid errors carry `:errors`, a humanized Malli
  explanation, under :errors, so callers can surface a readable reason."
  (:require [clojure.edn :as edn]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.kernel.error :as err]))

;; ============================================================================
;; the config schema
;; ============================================================================

(def JudgeSectionSchema
  "The :config/judge section contract (roadmap V5): the judge's
  model-call settings are exposed as :temperature (number),
  :system-prompt (string), and :max-tokens (pos-int). All three keys
  are OPTIONAL — the concrete defaults live in the judge itself
  (temperature 0.0, max-tokens 1024, the built-in system prompt) and
  apply when a key is absent. The section stays OPEN (unknown keys
  still pass, e.g. host-wiring keys like :model/:type/:model/id), so
  the three keys are validated WHEN PRESENT without closing the map:
  an invalid value (non-number :temperature, non-string
  :system-prompt, non-pos-int :max-tokens) is rejected by
  validate-config! with :config/invalid."
  [:map {:closed false}
   [:temperature {:optional true} number?]
   [:system-prompt {:optional true} string?]
   [:max-tokens {:optional true} pos-int?]])

(def ConfigSchema
  "The closed envelope for a validated EvoCLJ configuration. Sections
  are open maps — concrete routing/budget/retention keys arrive with
  their features; the :config/judge section is additionally validated
  via JudgeSectionSchema (V5 keys checked when present); the contract
  is the envelope + defaults + merge semantics."
  [:map {:closed true}
   [:config/version int?]
   [:config/profiles [:map-of keyword? :map]]
   [:config/model-routing :map]
   [:config/budget :map]
   [:config/judge JudgeSectionSchema]
   [:config/retention :map]
   [:config/evolution-loop :map]
   [:config/canary :map]])

(defn- default-config*
  "The base configuration: version 1 and every section empty. Returns a
  fresh value each call so callers may safely build on it."
  []
  {:config/version 1
   :config/profiles {}
   :config/model-routing {}
   :config/budget {:max-cost 0.0
                  :max-tokens 0}
   :config/judge {}
   :config/retention {}
   :config/evolution-loop {:max-generations 20
                           :plateau-window 5
                           :min-improvement 0.01
                           :stop-on-regression? true}
   :config/canary {:healthy-window 50}})

(defn default-config
  "The root configuration with every section at its default value:
  `{:config/version 1 :config/profiles {} :config/model-routing {}
   :config/budget {:max-cost 0.0 :max-tokens 0} :config/judge {}
   :config/retention {} :config/evolution-loop {:max-generations 20
   :plateau-window 5 :min-improvement 0.01 :stop-on-regression? true}
   :config/canary {:healthy-window 50}}`. Always validates against
  ConfigSchema."
  []
  (default-config*))

;; -- merge semantics ----------------------------------------------------------

(def ^:private section-keys
  "The envelope sections over which a profile or input may override."
  [:config/model-routing :config/budget :config/judge :config/retention
   :config/evolution-loop :config/canary])

(defn- config-invalid!
  "Throw a :config/invalid typed error carrying `message` and optional
  `errors` (a humanized explanation) and `data` in ex-data."
  ([message errors data]
   (throw (err/error :config/invalid message (assoc data :errors errors))))
  ([message errors]
   (config-invalid! message errors {})))

(defn- parse-input
  "Coerce `x` (an EDN string or a map) into a plain map, throwing
  :config/invalid on anything else. Strings are parsed with
  clojure.edn/read-string; unparseable EDN and non-map results are
  rejected."
  [x]
  (cond
    (map? x) x

    (string? x)
    (let [v (try
              (edn/read-string x)
              (catch Throwable t
                (config-invalid! "unable to parse config EDN"
                                 [{:value (str x)
                                   :message (.getMessage t)}]
                                 {:input (err/sanitize x)})))]
      (if (map? v)
        v
        (config-invalid! "config EDN must parse to a map"
                         [{:value (err/sanitize v)
                           :detail "expected a map, got something else"}]
                         {:input (err/sanitize x)})))

    :else
    (config-invalid! "config input must be an EDN string or a map"
                     [{:input (err/sanitize x)
                       :detail (str "got " (some-> (class x) .getName))}]
                     {:input (err/sanitize x)})))

(defn- merge-section
  "Merge an incoming section `value` over `default-sec`. A nil value (key
  absent) leaves the default untouched; a map is merged (shallow) so the
  incoming overrides same-name keys; anything else throws :config/invalid
  (non-map section)."
  [default-sec section-key value]
  (cond
    (nil? value)                default-sec
    (map? value)                (merge default-sec value)
    :else
    (config-invalid! "config section must be a map"
                     [{:path section-key
                       :value (err/sanitize value)}]
                     {:section section-key
                      :value (err/sanitize value)})))

(defn- merge-into-default
  "Deep-merge the validated input map `m` over `(default-config)` per
  section. Section values are maps merged with merge (shallow); the
  :config/profiles map is merged key-wise so incoming profiles overlay
  defaults at the same key. Non-map section values throw :config/invalid
  before any structural validation runs."
  [m]
  (let [base (default-config)]
    (-> base
        (assoc :config/version (or (:config/version m) (:config/version base)))
        (assoc :config/profiles (merge (:config/profiles base) (:config/profiles m)))
        (assoc :config/model-routing
               (merge-section (:config/model-routing base)
                              :config/model-routing (:config/model-routing m)))
        (assoc :config/budget
               (merge-section (:config/budget base)
                              :config/budget (:config/budget m)))
        (assoc :config/judge
               (merge-section (:config/judge base)
                              :config/judge (:config/judge m)))
        (assoc :config/retention
               (merge-section (:config/retention base)
                              :config/retention (:config/retention m)))
        (assoc :config/evolution-loop
               (merge-section (:config/evolution-loop base)
                              :config/evolution-loop (:config/evolution-loop m)))
        (assoc :config/canary
               (merge-section (:config/canary base)
                              :config/canary (:config/canary m))))))

(defn validate-config!
  "Validate `config` against ConfigSchema. Returns `config` unchanged on
  success, or throws :config/invalid with a humanized Malli explanation
  under :errors. Use as the public re-validator when a config crosses a
  trust boundary."
  [config]
  (if-let [expl (m/explain ConfigSchema config)]
    (throw (err/error :config/invalid
                      "config does not satisfy the EvoCLJ configuration contract"
                      {:errors (me/humanize expl)}))
    config))

(def ^:private top-level-keys
  "The complete set of keys the closed config envelope admits. Unknown
  top-level keys are rejected before merging so they cannot be silently
  discarded by the per-section merge."
  #{:config/version :config/profiles
    :config/model-routing :config/budget :config/judge :config/retention
    :config/evolution-loop :config/canary})

(defn- reject-unknown-keys!
  "Throw :config/invalid when `m` carries a top-level key outside the
  closed config envelope."
  [m]
  (when-let [unknown (seq (remove top-level-keys (keys m)))]
    (config-invalid! "unknown top-level config keys"
                     (mapv (fn [k] {:path k :detail "not a declared config section"})
                           unknown)
                     {:unknown-keys (vec unknown)})))

(defn load-config
  "Parse and validate a configuration from `x`, which is an EDN string or
  a map. Strings are read with clojure.edn/read-string. The parsed map is
  deep-merged over `(default-config)` per section — section values are
  maps merged with merge, and :config/profiles is merged key-wise — then
  validated against ConfigSchema. Returns the validated config.

  Throws :config/invalid on any violation: non-map input, unparseable
  EDN, unknown top-level keys, or non-map sections."
  [x]
  (let [m (parse-input x)]
    (reject-unknown-keys! m)
    (validate-config! (merge-into-default m))))

;; ============================================================================
;; profiles
;; ============================================================================

(defn resolve-profile
  "Resolve `profile-key` to the effective configuration for that profile:
  the default sections with that profile's section overrides applied on top
  (per-section `merge`, same semantics as `load-config`). Returns a map of
  the same shape as `(default-config)` (including :config/profiles) with
  the profile's overrides reflected in the affected sections.

  Throws :config/profile-not-found when `profile-key` is absent from
  :config/profiles, and :config/invalid when a profile section override is
  not a map."
  [config profile-key]
  (let [profiles (or (:config/profiles config) {})]
    (if-not (contains? profiles profile-key)
      (throw (err/error :config/profile-not-found
                        (str "no configuration profile named " profile-key)
                        {:policy/profile profile-key}))
      (let [profile (get profiles profile-key)]
        (merge-with (fn [a b]
                      (cond
                        (and (map? a) (map? b)) (merge a b)
                        (map? b) b
                        :else b))
                    config
                    profile)))))

(defn config-value
  "Resolve a single configuration value by `path`, a vector of keywords,
  from `config`. Equivalent to `(get-in config path)`; returns nil when any
  element of `path` is absent. Examples: `[:config/judge :enabled]`,
  `[:config/model-routing :default]`."
  [config path]
  (get-in config path))

;; ============================================================================
;; the built-in :demo profile (component)
;; ============================================================================

(defn demo-profile
  "The built-in :demo profile (component): a load-config input map whose
  :config/profiles :demo entry carries the demo's declared config
  section overrides (the v0 budget cap). Merge it into a config source
  and load with `load-config` to make the :demo profile resolvable —
  the CLI host does this automatically whenever the :demo profile is
  selected (component), so a fresh state dir needs no config file.

  The profile's NON-config surface — the built-in heuristic Mutator
  (evoclj.evolution.demo-mutator) plus the demo's hidden selection
  cases and fixture providers — is injected by the host through the
  SAME :overrides seam any host uses, never smuggled into this
  validated envelope (ConfigSchema is closed). Returns a fresh value
  each call so callers may build on it."
  []
  {:config/profiles
   {:demo
    {:config/budget {:max-candidates 3}}}})

;; ============================================================================
;; gated policy proposals (dual-control substrate)
;; ============================================================================

(def PolicyProposalSchema
  "The policy-proposal record contract. A proposal is immutable except for
  its :policy/status, which advances monotonically under the gated
  transition rules in `transition-policy`. The schema is closed: no
  generator may add its own keys to a proposal."
  [:map {:closed true}
   [:policy/proposal-id uuid?]
   [:policy/target keyword?]
   [:policy/proposed :map]
   [:policy/status [:enum :proposed :approved :rejected]]
   [:policy/created-at [:fn inst?]]])

(defn propose-policy
  "Create a new GATED policy proposal for `target` with the proposed change
  `proposed`, in status `:proposed`. A fresh #uuid and the current time
  (`now`) are stamped on the record. Pure with respect to persistence: the
  record is returned in-memory only — recording the proposal is distinct
  from applying or persisting it, which arrive with the consuming features.

  A proposal CANNOT approve itself: only the transition rules in
  `transition-policy` advance status, and they forbid a proposal from
  moving itself to :approved without going through the gated path."
  [target proposed]
  {:policy/proposal-id (random-uuid)
   :policy/target target
   :policy/proposed proposed
   :policy/status :proposed
   :policy/created-at (java.util.Date.)})

(def ^:private transition-edges
  "The only allowed status edges. :approved and :rejected are terminal;
  nothing may transition away from them, and a proposal may not oscillate
  back to :proposed."
  {:proposed #{:approved :rejected}})

(defn transition-policy
  "Transition `proposal` (a policy proposal record) to `new-status`,
  returning the updated record (pure — no persistence).

  Allowed edges are ONLY `:proposed -> :approved` and
  `:proposed -> :rejected`; :approved and :rejected are terminal, and a
  non-proposed record may not be re-proposed. Any other transition throws
  :config/invalid-transition carrying {:policy/status <current>
  :new-status <requested>} in ex-data. The returned record keeps every
  other field (id, target, proposed, created-at) identical to the input."
  [proposal new-status]
  (let [current (:policy/status proposal)]
    (if (contains? (get transition-edges current #{}) new-status)
      (assoc proposal :policy/status new-status)
      (throw (err/error :config/invalid-transition
                        (str "invalid policy transition from " current " to " new-status)
                        {:policy/status current
                         :new-status new-status})))))
