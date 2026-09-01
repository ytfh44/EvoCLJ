(ns evoclj.capability.schema
  "Malli schemas for the v0 CapabilityLease (component).

  A CapabilityLease is a bounded, HOST-OWNED grant: a plain immutable
  map the kernel issues so a Principal may cross an effect — never a
  string name visible to the model (component acceptance). The contract
  is normative:

    {:cap/id #uuid \"...\"
     :principal {:principal/type :session :session/id #uuid \"...\"}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 10}
     :issued-at #inst \"...\"
     :expires-at #inst \"...\"}

  Principal is a tagged union (I2):
    SessionPrincipal  {:principal/type :session  :session/id <uuid>}
    JobPrincipal      {:principal/type :job      :job/id <uuid|string>}
    EvalPrincipal     {:principal/type :eval     :eval/id <uuid|string>}
    OperatorPrincipal {:principal/type :operator}

  Principal equality is identity — no wildcard, no dual-anchor, no
  placeholder. Session pin validation is separate (Generation pin, not
  lease subject).

  Global Constraint 8 makes every external effect cross the
  kernel-owned Intent/Capability Broker, so a lease exists only as a
  kernel-issued value; Global Constraint 9 says a visible action/tool
  never itself grants resource authority, so the lease binds a
  resource AND an action set AND a principal — the schema validates the
  shape, and evoclj.capability.lease decides coverage; Global
  Constraint 19 keeps the authority root agent-immutable, so leases
  are immutable host-owned values, never agent-writable; Global
  Constraint 22 keeps only validated, plain Clojure data on the
  boundary, so validate-lease gates on EDN-safety BEFORE schema
  checking (reusing evoclj.sci.boundary).

  The :resource value is an OPEN map — its shape is provider-defined
  (v0: {:kind :tool :id ...} or {:kind :filesystem :path ...}) and
  matched by dispatch in evoclj.capability.lease. Validation never
  coerces: a valid lease is returned unchanged; any failure throws
  :capability/not-edn-safe or :capability/schema-invalid carrying a
  fully serializable Malli explanation (safe for pr-str /
  clojure.edn read-string round-tripping)."
  (:require [evoclj.kernel.error :as err]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m]))

;; --- principal shape (I2) ---------------------------------------------------

(def SessionIdSchema
  "A session identifier — UUID or non-empty string form."
  [:or uuid? [:string {:min 1}]])

(def JobIdSchema
  "A job identifier — UUID or non-empty string form."
  [:or uuid? [:string {:min 1}]])

(def EvalIdSchema
  "An eval identifier — UUID or non-empty string form."
  [:or uuid? [:string {:min 1}]])

(def SessionPrincipalSchema
  "Session principal — one live session."
  [:map {:closed true}
   [:principal/type [:= :session]]
   [:session/id SessionIdSchema]])

(def JobPrincipalSchema
  "Job principal — one evolution/mutation job."
  [:map {:closed true}
   [:principal/type [:= :job]]
   [:job/id JobIdSchema]])

(def EvalPrincipalSchema
  "Eval principal — one evaluation side."
  [:map {:closed true}
   [:principal/type [:= :eval]]
   [:eval/id EvalIdSchema]])

(def OperatorPrincipalSchema
  "Operator principal — singleton, no id."
  [:map {:closed true}
   [:principal/type [:= :operator]]])

(def PrincipalSchema
  "Tagged union of all principals (I2)."
  [:multi {:dispatch :principal/type}
   [:session SessionPrincipalSchema]
   [:job JobPrincipalSchema]
   [:eval EvalPrincipalSchema]
   [:operator OperatorPrincipalSchema]])

;; Backcompat alias — deprecated, do not use in new code
(def SubjectSchema
  "Deprecated alias for PrincipalSchema. Use PrincipalSchema."
  PrincipalSchema)

(def PhenotypeIdSchema
  "Legacy phenotype id kept for migration compat (not part of principal)."
  [:string {:min 1}])

(defn session-principal
  "Construct a SessionPrincipal for `sid`."
  [sid]
  {:principal/type :session :session/id sid})

(defn job-principal
  "Construct a JobPrincipal for `jid`."
  [jid]
  {:principal/type :job :job/id jid})

(defn eval-principal
  "Construct an EvalPrincipal for `eid`."
  [eid]
  {:principal/type :eval :eval/id eid})

(def operator-principal
  "Singleton OperatorPrincipal."
  {:principal/type :operator})

(defn principal?
  "True when x is a valid Principal value."
  [x]
  (m/validate PrincipalSchema x))

(def ^:private allowed-actions
  "Closed allowlist for lease actions — [W-03] actions must be non-empty
  and subset of this set. Unknown actions are rejected at the schema
  boundary (fail-closed)."
  #{:invoke :read :list :stat :write :create :delete})

(def ^:private positive-window?
  "A grant must span a positive window: :expires-at strictly after
  :issued-at. A zero- or negative-window lease could never be valid and
  is a host-side bug, rejected at the trust boundary."
  (fn [m]
    (let [issued (:issued-at m)
          expires (:expires-at m)]
      (and (inst? issued) (inst? expires)
           (.before ^java.util.Date issued ^java.util.Date expires)))))

(def ^:private constraints-schema
  "Closed constraints map: only known quota keys plus audit chain keys.
  Quota keys are registered ConstraintDescriptors (C3). Audit keys
  :cap/attenuated-from and :attenuated-from are allowed for derivation chain.
  Unknown keys fail closed — widening via passthrough is removed."
  [:map {:closed true}
   [:max-calls {:optional true} [:and :int [:fn (fn [x] (>= x 0))]]]
   [:max-bytes {:optional true} [:and :int [:fn (fn [x] (>= x 0))]]]
   [:cap/attenuated-from {:optional true} uuid?]
   [:attenuated-from {:optional true} uuid?]])

(def CapabilityLeaseSchema
  "The v0 CapabilityLease contract: a closed map of the seven normative
  fields. The top level is closed — no field may be missing, renamed,
  or extended. :principal is the tagged union Principal (I2);
  :actions is a set of keywords constrained to the closed
  allowlist #{:invoke :read :list :stat :write :create :delete} and
  must be non-empty; :resource is an open map (provider-defined);
  :constraints is a CLOSED map of known quota dimensions (C3) —
  only :max-calls, :max-bytes and audit keys are allowed, unknown
  keys are rejected fail-closed; :resource and constraints together
  with principal and TimeWindow form the full Lease algebra
  Lease = Grant × Principal × TimeWindow × Quota. The grant must span
  a positive window (:expires-at after :issued-at)."
  [:and
   [:map {:closed true}
    [:cap/id uuid?]
    [:principal PrincipalSchema]
    [:resource [:map {:closed false}]]
    [:actions [:and
               [:set [:enum :invoke :read :list :stat :write :create :delete]]
               [:fn seq]]]
    [:constraints constraints-schema]
    [:issued-at inst?]
    [:expires-at inst?]]
   [:fn positive-window?]])

;; --- sealed CapabilityLease (P1) -------------------------------------------
;; Mirrors broker/registry S5/S6 sealing: file-private secret + deftype,
;; assoc/without sealed, predicate via identical? secret, projection to
;; EDN map for event log (GC-20).

(def ^:private lease-secret (Object.))

(deftype CapabilityLease [capId principal resource actions constraints issued expires ^:private secret]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k notFound]
    (case k
      :cap/id capId
      :principal principal
      :subject principal
      :resource resource
      :actions actions
      :constraints constraints
      :issued-at issued
      :expires-at expires
      notFound))
  clojure.lang.Counted
  (count [this] 7)
  clojure.lang.IPersistentMap
  (assoc [this k v] (throw (UnsupportedOperationException. "CapabilityLease is sealed; use make-lease")))
  (without [this k] (throw (UnsupportedOperationException. "CapabilityLease is sealed")))
  clojure.lang.Seqable
  (seq [this] (seq {:cap/id capId
                    :principal principal
                    :resource resource
                    :actions actions
                    :constraints constraints
                    :issued-at issued
                    :expires-at expires}))
  java.lang.Iterable
  (iterator [this] (.iterator ^java.lang.Iterable (seq {:cap/id capId
                                                        :principal principal
                                                        :resource resource
                                                        :actions actions
                                                        :constraints constraints
                                                        :issued-at issued
                                                        :expires-at expires})))
  Object
  (toString [this] (str "CapabilityLease[" capId "]")))
(alter-meta! #'->CapabilityLease assoc :private true)

(defn lease?
  "True when x is a sealed CapabilityLease produced via make-lease.
  Arbitrary maps or records are never leases — sealing is via file-private
  secret checked with identical?."
  [x]
  (and (instance? CapabilityLease x)
       (identical? (.-secret ^CapabilityLease x) lease-secret)))

(defn lease->map
  "Project a sealed CapabilityLease to its EDN map for the event log
  (GC-20). Returns nil when x is not a sealed lease. The map is plain
  EDN and round-trips through pr-str / edn/read-string."
  [lease]
  (when (lease? lease)
    {:cap/id (.-capId ^CapabilityLease lease)
     :principal (.-principal ^CapabilityLease lease)
     :resource (.-resource ^CapabilityLease lease)
     :actions (.-actions ^CapabilityLease lease)
     :constraints (.-constraints ^CapabilityLease lease)
     :issued-at (.-issued ^CapabilityLease lease)
     :expires-at (.-expires ^CapabilityLease lease)}))


;; --- validation entry point ------------------------------------------------

(defn- subject->principal
  "Convert legacy :subject map to Principal tagged union for compat.
  If subject has :session/id -> SessionPrincipal, else -> OperatorPrincipal."
  [s]
  (cond
    (and (map? s) (:session/id s)) {:principal/type :session :session/id (:session/id s)}
    (map? s) {:principal/type :operator}
    :else nil))

(defn- canonicalize-constraints
  "C3: canonicalize :maxBytes -> :max-bytes for backward compat.
  Both spellings are accepted on input; canonical form is :max-bytes."
  [m]
  (if (map? m)
    (let [has-alias (contains? m :maxBytes)
          has-canonical (contains? m :max-bytes)]
      (cond
        (and has-alias has-canonical) (dissoc m :maxBytes)
        has-alias (-> m (assoc :max-bytes (:maxBytes m)) (dissoc :maxBytes))
        :else m))
    m))

(defn- canonicalize-lease-map
  "If map has legacy :subject and no :principal, convert to :principal and dissoc :subject.
  Also canonicalizes constraint alias :maxBytes -> :max-bytes (C3)."
  [m]
  (cond-> m
    (and (map? m) (contains? m :subject) (not (contains? m :principal)))
    (-> (assoc :principal (subject->principal (:subject m))) (dissoc :subject))
    (and (map? m) (contains? m :constraints))
    (update :constraints canonicalize-constraints)))

(defn validate-lease
  "Validate x as a v0 CapabilityLease.

  Accepts both a plain EDN map and a sealed CapabilityLease instance
  (INV-05: single implementation). For a sealed instance, projects via
  lease->map then validates the map; for a map, validates directly.

  Legacy :subject maps are canonicalized to :principal for compat (I2
  migration): {:principal/type :session :session/id sid} -> SessionPrincipal(sid),
  bare phenotype -> OperatorPrincipal. Validation never coerces otherwise.

  First the EDN-safe boundary gate (Global Constraint 22): the map
  must be plain, fully realized EDN data — raw Java objects, lazy
  sequences, records, and functions are rejected with
  :capability/not-edn-safe before any schema checking. Then the map is
  validated against CapabilityLeaseSchema.

  Returns x unchanged when it is a structurally valid lease; validation
  never coerces or rewrites values. Otherwise throws an ExceptionInfo
  with :error/type :capability/schema-invalid whose ex-data carries the
  sanitized input under :value and a fully serializable Malli
  explanation under :explanation."
  [x]
  (let [raw (if (lease? x) (lease->map x) x)
        m (canonicalize-lease-map raw)]
    (when-not (boundary/edn-safe? m)
      (throw (err/error :capability/not-edn-safe
                        "capability lease must be plain EDN-safe data (Global Constraint 22)"
                        {:value (err/sanitize m)})))
    (if (m/validate CapabilityLeaseSchema m)
      x
      (throw (err/error :capability/schema-invalid
                        "capability lease failed schema validation"
                        {:value (err/sanitize m)
                         :explanation (err/sanitize (m/explain CapabilityLeaseSchema m))})))))

(defn make-lease
  "Sealed factory for CapabilityLease. Validates m via CapabilityLeaseSchema
  and asserts issued < expires (positive window); on failure throws
  :capability/schema-invalid (never :capability/not-edn-safe for window
  or allowlist violations). Returns a sealed CapabilityLease instance
  on success — construct-time validated.
  Legacy :subject maps are accepted and canonicalized to :principal."
  [m]
  (let [m (canonicalize-lease-map m)
        validated (validate-lease m)]
    ;; validate-lease already enforces positive-window? via schema and
    ;; rejects non-EDN; extra assert keeps window failure typed as
    ;; :capability/schema-invalid even if schema were relaxed.
    (when-not (.before ^java.util.Date (:issued-at validated)
                       ^java.util.Date (:expires-at validated))
      (throw (err/error :capability/schema-invalid
                        "capability lease must span positive window: :expires-at after :issued-at"
                        {:value (err/sanitize m)
                         :explanation (err/sanitize (m/explain CapabilityLeaseSchema m))})))
    (let [p (or (:principal validated) (:subject validated) (:principal m) (subject->principal (:subject m)))]
      (CapabilityLease. (:cap/id validated)
                        p
                        (:resource validated)
                        (:actions validated)
                        (:constraints validated)
                        (:issued-at validated)
                        (:expires-at validated)
                        lease-secret))))
