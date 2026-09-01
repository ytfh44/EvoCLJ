(ns evoclj.capability.schema
  "Malli schemas for the v0 CapabilityLease (component).

  A CapabilityLease is a bounded, HOST-OWNED grant: a plain immutable
  map the kernel issues so a Phenotype may cross an effect — never a
  string name visible to the model (component acceptance). The contract
  is normative:

    {:cap/id #uuid \"...\"
     :subject {:session/id #uuid \"...\" :phenotype/id \"sha256:...\"}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 10}
     :issued-at #inst \"...\"
     :expires-at #inst \"...\"}
  Global Constraint 8 makes every external effect cross the
  kernel-owned Intent/Capability Broker, so a lease exists only as a
  kernel-issued value; Global Constraint 9 says a visible action/tool
  never itself grants resource authority, so the lease binds a
  resource AND an action set AND a subject — the schema validates the
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
  (:require [evoclj.intent.schema :as intent-schema]
            [evoclj.kernel.error :as err]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m]))

;; --- lease shape ------------------------------------------------------------

(def PhenotypeIdSchema
  "A canonical content-addressed PhenotypeId string
  (\"sha256:<64 hex>\"), the same v0 ABI form used by the Intent
  schemas (evoclj.intent.schema). One definition of the id, reused at
  every boundary."
  intent-schema/PhenotypeIdSchema)

(def SessionIdSchema
  "A session identifier — UUID or non-empty string form.
  P3 dual-anchor: every CapabilityLease subject MUST carry a session."
  [:or uuid? [:string {:min 1}]])

(def SubjectSchema
  "The lease subject: the SINGLE session+phenotype pair the grant belongs to.
  Dual-anchor [W-01] when both sides carry :session/id: BOTH session and
  phenotype must be equal; for backward compat with pre-P3 leases,
  :session/id is optional and ignored when missing."
  [:map {:closed true}
   [:phenotype/id PhenotypeIdSchema]
   [:session/id {:optional true} SessionIdSchema]])

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

(def CapabilityLeaseSchema
  "The v0 CapabilityLease contract: a closed map of the seven normative
  fields. The top level is closed — no field may be missing, renamed,
  or extended. :actions is a set of keywords constrained to the closed
  allowlist #{:invoke :read :list :stat :write :create :delete} and
  must be non-empty; :resource and :constraints are open maps whose
  shapes are provider-defined. The grant must span a positive window
  (:expires-at after :issued-at)."
  [:and
   [:map {:closed true}
    [:cap/id uuid?]
    [:subject SubjectSchema]
    [:resource [:map {:closed false}]]
    [:actions [:and
               [:set [:enum :invoke :read :list :stat :write :create :delete]]
               [:fn seq]]]
    [:constraints [:map {:closed false}]]
    [:issued-at inst?]
    [:expires-at inst?]]
   [:fn positive-window?]])

;; --- sealed CapabilityLease (P1) -------------------------------------------
;; Mirrors broker/registry S5/S6 sealing: file-private secret + deftype,
;; assoc/without sealed, predicate via identical? secret, projection to
;; EDN map for event log (GC-20).

(def ^:private lease-secret (Object.))

(deftype CapabilityLease [capId subject resource actions constraints issued expires ^:private secret]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k notFound]
    (case k
      :cap/id capId
      :subject subject
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
                    :subject subject
                    :resource resource
                    :actions actions
                    :constraints constraints
                    :issued-at issued
                    :expires-at expires}))
  java.lang.Iterable
  (iterator [this] (.iterator ^java.lang.Iterable (seq {:cap/id capId
                                                        :subject subject
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
     :subject (.-subject ^CapabilityLease lease)
     :resource (.-resource ^CapabilityLease lease)
     :actions (.-actions ^CapabilityLease lease)
     :constraints (.-constraints ^CapabilityLease lease)
     :issued-at (.-issued ^CapabilityLease lease)
     :expires-at (.-expires ^CapabilityLease lease)}))


;; --- validation entry point ------------------------------------------------

(defn validate-lease
  "Validate x as a v0 CapabilityLease.

  Accepts both a plain EDN map and a sealed CapabilityLease instance
  (INV-05: single implementation). For a sealed instance, projects via
  lease->map then validates the map; for a map, validates directly.

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
  (let [m (if (lease? x) (lease->map x) x)]
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
  on success — construct-time validated."
  [m]
  (let [validated (validate-lease m)]
    ;; validate-lease already enforces positive-window? via schema and
    ;; rejects non-EDN; extra assert keeps window failure typed as
    ;; :capability/schema-invalid even if schema were relaxed.
    (when-not (.before ^java.util.Date (:issued-at validated)
                       ^java.util.Date (:expires-at validated))
      (throw (err/error :capability/schema-invalid
                        "capability lease must span positive window: :expires-at after :issued-at"
                        {:value (err/sanitize m)
                         :explanation (err/sanitize (m/explain CapabilityLeaseSchema m))})))
    (CapabilityLease. (:cap/id validated)
                      (:subject validated)
                      (:resource validated)
                      (:actions validated)
                      (:constraints validated)
                      (:issued-at validated)
                      (:expires-at validated)
                      lease-secret)))
