(ns evoclj.capability.schema
  "Malli schemas for the v0 CapabilityLease (Task 4.2).

  A CapabilityLease is a bounded, HOST-OWNED grant: a plain immutable
  map the kernel issues so a Phenotype may cross an effect — never a
  string name visible to the model (Task 4.2 acceptance). The contract
  is normative:

    {:cap/id #uuid \"...\"
     :subject {:phenotype/id \"sha256:...\"}
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

(def SubjectSchema
  "The lease subject: the SINGLE phenotype the grant belongs to. A
  lease for P1 must never authorize P2, even when both share the same
  Genome — the subject carries only the phenotype id, and matching is
  exact."
  [:map {:closed true}
   [:phenotype/id PhenotypeIdSchema]])

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
  or extended. :actions is a set of keywords; :resource and
  :constraints are open maps whose shapes are provider-defined. The
  grant must span a positive window (:expires-at after :issued-at)."
  [:and
   [:map {:closed true}
    [:cap/id uuid?]
    [:subject SubjectSchema]
    [:resource [:map {:closed false}]]
    [:actions [:set keyword?]]
    [:constraints [:map {:closed false}]]
    [:issued-at inst?]
    [:expires-at inst?]]
   [:fn positive-window?]])

;; --- validation entry point ------------------------------------------------

(defn validate-lease
  "Validate x as a v0 CapabilityLease.

  First the EDN-safe boundary gate (Global Constraint 22): x must be
  plain, fully realized EDN data — raw Java objects, lazy sequences,
  records, and functions are rejected with :capability/not-edn-safe
  before any schema checking. Then x is validated against
  CapabilityLeaseSchema.

  Returns x unchanged when it is a structurally valid lease; validation
  never coerces or rewrites values. Otherwise throws an ExceptionInfo
  with :error/type :capability/schema-invalid whose ex-data carries the
  sanitized input under :value and a fully serializable Malli
  explanation under :explanation."
  [x]
  (when-not (boundary/edn-safe? x)
    (throw (err/error :capability/not-edn-safe
                      "capability lease must be plain EDN-safe data (Global Constraint 22)"
                      {:value (err/sanitize x)})))
  (if (m/validate CapabilityLeaseSchema x)
    x
    (throw (err/error :capability/schema-invalid
                      "capability lease failed schema validation"
                      {:value (err/sanitize x)
                       :explanation (err/sanitize (m/explain CapabilityLeaseSchema x))}))))
