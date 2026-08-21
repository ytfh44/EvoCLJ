(ns evoclj.genome.schema
  "Malli schemas for the immutable v1 Genome manifest and module
  descriptors (component).

  The manifest is a pure EDN contract evaluated at the trust boundary:
  GenomeManifestSchema is a closed map, so unknown top-level keys are
  rejected unless they live inside :metadata; module paths must be
  canonical relative paths that cannot escape the Genome bundle; and
  validation never coerces values — a valid manifest is returned
  unchanged and anything else throws :genome/schema-invalid carrying a
  fully serializable Malli explanation (Global Constraint 22)."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [malli.core :as m]))

(defn- relative-path?
  "True when s is a canonical relative file path: non-empty, forward-slash
  separated, with no leading slash, drive letter, backslash, or
  `.`/`..`/empty path component. Module paths must stay inside the
  Genome bundle directory, so anything that could resolve outside it
  (absolute paths, Windows drives, backslash traversal) is rejected."
  [s]
  (and (string? s)
       (pos? (count s))
       (not (str/starts-with? s "/"))
       (not (re-find #"\\" s))
       (not (re-find #"^[A-Za-z]:" s))
       (every? (fn [component]
                 (not (contains? #{"." ".." ""} component)))
               (str/split s #"/"))))

(def RelativePathSchema
  "A module file reference: a canonical relative path string."
  [:and :string [:fn relative-path?]])

(def ModuleDescriptorSchema
  "Closed map of the four required v1 Genome modules to their canonical
  relative file paths. Unknown module slots are rejected; the four
  declared modules are the only files the loader may trust."
  [:map {:closed true}
   [:topology RelativePathSchema]
   [:models RelativePathSchema]
   [:memory RelativePathSchema]
   [:evolution RelativePathSchema]])

(def CapabilityRequestSchema
  "Set of requested capability keywords. The manifest only declares that
  a capability is requested; whether it is actually granted is decided
  later by the kernel-owned broker, never by the schema."
  [:set keyword?])

(def AbiVersionSchema
  "Closed map of ABI slot to positive integer version."
  [:map {:closed true}
   [:kernel pos-int?]
   [:genome pos-int?]
   [:intent pos-int?]
   [:tool pos-int?]])

(def RiskClassSchema
  "The five risk classes defined by the evolution milestone (R0 :parameter,
  R1 :behavioral, R2 :program, R3 :topology, R4 :meta)."
  [:enum :parameter :behavioral :program :topology :meta])

(def EvolutionPolicySchema
  "Closed evolution policy: maximum mutation risk class and the set of
  mutable asset classes."
  [:map {:closed true}
   [:max-risk RiskClassSchema]
   [:mutable [:set keyword?]]])

(def GenomeManifestSchema
  "v1 Genome manifest as a closed map. Unknown top-level keys are
  rejected at the trust boundary; only :metadata is open to arbitrary
  keys and values."
  [:map {:closed true}
   [:genome/format [:enum 1]]
   [:agent/id keyword?]
   [:agent/entry keyword?]
   [:abi AbiVersionSchema]
   [:modules ModuleDescriptorSchema]
   [:capabilities/requested CapabilityRequestSchema]
   [:evolution EvolutionPolicySchema]
   [:metadata [:map {:closed false}]]])

(defn validate-manifest
  "Validate x against GenomeManifestSchema.

  Returns x unchanged when it is a structurally valid v1 manifest;
  validation never coerces or rewrites values. Otherwise throws an
  ExceptionInfo with :error/type :genome/schema-invalid whose ex-data
  carries the sanitized input under :value and a fully serializable
  Malli explanation under :explanation (safe for pr-str / clojure.edn
  read-string round-tripping)."
  [x]
  (if (m/validate GenomeManifestSchema x)
    x
    (throw (err/error :genome/schema-invalid
                      "genome manifest failed schema validation"
                      {:value (err/sanitize x)
                       :explanation (err/sanitize (m/explain GenomeManifestSchema x))}))))
