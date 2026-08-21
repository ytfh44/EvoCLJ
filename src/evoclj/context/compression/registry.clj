(ns evoclj.context.compression.registry
  "Archiver registry for the context-compression subsystem.

   Tools and plugins that maintain their own structured state (todo
   trackers, goal registries, capability brokers, etc.) can declare
   what they archived by implementing `CompacterArchive`. This keeps
   the core context modules decoupled from any specific tool: the
   registry is the only coupling point, and it is deliberately
   trivial.

   The archiver reports are consumed by `footer` to produce the
   compact footer text. They are NOT stored in the envelope itself,
   so they do not waste envelope tokens.")

;; ---------------------------------------------------------------------------
;; Protocol
;; ---------------------------------------------------------------------------

(defprotocol CompacterArchive
  "Implemented by any tool that can report what it archived during a
  compaction cycle.

   The returned map must be EDN-safe and contain:

     {:archiver/id          <keyword, unique>
      :archiver/description  <string, human-readable>
      :archiver/serialized   <map, EDN-safe summary of archived state>}

   The `serialized` map is what gets folded into the footer text. It
   should be small — the footer is already a token-saving device."
  (archive-manifest [this]
    "Return the archiver's manifest map."))

;; ---------------------------------------------------------------------------
;; Registry atom
;; ---------------------------------------------------------------------------

(def ^:private archivers
  "Atom holding the registered archivers. Each entry must satisfy
  `CompacterArchive`."
  (atom []))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn register!
  "Register an archiver instance. Returns the instance.

   `archiver` must satisfy `CompacterArchive`. Duplicate registrations
   are allowed (the footer will list each one)."
  [archiver]
  {:pre [(satisfies? CompacterArchive archiver)]}
  (swap! archivers conj archiver)
  archiver)

(defn unregister!
  "Remove all occurrences of `archiver` from the registry. Returns the
  atom's new value."
  [archiver]
  (reset! archivers (remove #(identical? % archiver) @archivers)))

(defn registered?
  "True when `archiver` is present in the registry."
  [archiver]
  (some #(identical? % archiver) @archivers))

(defn archiver-reports
  "Return a vector of manifest maps from all registered archivers.

   Each element is the result of `(archive-manifest archiver)`. The
   vector is empty when no archivers are registered."
  []
  (vec (map archive-manifest @archivers)))

(defn clear-registry!
  "Remove all archivers. Intended for test teardown."
  []
  (reset! archivers []))
