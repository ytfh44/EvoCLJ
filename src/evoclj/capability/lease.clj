(ns evoclj.capability.lease
  "Lease semantics for the v0 CapabilityLease (component).

  A CapabilityLease is a bounded HOST-OWNED grant: the kernel issues a
  plain immutable map binding ONE subject, ONE resource grant, an
  :actions set, and an instant window; the model never sees a lease as
  a name — only the kernel's broker (Milestone 4) reads it. The three
  pure decision functions are the whole semantics:

    (valid-at? lease instant)          ; window check
    (subject-matches? lease subject)   ; EXACT phenotype-id match
    (resource-covers? lease normalized-resource action) ; resource + action

  Every decision input is schema-checked before any judgment is made;
  a malformed lease, subject, resource, or action throws
  :capability/schema-invalid rather than silently granting or denying
  (a capability is a bounded host-owned grant, so garbage never
  authorizes and never hides a caller bug).

  Subject matching is EXACT on the phenotype id: a lease for P1 must
  never authorize P2, even when both phenotypes share the same Genome
  (Global Constraint 9 — a visible action never grants resource
  authority, and neither does a sibling phenotype).

  Resource coverage matches a CANONICAL resource plus an action:
  tool resources match by exact canonical id; filesystem resources
  match by containment of CANONICAL RESOLVED PATHS; memory resources
  match by exact canonical key id ({:kind :memory :id <key>}, feature
  R1 — the episodic-memory lease an :intent/memory-read/write carries
  is scoped to one exact key, mirroring :tool). Matching never happens
  on user-supplied strings: canonicalize-path resolves \".\"
  and \"..\" segments first, so a lease rooted at \"/work\" covers
  \"/work/a/../secret\" only because it resolves to \"/work/secret\"
  (inside the root), and a traversal escaping to \"/etc\" is never
  covered. The pure path canonicalization lives here so coverage is
  always decided on canonical forms; provider-side normalization of
  user-facing requests (kinds, ids, paths, Windows drive/backslash
  forms) is component (evoclj.provider). Unknown resource kinds fail
  closed: nothing is covered."
  (:require [clojure.string :as str]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]
            [malli.core :as m]))

;; --- pure path canonicalization --------------------------------------------

(defn canonicalize-path
  "Resolve a path string to its canonical form by dropping empty and
  \".\" segments and popping \"..\" segments. Operates on
  \"/\"-separated paths (the canonical form used by the v0 filesystem
  resource): \"/work/a/../secret\" -> \"/work/secret\". A \"..\" that
  would climb above an absolute root is clamped to the root, so
  \"/work/../../etc\" -> \"/etc\" and never escapes the filesystem
  root. Returns nil for non-string input, so matching fails closed.
  Windows drive/backslash canonicalization is provider-side
  normalization (component); this helper is the pure segment-level
  canonical form coverage is decided on."
  [s]
  (when (string? s)
    (let [absolute? (.startsWith s "/")
          segments (->> (str/split s #"/")
                        (remove #{"" "."})
                        (reduce (fn [acc seg]
                                  (if (= seg "..")
                                    (if (seq acc) (pop acc) acc)
                                    (conj acc seg)))
                                []))]
      (str (when absolute? "/") (str/join "/" segments)))))

(defn- path-inside?
  "True when the canonical path p lies inside the canonical root r:
  p equals r, or p is r plus one or more segments. The segment boundary
  matters — root \"/work\" covers \"/work/secret\" but never
  \"/workspace/x\". Root \"/\" covers every absolute path."
  [root path]
  (let [r (canonicalize-path root)
        p (canonicalize-path path)]
    (and r p
         (or (= r "/") (= r p)
             (.startsWith p (str r "/"))))))

(defn- canonicalize-mount-path
  "Mount-relative canonicalization for logical mount namespace (no leading /).
  Resolves \".\" and \"..\"; empty or \".\" -> \"\"."
  [s]
  (when (string? s)
    (let [clean (str/replace s "\\" "/")
          segments (->> (str/split clean #"/")
                        (remove #{ "" "."})
                        (reduce (fn [acc seg]
                                  (if (= seg "..")
                                    (if (seq acc) (pop acc) acc)
                                    (conj acc seg)))
                                []))]
      (str/join "/" segments))))

(defn- mount-path-inside?
  "True when mount-relative request path is inside grant path.
  Empty grant covers whole mount (segment-boundary aware)."
  [grant-path req-path]
  (let [g (canonicalize-mount-path (or grant-path ""))
        p (canonicalize-mount-path (or req-path ""))]
    (or (= g "") (= g p) (str/starts-with? p (str g "/")))))

;; --- shared input gate ------------------------------------------------------

(defn- validate-input!
  "Schema-check a lease and, when given, an additional decision input;
  throw :capability/schema-invalid on any failure. Every predicate
  gates its inputs here so no judgment is ever made on malformed data."
  [lease & [input-schema input]]
  (schema/validate-lease lease)
  (when (and input-schema (not (m/validate input-schema input)))
    (throw (err/error :capability/schema-invalid
                      "invalid capability lease decision input"
                      {:value (err/sanitize input)}))))

;; --- the three decision functions -------------------------------------------

(defn valid-at?
  "True when `instant` falls inside the lease's window:
  :issued-at INCLUSIVE, :expires-at EXCLUSIVE — a lease is valid AT
  :issued-at, dead AT :expires-at, and dead before :issued-at. The
  lease and the instant must be schema-valid or
  :capability/schema-invalid is thrown."
  [lease instant]
  (validate-input! lease)
  (when-not (inst? instant)
    (throw (err/error :capability/schema-invalid
                      "lease instant must be an #inst value"
                      {:value (err/sanitize instant)})))
  (and (not (.before ^java.util.Date instant ^java.util.Date (:issued-at lease)))
       (.before ^java.util.Date instant ^java.util.Date (:expires-at lease))))

(defn subject-matches?
  "True when the requesting `subject` matches the lease's subject.
  Dual-anchor [W-01] when both sides carry :session/id: BOTH session and
  phenotype must be equal. For backward compat with pre-P3 leases/tests
  that only carry :phenotype/id (no session), session is ignored when
  either side lacks it — only phenotype is compared."
  [lease subject]
  (validate-input! lease schema/SubjectSchema subject)
  (let [lease-session (get-in lease [:subject :session/id])
        subject-session (get-in subject [:session/id])
        lease-pheno (get-in lease [:subject :phenotype/id])
        subject-pheno (get-in subject [:phenotype/id])]
    (and (= lease-pheno subject-pheno)
         (or (nil? lease-session)
             (nil? subject-session)
             (= lease-session subject-session)))))

(defn resource-covers?
  "True when the lease's :resource grant covers the canonical
  `normalized-resource` for `action`: the action must be in the
  lease's :actions set AND the resource must match by kind. Tool
  resources match by exact canonical id ({:kind :tool :id ...});
  filesystem resources match by containment of canonical resolved
  paths ({:kind :filesystem :path ...}); memory resources (feature R1)
  match by exact key id ({:kind :memory :id <key>}, like :tool). Any
  other kind, a kind mismatch, a missing id/path, or a missing action
  fails closed. A malformed lease, resource, or action throws
  :capability/schema-invalid."
  [lease normalized-resource action]
  (validate-input! lease)
  (when-not (and (map? normalized-resource) (keyword? action))
    (throw (err/error :capability/schema-invalid
                      "resource must be a map and action a keyword"
                      {:value (err/sanitize normalized-resource)
                       :action (err/sanitize action)})))
  (let [granted (:resource lease)
        kind (:kind granted)]
    (and (contains? (:actions lease) action)
         (= kind (:kind normalized-resource))
         (case kind
           :tool (and (keyword? (:id granted))
                      (= (:id granted) (:id normalized-resource)))
           :memory (and (keyword? (:id granted))
                        (= (:id granted) (:id normalized-resource)))
           :model (and (:id granted)
                       (let [g (str (:id granted))
                             n (str (:id normalized-resource))]
                         (or (= g n)
                             (and (str/ends-with? g "/*")
                                  (str/starts-with? n (subs g 0 (dec (count g))))))))
           :filesystem (path-inside? (:path granted) (:path normalized-resource))
           :filesystem/path (if (contains? granted :mount/id)
                              (and (= (:mount/id granted) (:mount/id normalized-resource))
                                   (mount-path-inside? (:path granted) (:path normalized-resource)))
                              (path-inside? (:path granted) (:path normalized-resource)))
           false))))

;; ---------------------------------------------------------------------------
;; Generic LeaseRegistry helpers (P5) — unified for ANY kind
;; Delegates to capability/mint (single definition). Kept here as well so
;; callers can require either mint or lease. Idempotent revoke, fail-closed.
;; ---------------------------------------------------------------------------

(defn create-lease-registry
  "Create a fresh LeaseRegistry atom (delegates to capability/mint)."
  []
  (atom {}))

(defn get-lease
  "Look up a recorded lease by :cap/id, or nil (delegates to mint)."
  [registry cap-id]
  (get-in @registry [cap-id :lease]))

(defn lease-revoked?
  "True when the lease with :cap/id is recorded as revoked."
  [registry cap-id]
  (boolean (get-in @registry [cap-id :revoked?])))

(defn revoked?
  "Alias of lease-revoked? for ANY kind."
  [registry cap-id]
  (lease-revoked? registry cap-id))

(defn revoke-lease!
  "Revoke the recorded lease with :cap/id, idempotent."
  [registry cap-id]
  (swap! registry update cap-id (fn [rec]
                                  (cond-> (or rec {:lease nil :revoked? true})
                                    true (assoc :revoked? true))))
  nil)

(defn revoke-leases!
  "Revoke each lease in `leases` via `revoke-lease!`. Idempotent. S4 helper
  mirroring capability/mint. Delegates to revoke-lease! for each cap-id."
  [registry leases]
  (when (and registry (seq leases))
    (doseq [l leases]
      (when-let [cap-id (:cap/id l)]
        (revoke-lease! registry cap-id))))
  nil)

(defn leases-for-session
  "Return leases in `registry` for `session-id` (str-coerced compare)."
  [registry session-id]
  (let [sid (str session-id)]
    (->> @registry
         vals
         (keep :lease)
         (filterv (fn [l] (= sid (str (get-in l [:subject :session/id]))))))))
