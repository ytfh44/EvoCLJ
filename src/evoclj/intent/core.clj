(ns evoclj.intent.core
  "Canonical Intent normalization and the pure v0 Intent constructors
  (Task 4.1).

  An Intent is the only way evolvable code requests an effect, and a
  runtime action is always represented by a validated immutable value
  before any provider code runs (Task 4.1 acceptance).

  (normalize-intent x) validates x against the Intent ABI and returns a
  canonical form. Canonicalization is order-insensitive: two
  semantically equal maps that differ only in key ordering normalize to
  equal values with identical serialization, so
  `(= (normalize-intent a) (normalize-intent b))` holds whenever a and
  b carry the same intent data. normalize-intent NEVER invents
  authorization: it adds no capability, lease, grant, or decision keys —
  the intent remains a request, and only the kernel-owned capability
  broker (Milestone 4) can authorize it.

  The constructors (model-call, tool-call, memory-read, memory-write,
  finish, fail) build full, validated intents from kernel-provided
  attribution (session, phenotype, node, cause event) plus payload and
  budget. They are pure data constructors: no IO, no host state, no
  ambient authority. Attribution is a parameter, never guessed — a
  constructor cannot invent a session, phenotype, or node (Global
  Constraint 20). Each constructor assigns a fresh :intent/id and
  validates the assembled map before returning it, so an invalid intent
  fails at construction, not at the provider boundary."
  (:require [evoclj.intent.schema :as schema]
            [evoclj.kernel.error :as err]))

;; --- canonicalization -------------------------------------------------------

(declare canonicalize)

(defn- canonicalize-map
  "Rebuild a map with its entries in a deterministic total order (by
  pr-str of the key), canonicalizing each value recursively. Sorting by
  pr-str avoids mixed-type comparison failures (metadata may hold keys
  of different types) while remaining deterministic for EDN-safe keys."
  [m]
  (into {}
        (map (fn [[k v]] [k (canonicalize v)]))
        (sort-by (fn [[k _]] (pr-str k)) m)))

(defn- canonicalize
  "Recursively rebuild x so equal data always has equal serialization:
  every map is re-materialized with its entries in a deterministic
  total order; sets, vectors, and lists are canonicalized element-wise;
  primitive EDN leaves pass through unchanged. Only EDN-safe values
  reach this function (validate-intent gates first), so no lazy
  sequence or Java object can appear here."
  [x]
  (cond
    (map? x) (canonicalize-map x)
    (vector? x) (mapv canonicalize x)
    (set? x) (into #{} (map canonicalize) x)
    (list? x) (apply list (map canonicalize x))
    :else x))

(defn normalize-intent
  "Validate x as a v0 Intent and return its canonical form.

  Order-insensitive: two semantically equal intents whose maps differ
  only in key ordering normalize to EQUAL values (and equal
  serializations).

  Adds nothing: the normalized value has exactly the keys of the base
  Intent shape and contains no capability, lease, grant, or decision
  keys — normalization never invents authorization. A value that is not
  EDN-safe (Java objects, lazy sequences) throws :intent/not-edn-safe;
  a value that fails the ABI schema (missing attribution, unknown
  intent type, negative budget, wrong payload) throws
  :intent/schema-invalid."
  [x]
  (canonicalize (schema/validate-intent x)))

;; --- pure constructors ------------------------------------------------------

(defn- assemble!
  "Assemble the full intent map for `type` from kernel-provided
  attribution (session, phenotype, node, cause event), payload, and
  budget; assign a fresh :intent/id; validate the assembled map against
  the Intent ABI; and return the canonical normalized value. Attribution
  is required and never defaulted: the caller (the runtime node
  executor) must supply the real session, phenotype, node, and cause
  event (Global Constraint 20)."
  [type session-id phenotype-id node-id cause-event-id payload budget]
  (normalize-intent
   {:intent/id (random-uuid)
    :intent/type type
    :session/id session-id
    :phenotype/id phenotype-id
    :node/id node-id
    :cause/event-id cause-event-id
    :payload payload
    :budget budget
    :metadata {}}))

(defn model-call
  "Build a validated :intent/model-call intent requesting a model call
  for the given session/phenotype/node attribution, cause event, payload
  ({:model/id ... :messages [...]}), and budget."
  [session-id phenotype-id node-id cause-event-id payload budget]
  (assemble! :intent/model-call session-id phenotype-id node-id
             cause-event-id payload budget))

(defn tool-call
  "Build a validated :intent/tool-call intent requesting a tool
  invocation for the given session/phenotype/node attribution, cause
  event, payload ({:tool/id ... :args {...}}), and budget."
  [session-id phenotype-id node-id cause-event-id payload budget]
  (assemble! :intent/tool-call session-id phenotype-id node-id
             cause-event-id payload budget))

(defn memory-read
  "Build a validated :intent/memory-read intent requesting an episodic
  memory read for the given session/phenotype/node attribution, cause
  event, payload ({:memory/key ...}), and budget."
  [session-id phenotype-id node-id cause-event-id payload budget]
  (assemble! :intent/memory-read session-id phenotype-id node-id
             cause-event-id payload budget))

(defn memory-write
  "Build a validated :intent/memory-write intent requesting an episodic
  memory write for the given session/phenotype/node attribution, cause
  event, payload ({:memory/key ... :memory/content ...}), and budget.
  Episodic memory writes stay distinct from procedural Genome changes
  (Global Constraint 10)."
  [session-id phenotype-id node-id cause-event-id payload budget]
  (assemble! :intent/memory-write session-id phenotype-id node-id
             cause-event-id payload budget))

(defn finish
  "Build a validated :intent/finish intent carrying the task result for
  the given session/phenotype/node attribution, cause event, payload
  ({:value ...}), and budget."
  [session-id phenotype-id node-id cause-event-id payload budget]
  (assemble! :intent/finish session-id phenotype-id node-id
             cause-event-id payload budget))

(defn fail
  "Build a validated :intent/fail intent carrying a failure message for
  the given session/phenotype/node attribution, cause event, payload
  ({:message ...}), and budget."
  [session-id phenotype-id node-id cause-event-id payload budget]
  (assemble! :intent/fail session-id phenotype-id node-id
             cause-event-id payload budget))
