(ns evoclj.runtime.subagent-capability
  "Capability narrowing for subagent spawn (Grant meet, not identity).

  The model's :capabilities arg (vector of hint strings on :agent/spawn,
  carried into child-spec) is a NARROWING request, not audit metadata:
  each parent lease meets the request via Grant meet and disjoint leases
  are dropped, so requesting fewer caps yields fewer child leases.
  Default derivation is meet, not identity: the implicit spawn right
  {:kind :tool :id :agent/spawn} is denied unless explicitly requested.
  Depth (5) and fanout (10) remain as fail-closed backstops; task-level
  planning (the :task + :capabilities request) is primary.

  Split out of evoclj.runtime.subagent, which had grown to 1400+ lines
  across three unrelated concerns. This namespace is a pure leaf: it
  depends only on clojure.string and the capability grant/mint modules,
  and nothing it requires reaches the spawn lifecycle — so the split
  introduces no cycle."
  (:require [clojure.string :as str]
            [evoclj.capability.grant :as grant]
            [evoclj.capability.mint :as mint]))

(def ^:private spawn-tool-id
  "Tool id whose inheritance is denied by default (must be explicitly requested)."
  :agent/spawn)

(defn- parse-capability-hint
  "Parse one model capability hint string into a narrowing request map
  {:resource {...} :actions (set-or-nil)}; nil actions means resource-only
  narrowing (keep the parent's actions). Returns nil for unparseable hints
  (fail-closed: unknown hints match nothing).
  Grammar: \"<kind>:<id>[:<action>[,<action>...]]\" where kind is one of
  tool, memory, model, filesystem; a bare \"ns/name\" is a tool id."
  [s]
  (when (and (string? s) (seq (str/trim s)))
    (let [trimmed (str/trim s)
          parts (str/split trimmed #":")
          kw-id (fn [id] (when (seq id) (keyword id)))
          actions-of (fn [a] (when (seq a)
                               (into #{} (comp (map str/trim)
                                               (filter seq)
                                               (map keyword))
                                         (str/split a #","))))]
      (cond
        (= 1 (count parts))
        (when-let [id (kw-id (first parts))]
          {:resource {:kind :tool :id id} :actions nil})
        :else
        (let [[kind id & rest] parts
              actions (when (seq rest) (actions-of (str/join ":" rest)))]
          (cond
            (and (= kind "tool") (seq id))
            (when-let [tid (kw-id id)]
              {:resource {:kind :tool :id tid} :actions actions})
            (and (= kind "memory") (seq id))
            (when-let [mid (kw-id id)]
              {:resource {:kind :memory :id mid} :actions actions})
            (and (= kind "model") (seq id))
            {:resource {:kind :model :id id} :actions actions}
            (and (= kind "filesystem") (seq id))
            {:resource {:kind :filesystem :path id} :actions actions}
            :else nil))))))

(defn- capability-requests
  "Narrowing requests from child-spec's :capabilities (vector of hint
  strings, or already-structured {:resource _} maps for compat).
  Unparseable entries match nothing and are dropped."
  [child-spec]
  (let [caps (:capabilities (or child-spec {}))]
    (when (sequential? caps)
      (into []
            (comp (map (fn [c]
                         (cond
                           (string? c) (parse-capability-hint c)
                           (and (map? c) (map? (:resource c)))
                           {:resource (:resource c)
                            :actions (when (:actions c)
                                       (if (set? (:actions c)) (:actions c) (set (:actions c))))}
                           :else nil)))
                  (filter some?))
            caps))))


(defn- spawn-requested?
  "True when the narrowing requests explicitly name the spawn right."
  [requests]
  (boolean (some #(= {:kind :tool :id spawn-tool-id} (:resource %)) requests)))

(defn- keep-spawn-lease?
  "True unless `parent-lease` carries the spawn tool right and the request
  did not name it (the spawn right is denied by default)."
  [parent-lease keep-spawn?]
  (or keep-spawn?
      (not= spawn-tool-id (:id (:resource parent-lease)))))

(defn- narrow-one-parent
  "Apply the child-grant = parent-grant ⊓ request algebra to one parent.
  With no requests, returns [identity child lease]. With requests, returns
  one child lease per non-nil meet (or [] when every meet is nil)."
  [db registry parent-lease child-principal requests]
  (if (empty? requests)
    [(mint/derive-lease! db registry parent-lease {:principal child-principal})]
    (let [cands (filter #(= (:kind (:resource parent-lease))
                            (:kind (:resource %)))
                        requests)]
      (into []
            (keep (fn [request]
                    (when-let [g (grant/meet {:resource (:resource parent-lease)
                                              :actions  (or (:actions parent-lease) #{})}
                                             {:resource (:resource request)
                                              :actions  (or (:actions request)
                                                           (:actions parent-lease))})]
                      (mint/derive-lease! db registry parent-lease
                                           {:principal child-principal
                                            :resource (:resource g)
                                            :actions  (:actions g)}))))
            cands))))

(defn derive-child-leases
  "Derive child leases from parent leases narrowed by child-spec :capabilities.
  Algebra: child = parent ⊓ request, one lease per non-nil meet (grant/meet).
  With no parsable request, the child is the parent itself (identity).
  Disjoint parents (no non-nil meet against any request) produce nothing.
  The spawn right is denied by default unless the model explicitly requested it."
  [db registry parent-leases child-spec child-principal]
  (let [requests (capability-requests child-spec)
        keep-spawn? (spawn-requested? requests)]
    (into []
          (mapcat (fn [pl]
                    (if (keep-spawn-lease? pl keep-spawn?)
                      (narrow-one-parent db registry pl child-principal requests)
                      [])))
          parent-leases)))
