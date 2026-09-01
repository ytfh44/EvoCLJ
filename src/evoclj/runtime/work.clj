(ns evoclj.runtime.work
  "Work unified lifecycle — durable queued/running/waiting/succeeded/failed/cancelled/timed-out.

  W1 collapses the Session (8 states) × Command (6 states) = 48-state product
  into a single durable 7-state machine. Session becomes immutable context
  (pin: Genome/Resolution/CodeImage/Deployment/Generation), Work carries the
  lifecycle. Works replace commands + subagent_sessions portions.

  States (7, closed):

    :queued      — durably queued, not yet dispatched
    :running     — dispatched, actively executing
    :waiting     — paused waiting for input (subagent child, external signal)
    :succeeded   — terminal success
    :failed      — terminal failure
    :cancelled   — terminal cancellation
    :timed-out   — terminal deadline exceeded

  Transitions (closed, acyclic):

    :queued    -> #{:running :cancelled :failed}
    :running   -> #{:waiting :succeeded :failed :cancelled :timed-out}
    :waiting   -> #{:succeeded :failed :cancelled :timed-out}
    terminals -> #{}

  This is acyclic: queued < running < waiting < terminals in topological
  order, every edge moves forward. Four terminals are sinks. The graph
  satisfies Wolfram-style checks: edgesLegal, acyclic, terminals sink,
  queued->succeeded path and queued->running->timed-out path exist.

  Wolfram predicates (mirroring async-model [W-20..W-24]):

    edgesLegal?      — every transition endpoint inside the 7-state vocabulary
    acyclic?         — directed graph has no cycle
    terminals?       — succeeded/failed/cancelled/timed-out have no outgoing edges
    queued->succeeded-path? — a path queued -> ... -> succeeded exists
    queued->timed-out-path? — a path queued -> running -> timed-out exists

  The SM is definition > validation: this namespace defines the vocabulary;
  evoclj.store.work validates against it. Malli enum and DB mapping are
  single-source here."
  (:require [clojure.set :as set]))

;; ---------------------------------------------------------------------------
;; Vocabulary
;; ---------------------------------------------------------------------------

(def work-states
  "Closed Work state vocabulary (7 states)."
  #{:queued :running :waiting :succeeded :failed :cancelled :timed-out})

(def work-transitions
  "Closed Work transition table. Terminal states map to #{}."
  {:queued    #{:running :cancelled :failed}
   :running   #{:waiting :succeeded :failed :cancelled :timed-out}
   :waiting   #{:succeeded :failed :cancelled :timed-out}
   :succeeded #{}
   :failed    #{}
   :cancelled #{}
   :timed-out #{}})

(def terminal-states
  "States that accept no further transitions."
  #{:succeeded :failed :cancelled :timed-out})

(def initial-state :queued)

;; ---------------------------------------------------------------------------
;; DB mapping (single source; mirrors works.state CHECK)
;; ---------------------------------------------------------------------------

(def db-state->kw
  "DB string -> keyword. All 7 states are persisted."
  {"queued"    :queued
   "running"   :running
   "waiting"   :waiting
   "succeeded" :succeeded
   "failed"    :failed
   "cancelled" :cancelled
   "timed_out" :timed-out
   "timed-out" :timed-out})

(def kw->db-state
  "Keyword -> DB string. Every work state is persistable."
  {:queued    "queued"
   :running   "running"
   :waiting   "waiting"
   :succeeded "succeeded"
   :failed    "failed"
   :cancelled "cancelled"
   :timed-out "timed_out"})

;; ---------------------------------------------------------------------------
;; Malli enum
;; ---------------------------------------------------------------------------

(def work-state-enum
  "Malli [:enum ...] vector derived from work-states."
  (into [:enum] (sort work-states)))

;; ---------------------------------------------------------------------------
;; Pure helpers
;; ---------------------------------------------------------------------------

(defn work-state?
  [kw]
  (contains? work-states kw))

(defn db-state?
  [s]
  (contains? db-state->kw s))

(defn kw->db
  [kw]
  (get kw->db-state kw))

(defn db->kw
  [s]
  (get db-state->kw s))

(defn next-states
  [state]
  (get work-transitions state))

(defn valid-transition?
  [from to]
  (contains? (get work-transitions from #{}) to))

(defn valid-state?
  [kw]
  (work-state? kw))

(defn terminal?
  [kw]
  (contains? terminal-states kw))

;; ---------------------------------------------------------------------------
;; Wolfram-style verification predicates (pure, for tests)
;; ---------------------------------------------------------------------------

(defn edges-legal?
  "True when every transition endpoint is inside work-states (W-20 analogue)."
  []
  (every? (fn [[from tos]]
            (and (contains? work-states from)
                 (every? #(contains? work-states %) tos)))
          work-transitions))

(defn- has-cycle?
  "DFS cycle detection."
  []
  (let [visited (atom #{})
        stack (atom #{})
        found (atom false)]
    (letfn [(dfs [n]
              (when-not @found
                (swap! visited conj n)
                (swap! stack conj n)
                (doseq [m (get work-transitions n #{})]
                  (cond
                    (contains? @stack m) (reset! found true)
                    (not (contains? @visited m)) (dfs m)))
                (swap! stack disj n)))]
      (doseq [n work-states :when (not (contains? @visited n))]
        (dfs n))
      @found)))

(defn acyclic?
  "True when the directed graph is acyclic (W-21 analogue)."
  []
  (not (has-cycle?)))

(defn terminals-sink?
  "True when the four terminals have no outgoing edges (W-22 analogue)."
  []
  (every? #(empty? (get work-transitions %)) terminal-states))

(defn- reachable?
  "True when target is reachable from start via work-transitions."
  [start target]
  (loop [queue [start] visited #{}]
    (cond
      (empty? queue) false
      (contains? visited (first queue)) (recur (rest queue) visited)
      (= (first queue) target) true
      :else (let [cur (first queue)
                  nxt (get work-transitions cur #{})]
              (recur (into (vec (rest queue)) (remove visited nxt))
                     (conj visited cur))))))

(defn queued->succeeded-path?
  "True when a path queued -> ... -> succeeded exists (W-23 analogue)."
  []
  (reachable? :queued :succeeded))

(defn queued->timed-out-path?
  "True when a path queued -> running -> timed-out exists (W-24 analogue)."
  []
  (and (contains? (get work-transitions :queued #{}) :running)
       (contains? (get work-transitions :running #{}) :timed-out)))

(defn verify-work-sm
  "Run all Wolfram-style checks. Returns {:pass? bool :checks {k bool}}."
  []
  (let [checks {:edgesLegal (edges-legal?)
                :acyclic (acyclic?)
                :terminalsSink (terminals-sink?)
                :queuedToSucceededPath (queued->succeeded-path?)
                :queuedToTimedOutPath (queued->timed-out-path?)}]
    {:pass? (every? true? (vals checks))
     :checks checks}))

;; ---------------------------------------------------------------------------
;; Session × Command collapse (48 -> 7)
;; ---------------------------------------------------------------------------

(def session-x-command-product
  "48-state product size before collapse (8 session × 6 command)."
  48)

(defn work-states-count
  []
  (count work-states))

(defn collapse-ratio
  "Human-readable collapse description."
  []
  (format "Session×Command %d states collapses to Work %d" session-x-command-product (work-states-count)))
