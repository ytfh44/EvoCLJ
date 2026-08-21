(ns evoclj.analytics.behavior
  "Foundation F1 — the behavior-profile layer (component).

  Turns an event-log sequence into a structured BehaviorProfile: a
  closed Malli-validated map summarizing how a session actually
  behaved, plus a deterministic fingerprint and failure/summary
  projections. This is the pure analytics substrate for speciation,
  novelty search, anomaly detection, curriculum difficulty, and
  worst-case profiling — it never reads a store or a live session, it
  only folds over the events it is handed. It is deliberately
  store-agnostic: the profile accepts any sequential collection of
  maps carrying the three required keys (:event/seq int?, :event/type
  keyword?, :metadata map?) and ignores everything else, so the full
  public Event maps from evoclj.store.event satisfy the contract
  unchanged (extra keys are ignored).

  INPUT CONTRACT
    `profile-events`, `summarize-failures`, and `tool-usage-stats`
    accept a sequential collection of event maps, each with at least:
      {:event/seq int? :event/type keyword? :metadata map?}
    Invalid input — not sequential, or an element missing :event/seq
    (int?) / :event/type (keyword?) / :metadata (map?) — throws
    :analytics/events-invalid with a distinguishing :reason (:not-
    sequential | :malformed-element).

  DERIVATION RULES (used by profile-events)
    :behavior/session-id   the first event's :session/id (top-level,
                           falling back to :metadata :session/id), or
                           nil when absent.
    :behavior/n-events     the number of events folded.
    :behavior/intents      per-category counts over the six intent
                           categories. An event's intent type comes
                           from TWO documented sources, in order:
                           (a) :metadata :intent/type when present, or
                           (b) its :event/type when that keyword is in
                           the \"intent\" namespace (e.g. :intent/model-
                           call). The raw :intent/* keyword maps to a
                           category by dropping its namespace
                           (:intent/tool-call -> :tool-call); only the
                           six recognized categories are counted.
                           Tool ids for :tool-call come from
                           :metadata :tool/id or :resource/id,
                           stringified. Model calls are counted from
                           the :intent/model-call intent type (source
                           a and b cover the \"or metadata\" case).
    :behavior/failures     one entry per failing event, in event order.
                           A failure signal is :metadata carrying
                           :result/status :error, or an :error/type
                           key, or :intent/type :intent/fail, OR an
                           :event/type whose keyword form contains
                           \"fail\"/\"error\". Each is classified by the
                           failure taxonomy below and carries a small
                           EDN-safe :detail (the relevant metadata
                           submap, sanitized via evoclj.kernel.error/
                           sanitize so only serializable data crosses).
    :behavior/tool-seq     every tool id seen (:metadata :tool/id or
                           :resource/id, stringified) in event order,
                           keeping every invocation (no dedup).
    :behavior/status       from :metadata :session/state or :status
                           when present (string or keyword
                           \"completed\"/\"failed\"/\"budget-exhausted\"
                           map to keywords; other values -> :unknown);
                           when no explicit status is found it is
                           derived: any failure -> :failed, else
                           :completed.
    :behavior/wall-ms      the SUM of each event's first-present
                           number in :metadata :wall-ms or :duration-ms
                           (nil when no event carries either); wall-ms
                           is preferred over duration-ms per event.
    :behavior/resource     sums of :metadata :model-input-tokens,
                           :model-output-tokens, and :provider-calls
                           across all events (0 default each).

  FAILURE TAXONOMY
    A failing event is classified to exactly one of:
      :failure/schema      its :error/type keyword form contains
                           \"schema\" or \"invalid\" (a validation error).
      :failure/model       model-call related (intent :intent/model-
                           call, or the event keyword names \"model\").
      :failure/tool        tool-call related (intent :intent/tool-call,
                           or the event keyword names \"tool\"/\"
                           provider\").
      :failure/memory      memory-related (intent :intent/memory-read
                           or :intent/memory-write, or the event
                           keyword names \"memory\").
      :failure/unknown     no family matched.
    The schema check is the only override on intent family: a genuine
    validation error is reported as a schema failure even when it arose
    during a tool/model call.

  FINGERPRINT
    `fingerprint` folds the profile into a deterministic
    \"sha256:<64 hex>\" content address via evoclj.genome.hash/text-
    digest over the pr-str of a canonically-ordered copy of the
    profile (maps and sets sorted recursively by their pr-str key
    form), so equal logical profiles always hash alike and any change
    to the profile changes the hash.

  ERROR CONTRACT (Global Constraint 22 — plain serializable data)
    :analytics/events-invalid     (:reason :not-sequential |
                                  :malformed-element) — bad input.
    :analytics/profile-invalid    (:errors <humanized Malli
                                  explanation>) — defensive net; the
                                  reducer should never produce this."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err]))

;; --- behavioral profile schema ----------------------------------------------

(def BehaviorProfileSchema
  "The closed Malli contract for a BehaviorProfile (component).

  A behavior profile is fully EDN-safe: keywords, ints, strings,
  uuids, and small maps only. All key sets are closed; downstream
  consumers may rely on exactly these keys."
  [:map {:closed true}
   [:behavior/session-id [:maybe uuid?]]
   [:behavior/n-events int?]
   [:behavior/intents
    [:map {:closed true}
     [:tool-call [:map {:closed true}
                  [:count int?]
                  [:by-tool [:map-of string? int?]]]]
     [:model-call [:map {:closed true} [:count int?]]]
     [:memory-read [:map {:closed true} [:count int?]]]
     [:memory-write [:map {:closed true} [:count int?]]]
     [:finish [:map {:closed true} [:count int?]]]
     [:fail [:map {:closed true} [:count int?]]]]]
   [:behavior/failures
    [:vector [:map {:closed true}
              [:event/seq int?]
              [:failure/type keyword?]
              [:detail :map]]]]
   [:behavior/tool-seq [:vector string?]]
   [:behavior/status keyword?]
   [:behavior/wall-ms [:maybe number?]]
   [:behavior/resource
    [:map {:closed true}
     [:provider-calls int?]
     [:model-input-tokens int?]
     [:model-output-tokens int?]]]])

;; --- input validation -------------------------------------------------------

(defn- validate-events!
  "Enforce the input contract: a sequential collection of event maps
  each carrying :event/seq int?, :event/type keyword?, and :metadata
  map?. Returns the collection unchanged or throws
  :analytics/events-invalid."
  [events]
  (when-not (sequential? events)
    (throw (err/error :analytics/events-invalid
                      "events must be a sequential collection of event maps"
                      {:reason :not-sequential
                       :value (err/sanitize events)})))
  (doseq [e events]
    (when-not (and (map? e)
                   (int? (:event/seq e))
                   (keyword? (:event/type e))
                   (map? (:metadata e)))
      (throw (err/error :analytics/events-invalid
                        "each event must be a map with :event/seq int?, :event/type keyword?, and a :metadata map"
                        {:reason :malformed-element
                         :value (err/sanitize e)}))))
  events)

;; --- derivations (documented in the ns docstring) ---------------------------

(def ^:private intent-categories
  "The six recognized :intent/* types (evoclj.intent.schema v0 enum)."
  #{:intent/model-call :intent/tool-call :intent/memory-read
    :intent/memory-write :intent/finish :intent/fail})

(defn- category-key
  "Map a recognized :intent/* type to the :behavior/intents category
  key (:intent/tool-call -> :tool-call), or nil when unrecognized."
  [intent-type]
  (when (contains? intent-categories intent-type)
    (keyword (name intent-type))))

(defn- intent-type-of
  "The event's intent type from the two documented sources, in order:
  (a) :metadata :intent/type, else (b) its :event/type when that type
  is in the \"intent\" namespace. Returns a raw :intent/* keyword or
  nil."
  [event]
  (let [m (:metadata event)]
    (or (:intent/type m)
        (when (= "intent" (namespace (:event/type event)))
          (:event/type event)))))

(defn- tool-id-of
  "The stringified tool id for an event (:metadata :tool/id or
  :resource/id), or nil when neither is present. Keywords stringify by
  their bare name (:fs-read -> \"fs-read\") so tool ids read as the
  tool's identity, not its literal EDN form; non-keyword ids (strings)
  are kept as-is."
  [event]
  (let [m (:metadata event)
        id (or (:tool/id m) (:resource/id m))]
    (when id (if (keyword? id) (name id) (str id)))))

(defn- session-id-of
  "The first-present :session/id (top-level, falling back to
  :metadata :session/id), or nil."
  [event]
  (or (:session/id event)
      (:session/id (:metadata event))))

(def ^:private status-mapping
  "String form -> canonical session status keyword."
  {"completed" :completed
   "failed" :failed
   "budget-exhausted" :budget-exhausted})

(defn- status-of
  "The explicit session status encoded in an event's :metadata
  :session/state or :status (string or keyword), mapped to a canonical
  keyword. Returns nil when no explicit status is present, or
  :unknown when the value is not one of the three terminal statuses."
  [event]
  (let [v (or (:session/state (:metadata event))
              (:status (:metadata event)))]
    (when-not (nil? v)
      (get status-mapping (str/lower-case (if (keyword? v) (name v) (str v)))
           :unknown))))

(defn- wall-ms-of
  "The event's first-present number in :metadata :wall-ms or
  :duration-ms (wall-ms preferred), or nil."
  [event]
  (let [m (:metadata event)
        v (or (:wall-ms m) (:duration-ms m))]
    (when (number? v) v)))

(def ^:private failure-detail-keys
  "The small, EDN-safe metadata keys carried into a failure :detail."
  [:error/type :error/message :error/artifact-ref :reason :result/status
   :step :intent/type :node/id :limit :output/ref])

(defn- failure-signal?
  "True when the event carries a failure signal: a :metadata failure
  flag (:result/status :error, :error/type, :intent/type :intent/fail)
  OR an :event/type whose keyword form contains \"fail\"/\"error\"."
  [event]
  (let [m (:metadata event)
        t (str (:event/type event))]
    (or (= :error (:result/status m))
        (contains? m :error/type)
        (= :intent/fail (:intent/type m))
        (boolean (re-find #"(?i)fail|error" t)))))

(defn- classify-failure
  "Classify a failing event into the documented failure taxonomy. The
  schema check (:error/type naming schema/invalid) is the only
  override on intent family."
  [event]
  (let [m (:metadata event)
        et (:error/type m)
        itype (intent-type-of event)
        t (str (:event/type event))]
    (if (and et (re-find #"(?i)schema|invalid" (str et)))
      :failure/schema
      (cond
        (or (= :intent/model-call itype)
            (re-find #"(?i)model" t))
        :failure/model
        (or (= :intent/tool-call itype)
            (re-find #"(?i)tool|provider" t))
        :failure/tool
        (or (#{:intent/memory-read :intent/memory-write} itype)
            (re-find #"(?i)memory" t))
        :failure/memory
        :else :failure/unknown))))

(defn- failure-detail
  "The relevant metadata submap for a failure, sanitized so exactly
  EDN-safe data crosses (Global Constraint 22)."
  [event]
  (err/sanitize (select-keys (:metadata event) failure-detail-keys)))

(defn- failure-entry
  "The {:event/seq ... :failure/type ... :detail ...} entry for a
  failing event, or nil when the event carries no failure signal."
  [event]
  (when (failure-signal? event)
    {:event/seq (:event/seq event)
     :failure/type (classify-failure event)
     :detail (failure-detail event)}))

(defn- zeroed-acc
  "The empty reducer accumulator."
  []
  {:n-events 0
   :session/id nil
   :intents {:tool-call {:count 0 :by-tool {}}
             :model-call {:count 0}
             :memory-read {:count 0}
             :memory-write {:count 0}
             :finish {:count 0}
             :fail {:count 0}}
   :failures []
   :tool-seq []
   :explicit-status nil
   :wall-ms 0
   :wall-seen? false
   :resource {:provider-calls 0 :model-input-tokens 0 :model-output-tokens 0}})

;; --- entry point ------------------------------------------------------------

(defn profile-events
  "Fold `events` (a sequential collection of event maps) into a closed
  BehaviorProfile (see the namespace docstring for every derivation
  rule). Invalid input throws :analytics/events-invalid; a defensive
  Malli check of the assembled profile throws
  :analytics/profile-invalid."
  [events]
  (validate-events! events)
  (let [acc (reduce
             (fn [acc event]
               (let [m (:metadata event)
                     itype (intent-type-of event)
                     cat (category-key itype)
                     tid (tool-id-of event)
                     ;; intents: a recognized intent type bumps that
                     ;; category; tool calls also track by-tool.
                     acc (if cat
                           (if (= :tool-call cat)
                             (update-in acc [:intents :tool-call :count] inc)
                             (update-in acc [:intents cat :count] inc))
                           acc)
                     acc (if (and cat (= :tool-call cat) tid)
                           (update-in acc [:intents :tool-call :by-tool]
                                      (fn [bt] (update bt tid (fnil inc 0))))
                           acc)
                     ;; tool-seq: every tool id in event order, no dedup
                     acc (if tid (update acc :tool-seq conj tid) acc)
                     ;; failures
                     acc (if (failure-signal? event)
                           (update acc :failures conj (failure-entry event))
                           acc)
                     ;; explicit session status (first present wins)
                     acc (if-let [s (status-of event)]
                           (if (nil? (:explicit-status acc))
                             (assoc acc :explicit-status s)
                             acc)
                           acc)
                     ;; wall time: sum first-present wall-ms/duration-ms
                     wall (wall-ms-of event)
                     acc (if wall (update acc :wall-ms + wall) acc)
                     acc (if (and wall (not (:wall-seen? acc)))
                           (assoc acc :wall-seen? true)
                           acc)
                     ;; resource counters
                     acc (-> acc
                             (update :resource
                                     (fn [r]
                                       (-> r
                                           (update :provider-calls
                                                   + (int (or (:provider-calls m) 0)))
                                           (update :model-input-tokens
                                                   + (int (or (:model-input-tokens m) 0)))
                                           (update :model-output-tokens
                                                   + (int (or (:model-output-tokens m) 0)))))))
                     ;; session id: first present wins
                     acc (if (and (nil? (:session/id acc))
                                  (session-id-of event))
                           (assoc acc :session/id (session-id-of event))
                           acc)]
                 (update acc :n-events inc)))
             (zeroed-acc)
             events)
        profile {:behavior/session-id (:session/id acc)
                 :behavior/n-events (:n-events acc)
                 :behavior/intents (:intents acc)
                 :behavior/failures (:failures acc)
                 :behavior/tool-seq (:tool-seq acc)
                 :behavior/status (or (:explicit-status acc)
                                      (if (seq (:failures acc))
                                        :failed
                                        :completed))
                 :behavior/wall-ms (if (:wall-seen? acc) (:wall-ms acc) nil)
                 :behavior/resource (:resource acc)}]
    (when-let [expl (m/explain BehaviorProfileSchema profile)]
      (throw (err/error :analytics/profile-invalid
                        "assembled profile does not satisfy the BehaviorProfile contract"
                        {:errors (me/humanize expl)})))
    profile))

;; --- fingerprint ------------------------------------------------------------

(defn- canonical
  "Deterministic EDN form for hashing: maps and sets sorted by their
  pr-str key/element form, nested values collapsed recursively so equal
  logical content always pr-strs identically (the same convention the
  repo uses in evoclj.eval.replay)."
  [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical) x)
    (vector? x) (mapv canonical x)
    (seq? x) (mapv canonical x)
    :else x))

(defn fingerprint
  "A deterministic \"sha256:<64 hex>\" content address for a
  BehaviorProfile: the pr-str of a canonically-ordered copy of `profile`
  hashed via evoclj.genome.hash/text-digest. Equal logical profiles
  hash alike; any change to the profile changes the hash."
  [profile]
  (hash/text-digest (pr-str (canonical profile))))

;; --- summaries --------------------------------------------------------------

(defn summarize-failures
  "Group a sequential event collection's failing events by failure
  taxonomy class. Returns [{:failure/type keyword? :count int?
  :event/seqs [int? ...]} ...] sorted by :count descending, then by
  :failure/type. Invalid input throws :analytics/events-invalid."
  [events]
  (validate-events! events)
  (->> events
       (keep failure-entry)
       (group-by :failure/type)
       (map (fn [[t es]]
              {:failure/type t
               :count (count es)
               :event/seqs (mapv :event/seq es)}))
       (sort-by (juxt (comp - :count) :failure/type))
       vec))

(defn tool-usage-stats
  "Per-tool usage over a sequential event collection. Returns
  [{:tool/id string? :calls int? :first-seq int? :last-seq int?} ...]
  sorted by :tool/id. Tool ids come from :metadata :tool/id or
  :resource/id (stringified). Invalid input throws
  :analytics/events-invalid."
  [events]
  (validate-events! events)
  (->> events
       (keep (fn [e]
               (when-let [id (tool-id-of e)]
                 {:tool/id id :seq (:event/seq e)})))
       (group-by :tool/id)
       (map (fn [[id entries]]
              (let [seqs (mapv :seq entries)]
                {:tool/id id
                 :calls (count entries)
                 :first-seq (apply min seqs)
                 :last-seq (apply max seqs)})))
       (sort-by :tool/id)
       vec))
