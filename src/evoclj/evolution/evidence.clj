(ns evoclj.evolution.evidence
  "Evidence selection and frozen evidence packs (component).

  build-evidence-pack freezes a reproducible evidence boundary for one
  generation: it selects episodes of that generation whose causal
  trace ends AT OR BEFORE the immutable :cutoff-event-id, ranks them
  deterministically, writes each selected episode's full trace
  excerpt to the filesystem CAS (Global Constraint 21 — large payloads
  live by content hash, the pack carries compact metadata and refs),
  and returns an immutable content-addressed pack:

      {:evidence/id \"sha256:<64 hex>\"
       :generation/id \"G42\"
       :cutoff-event-id 9001
       :episodes [{:episode/id ... :session/id ... :generation/id ...
                   :excerpt-ref \"sha256:...\" :outcome ... :trace ...
                   :usage ...} ...]   ; :usage ONLY when the episode
                                       ; carries model usage (roadmap E5)
       :summary {:selector ... :seed ... :eligible n :selected n
                 :successes n :failures n :high-cost n
                 :usage ...}}          ; aggregate when any selected
                                       ; episode carries usage, ABSENT
                                       ; otherwise (never zero)

  CUTOFF IMMUTABILITY (Step 2): eligibility is decided purely by the
  request's :cutoff-event-id against each episode's stored
  :last-event. Episodes whose trace ends after the cutoff — including
  episodes materialized after the pack was built — can never enter
  that pack, because the pack is a pure function of (store, request):
  rebuilding it with the same request reproduces the same :evidence/id
  and the same :episodes byte-for-byte. The pack itself is stored in
  the CAS under its own content hash, so :evidence/id IS the content
  address of the frozen pack body (the mutations table's
  'ArtifactId of the frozen evidence pack').

  SELECTION (Step 1 + 3): the selector's quotas are applied to the
  FULL eligible set ranked by recency (largest :last-event first;
  equal recency broken deterministically by episode id):
    :recent n            — the n most recent episodes
    :include-successes n — up to n most recent success episodes
                           (backfill when the recent pool is
                           failure-skewed)
    :include-failures n  — up to n most recent failure episodes
    :include-high-cost n — up to n most recent high-cost episodes
  The pack is the deduplicated union, re-ranked by recency, so BOTH
  successes and failures are always represented (the optimizer can
  never learn only from failures and destroy already-correct
  behavior). The pack stays bounded: at most :recent +
  :include-successes + :include-failures + :include-high-cost
  episodes. A success is an episode whose :outcome :status is
  :completed; every other terminal outcome (:failed,
  :budget-exhausted, :cancelled) is a failure — failures are evidence,
  not discarded traces. An episode is high-cost when its :usage cost
  (:total-cost, falling back to :cost) is positive.

  SEED POLICY (Step 3): selection needs no randomness — recency ties
  are broken by episode id, so the same (store, request) always
  reproduces the same pack. A caller may still supply an optional
  :seed in the selector; when present it replaces the tie-break with
  the deterministic sha256(seed | episode-id) order, and the seed IS
  persisted in the pack's :summary (and inside :summary :selector),
  so any seeded selection is reproducible from the pack alone.

  EXCERPTS (Step 4): for every selected episode a CAS artifact carries
  the episode's full event trace (filtered to its :trace range) plus
  the ORIGINAL episode provenance (:episode/id, :session/id,
  :generation/id, :trace), so a Diagnostician consuming an excerpt can
  cite episodes by id. The pack entry is compact metadata only — no
  trace payload bytes ever cross into the pack.

  `store` is the executor :stores map {:sqlite <db> :cas <CAS root>},
  exactly as in evoclj.runtime.episode; both handles arrive open and
  this namespace opens and closes nothing.

  Error contract (Global Constraint 22 — plain serializable data):
  :evidence/store-invalid (:reason :not-a-map :sqlite-missing
  :cas-missing), :evidence/request-invalid, :evidence/episode-invalid,
  :evidence/pack-invalid, :evidence/excerpt-invalid (Malli
  explanations). CAS/store errors (:store/cas-*) propagate as-is.

  USAGE ENRICHMENT (roadmap E5): a pack entry carries :usage only when
  its episode carries model usage from the model-call channel (token
  counts, cost estimate — component counters); the pack summary
  aggregates those counters over the SELECTED episodes. Unknown usage
  is ABSENT — never fabricated as zeros (honest accounting)."
  (:require [clojure.edn :as edn]
            [evoclj.evolution.evidence-schema :as es]
            [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err]
            [evoclj.runtime.usage :as usage]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.util UUID)))

(def excerpt-version
  "Version of the trace-excerpt artifact format."
  1)

(def ^:private excerpt-media-type "application/edn")

(defn- utf8-bytes
  "UTF-8 bytes of a string (the serialization convention of
  evoclj.runtime.scheduler/put-payload!)."
  [s]
  (.getBytes ^String s StandardCharsets/UTF_8))

;; --- store trust boundary ----------------------------------------------------

(defn- validate-store!
  "Validate the store trust boundary: the executor :stores map
  {:sqlite <db> :cas <CAS root>}."
  [store]
  (when-not (map? store)
    (throw (err/error :evidence/store-invalid
                      "store must be the executor :stores map {:sqlite ... :cas ...}"
                      {:reason :not-a-map :value (err/sanitize store)})))
  (when-not (contains? store :sqlite)
    (throw (err/error :evidence/store-invalid
                      "store must carry the :sqlite handle"
                      {:reason :sqlite-missing})))
  (when-not (contains? store :cas)
    (throw (err/error :evidence/store-invalid
                      "store must carry the :cas handle"
                      {:reason :cas-missing})))
  store)

;; --- row mapping -------------------------------------------------------------

(defn- row->episode
  "Convert an episodes DB row into the Episode contract map (the same
  shape materialize-episode! produces). The :usage column is nullable
  (EDN map or NULL — roadmap E5): when NULL the key is omitted
  entirely, so unknown usage never enters the pack as a fabricated
  zero."
  [row]
  (let [usage (some-> (:usage row) edn/read-string)]
    (cond-> {:episode/id (UUID/fromString (:id row))
             :session/id (UUID/fromString (:session_id row))
             :generation/id (:generation_id row)
             :genome/id (:genome_id row)
             :resolution/id (:resolution_id row)
             :task-ref (:task_ref row)
             :trace {:first-event (:first_event_id row)
                     :last-event (:last_event_id row)}
             :outcome (edn/read-string (:outcome row))}
      usage (assoc :usage usage))))

;; --- classification ----------------------------------------------------------

(defn- success?
  "A success episode: :outcome :status :completed."
  [episode]
  (= :completed (get-in episode [:outcome :status])))

(defn- failure?
  "A failure episode: any terminal outcome other than :completed
  (:failed, :budget-exhausted, :cancelled). Failures are evidence,
  not discarded traces."
  [episode]
  (not (success? episode)))

(defn- episode-cost
  "The numeric cost of an episode from its :usage map: :total-cost,
  falling back to :cost, else 0.0. v0 usage accounting (component)
  will populate these keys; until then only fabricated usage carries
  cost."
  [episode]
  (let [u (:usage episode)]
    (or (some-> (:total-cost u) double)
        (some-> (:cost u) double)
        0.0)))

(defn- high-cost?
  "A high-cost episode: a strictly positive usage cost."
  [episode]
  (pos? (episode-cost episode)))

(defn- summary-usage
  "The model usage of the selected evidence (roadmap E5): the component counters of the selected episodes accumulated with the standard
  evoclj.runtime.usage merge (counters sum — monotonic accounting).
  Attribution keys (which may be non-numeric) are dropped so the
  summary stays a numeric-only usage map. Returns {} when no selected
  episode carries usage — the caller then omits :usage from the
  summary entirely (unknown is ABSENT, never zero: honest
  accounting)."
  [episodes]
  (let [total (usage/aggregate (keep :usage episodes))]
    (into {} (filter (fn [[_ v]] (number? v))) total)))

;; --- deterministic ranking ---------------------------------------------------

(defn- rank-key
  "Recency: the episode's :last-event (the trace bound that also
  decides cutoff eligibility)."
  [episode]
  (get-in episode [:trace :last-event]))

(defn- tie-key
  "Deterministic tie-break key for episodes of equal recency: the
  episode id string, or the sha256 of (seed | episode id) when the
  selector carries a seed. Both orders are total and deterministic."
  [seed episode]
  (if seed
    (hash/text-digest (str seed "|" (:episode/id episode)))
    (str (:episode/id episode))))

(defn- recency-comparator
  "Total order for selection: most recent (largest :last-event) first;
  equal recency is resolved by tie-key (episode id, or the seeded
  digest). Returns a raw comparison fn — Clojure fns implement
  java.util.Comparator, which is what sort requires — so the compare
  RESULT (a number) is honored. Note: clojure.core/comparator must NOT
  be used here: it interprets its argument as a boolean predicate and
  would reorder by truthiness instead of by the compare value.
  Sorting with this comparator is deterministic, so the same eligible
  set always yields the same selection."
  [seed]
  (fn [a b]
    (let [c (compare (rank-key b) (rank-key a))]
      (if (zero? c)
        (compare (tie-key seed a) (tie-key seed b))
        c))))

;; --- selection ---------------------------------------------------------------

(defn- select-episodes
  "Deterministic evidence selection over the eligible episode set.

  The selector's quotas are applied to the FULL eligible set ranked by
  recency (ties broken deterministically — see recency-comparator):
    - :recent n            — the n most recent episodes
    - :include-successes n — up to n most recent successes (backfill
                             when the recent pool is success-poor)
    - :include-failures n  — up to n most recent failures
    - :include-high-cost n — up to n most recent high-cost episodes
  The pack is the deduplicated union, re-ranked by recency, so a
  failure-skewed recent window still shows the optimizer successes
  (component Step 1) and vice versa. The pack stays bounded: at most
  :recent + :include-successes + :include-failures + :include-high-cost
  episodes.

  Returns {:ordered [...] :counts {:selected n :successes n
  :failures n :high-cost n}}."
  [eligible {:keys [recent include-successes include-failures include-high-cost]
             :or {recent 0 include-successes 0 include-failures 0
                  include-high-cost 0}
             :as selector}]
  (let [seed (:seed selector)
        by-recency (sort (recency-comparator seed) eligible)
        take-n (fn [n pred] (take n (filter pred by-recency)))
        recent-pool (take recent by-recency)
        successes (take-n include-successes success?)
        failures (take-n include-failures failure?)
        high-cost (take-n include-high-cost high-cost?)
        chosen (distinct (concat recent-pool successes failures high-cost))
        ordered (sort (recency-comparator seed) chosen)]
    {:ordered ordered
     :counts {:selected (count ordered)
              :successes (count (filter success? ordered))
              :failures (count (filter failure? ordered))
              :high-cost (count (filter high-cost? ordered))}}))

;; --- trace excerpts ----------------------------------------------------------

(defn- build-excerpt
  "The trace excerpt for one episode: the full event trace of the
  episode's causal range [:first-event :last-event] (inclusive), read
  from the append-only event log, plus the ORIGINAL episode provenance
  (:episode/id, :session/id, :generation/id, :trace) so consumers can
  cite episodes by id."
  [db episode]
  (let [{:keys [trace]} episode
        events (->> (event/events-for-session db (:session/id episode))
                    (filter #(<= (:first-event trace)
                                 (:event/id %)
                                 (:last-event trace)))
                    vec)]
    {:excerpt/version excerpt-version
     :episode/id (:episode/id episode)
     :session/id (:session/id episode)
     :generation/id (:generation/id episode)
     :trace trace
     :events events}))

;; --- content addressing ------------------------------------------------------

(defn- canonical
  "Deterministic EDN form for hashing: maps sorted by their pr-str key
  form, sets by their pr-str element form, collections realized
  eagerly. Any EDN-safe value yields a stable pr-str, so the content
  hash is a pure function of logical content (Global Constraint 6)."
  [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical) x)
    (vector? x) (mapv canonical x)
    (seq? x) (mapv canonical x)
    :else x))

(defn- pack-digest
  "The evidence id: a content hash over the canonical pack data using
  the deterministic text conventions of evoclj.genome.hash (UTF-8, LF
  line endings, sha256:<64 hex>). The frozen pack body is stored in
  the CAS under exactly these bytes, so :evidence/id IS the content
  address of the pack artifact."
  [data]
  (hash/text-digest (pr-str (canonical data))))

;; --- the freeze --------------------------------------------------------------

(defn build-evidence-pack
  "Freeze a bounded, immutable evidence pack for one generation and
  return it (component).

  Eligible episodes are the rows of the episodes table with the
  request's :generation/id whose :trace :last-event ≤ the IMMUTABLE
  :cutoff-event-id (inclusive). They are ranked deterministically by
  recency and selected by the :selector quotas (successes AND failures
  always represented — see select-episodes). Each selected episode's
  full trace excerpt is written to the CAS as an artifact that
  preserves the original episode provenance; the pack entry carries
  only compact metadata and the :excerpt-ref. The canonical pack data
  is itself stored in the CAS, so the returned :evidence/id resolves
  to the frozen pack body by content hash. Rebuilding with the same
  request — even after new episodes with later traces arrive —
  reproduces the same :evidence/id and :episodes (Step 2).

  Typed errors: :evidence/store-invalid, :evidence/request-invalid,
  :evidence/episode-invalid, :evidence/pack-invalid,
  :evidence/excerpt-invalid. CAS/store errors propagate as-is."
  [store request]
  (validate-store! store)
  (es/validate-request request)
  (let [db (:sqlite store)
        generation-id (:generation/id request)
        cutoff-event-id (:cutoff-event-id request)
        selector (:selector request)
        eligible (->> (sqlite/query db
                                    ["SELECT * FROM episodes
                                      WHERE generation_id = ? AND last_event_id <= ?
                                      ORDER BY last_event_id ASC, id ASC"
                                     generation-id cutoff-event-id])
                      (mapv (fn [row] (es/validate-episode (row->episode row)))))
        {:keys [ordered counts]} (select-episodes eligible selector)
        entries (mapv (fn [episode]
                        (let [excerpt (build-excerpt db episode)
                              _ (es/validate-excerpt excerpt)
                              put-result
                              (cas/put-bytes! (:cas store)
                                              (utf8-bytes (pr-str (canonical excerpt)))
                                              {:media-type excerpt-media-type})]
                          (cond-> {:episode/id (:episode/id episode)
                           :session/id (:session/id episode)
                           :generation/id (:generation/id episode)
                           :excerpt-ref (:artifact/id put-result)
                           :outcome (:outcome episode)
                           :trace (:trace episode)}
                          ;; roadmap E5: usage is included ONLY when the
                          ;; episode carries it — unknown usage is ABSENT,
                          ;; never a fabricated zero (honest accounting)
                          (seq (:usage episode))
                          (assoc :usage (:usage episode)))))
                      ordered)
        usage-total (summary-usage ordered)
        data {:generation/id generation-id
              :cutoff-event-id cutoff-event-id
              :episodes entries
              :summary (cond-> {:selector selector
                                :seed (:seed selector)
                                :eligible (count eligible)
                                :selected (:selected counts)
                                :successes (:successes counts)
                                :failures (:failures counts)
                                :high-cost (:high-cost counts)}
                         ;; roadmap E5: the aggregate appears only when
                         ;; some selected episode carries usage
                         (seq usage-total) (assoc :usage usage-total))}
        id (pack-digest data)
        ;; freeze: the canonical pack data IS stored under its own
        ;; content hash, so :evidence/id is a resolvable ArtifactId
        _ (cas/put-bytes! (:cas store) (utf8-bytes (pr-str (canonical data)))
                          {:media-type excerpt-media-type})
        pack (assoc data :evidence/id id)]
    (es/validate-pack pack)
    pack))
