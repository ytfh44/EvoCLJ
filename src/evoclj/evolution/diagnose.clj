(ns evoclj.evolution.diagnose
  "Diagnostician contract and the deterministic pattern adapter (component).

  The Diagnostician protocol (Step 3) is the single evolution-facing
  diagnosis contract:

      (defprotocol Diagnostician
        (diagnose [d evidence-pack]))

  A Diagnostician consumes exactly ONE value — the frozen,
  content-addressed Evolution-set evidence pack produced by
  evoclj.evolution.evidence/build-evidence-pack (component) — and
  returns a validated, content-addressed Diagnosis:

      {:diagnosis/id \"sha256:...\"          ; content hash of the body
       :evidence/id \"sha256:...\"          ; provenance: the frozen pack
       :hypotheses [{:hypothesis/id #uuid   ; deterministic name-based UUID
                     :pattern :task/success
                     :claim \"...\"
                     :support [{:episode/id ... :event-ids [...]}]
                     :counterevidence [{:episode/id ...}]
                     :target {:kind :workflow :id :task}
                     :expected-effect {:metric :task/success :direction :increase}
                     :confidence 0.7        ; numeric, [0,1], the ranking key
                     :confidence-band :medium}]}

  THE DETERMINISTIC ADAPTER (Step 3): `pattern-diagnostician` is the
  first adapter — a pure, deterministic pattern scanner. It reads the
  pack's episode summaries and emits BOUNDED hypotheses:

    :task/success   — fires when the pack's task success rate
                      (successes / selected, from the pack's
                      :summary) is BELOW the configured
                      :task/success-threshold; supported by each
                      failure episode, countered by each success
                      episode.
    :task/high-cost — fires when at least one episode carries a
                      positive usage cost; supported by each
                      high-cost episode, countered by the zero-cost
                      episodes.

  Every supporting episode cites its :trace :last-event — the
  terminal event of the causal trace where the episode's outcome was
  recorded — because the adapter consumes ONLY pack metadata and holds
  no store handle to dereference the CAS excerpt. Hypothesis ids are
  deterministic name-based (v3) UUIDs over the hypothesis content and
  :diagnosis/id is the sha256 content hash of the diagnosis body, so
  the same (config, pack) always yields the same diagnosis
  byte-for-byte. A future LLM adapter conforms to the same protocol.

  HYPOTHESIS RANKING (roadmap E2): every pattern hypothesis carries a
  numeric :confidence in [0,1] — the share of the pack's episodes that
  substantiate the pattern (failure share for :task/success, costly
  share for :task/high-cost). The diagnosis' hypotheses are the output
  of `rank-hypotheses`, the kernel re-validation gate applied before
  adoption: it re-validates every :confidence (malformed — missing,
  non-numeric, or outside [0,1] — is rejected with the typed
  :evolution/hypothesis-confidence-invalid error) and adopts unordered
  input in validated DESCENDING-confidence order, breaking ties
  deterministically by :hypothesis/id, so the same hypotheses always
  adopt in the same order across runs (Global Constraint 6).

  EVOLUTION-SET EVIDENCE ONLY (Step 4, Global Constraint 11): the
  adapter's constructor receives ONLY a plain pattern-config map — no
  store handle and no Selection/Audit fixture handle. The config
  schema is closed (unknown keys are rejected), the record holds
  exactly one field (:config), and `diagnose` reads nothing but the
  pack it is handed, so candidate-evaluation selection data can never
  leak into a diagnosis.

  PERSISTENCE (Step 5): persistence is a SEPARATE step
  (persist-diagnosis!) so the adapter itself stays pure and
  deterministic. The diagnosis body (everything except :diagnosis/id)
  is stored in the filesystem CAS under its own content hash — the
  :diagnosis/id must BE that hash, so a forged id is rejected — and
  the artifacts registry row is written when the tables allow (it
  does: 001-init.sql creates `artifacts` with hash as PRIMARY KEY; the
  INSERT OR IGNORE keeps the write idempotent). The body embeds
  :evidence/id, so the artifact is self-provenancing back to the
  frozen evidence pack.

  INPUT CONTEXT (component): `build-context` assembles the compact EDN
  summary the Diagnostician consumes alongside the frozen pack — the
  candidate's BehaviorProfile (evoclj.analytics.behavior/profile-events
  plus its stable sha256 fingerprint) — present only when evidence
  events are available.

  NO GENOME ALTERATION: there is no API in this namespace that writes
  to genomes, generations, kernel source, or any evolution asset — the
  only store write is the CAS artifact + artifacts registry row above
  (Global Constraints 10, 12, 19: episodic reasoning stays distinct
  from procedural Genome changes, a candidate never modifies the
  evaluator that judges it, and kernel/authority/audit/
  evaluator-isolation/promotion roots are never agent-mutable). No
  free-form diagnosis can directly alter the Genome; this is enforced
  by design — the API does not exist.

  Error contract (Global Constraint 22 — plain serializable data):
  :diagnosis/config-invalid, :diagnosis/hypothesis-invalid,
  :diagnosis/invalid, :diagnosis/store-invalid, :diagnosis/id-mismatch,
  :evolution/hypothesis-confidence-invalid (the kernel ranking gate).
  Invalid evidence packs are rejected with the component
  :evidence/pack-invalid error; CAS/store errors propagate as-is."
  (:require [evoclj.analytics.behavior :as behavior]
            [evoclj.evolution.diagnosis-schema :as ds]
            [evoclj.evolution.evidence-schema :as es]
            [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.util Locale UUID)))

(defprotocol Diagnostician
  "The Diagnostician contract (component Step 3).

  (diagnose d evidence-pack) consumes exactly one value: the frozen
  Evolution-set evidence pack (component). It returns a validated
  Diagnosis {:diagnosis/id :evidence/id :hypotheses [...]}. The
  adapter holds no store handle and no Selection/Audit fixture handle
  (Global Constraint 11); everything a Diagnostician can see is
  exactly the pack it is handed."
  (diagnose [d evidence-pack]
    "Return a Diagnosis for the frozen Evolution-set evidence pack."))

;; --- content addressing ------------------------------------------------------

(def ^:private diagnosis-media-type "application/edn")

(defn- utf8-bytes
  [s]
  (.getBytes ^String s StandardCharsets/UTF_8))

(defn- canonical
  "Deterministic EDN form for hashing — the same convention as
  evoclj.evolution.evidence/canonical: maps sorted by their pr-str key
  form, sets by their pr-str element form, collections realized
  eagerly. The diagnosis id is a pure function of logical content
  (Global Constraint 6)."
  [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical) x)
    (vector? x) (mapv canonical x)
    (seq? x) (mapv canonical x)
    :else x))

(defn- digest
  "Content hash (sha256:<64 hex>) of the canonical pr-str of `data`."
  [data]
  (hash/text-digest (pr-str (canonical data))))

(defn- deterministic-uuid
  "A deterministic name-based UUID (v3) over the canonical pr-str of
  `data`: identical logical content always maps to the same id, so a
  diagnosis is a pure function of its evidence pack."
  [data]
  (UUID/nameUUIDFromBytes (utf8-bytes (pr-str (canonical data)))))

;; --- the diagnose input context (component) ------------------------------------

(defn build-context
  "Build the Diagnostician input context map from the candidate's
  evidence events (component).

  The context is the compact EDN summary a Diagnostician consumes
  alongside the frozen evidence pack. When `events` is a non-empty
  sequential collection of evidence events (the F1 contract:
  {:event/seq int? :event/type keyword? :metadata map?}), the context
  carries

      :context/behavior-profile
      {:behavior/session-id ... :behavior/n-events ...
       :behavior/intents {...} :behavior/failures [...]
       :behavior/tool-seq [...] :behavior/status ...
       :behavior/wall-ms ... :behavior/resource {...}
       :behavior/fingerprint \"sha256:<64 hex>\"}

  — the closed BehaviorProfile computed via
  evoclj.analytics.behavior/profile-events with the profile's stable
  sha256 fingerprint attached as :behavior/fingerprint. Without
  evidence events (nil or an empty collection) the key is absent and
  the context is {}.

  Deterministic (Global Constraint 6): profile-events is a pure fold
  over the events and fingerprint is a canonical content hash, so the
  same events always yield the same profile and fingerprint. Any
  malformed element inside `events` is rejected by the F1 error
  contract (:analytics/events-invalid)."
  [events]
  (if (and (sequential? events) (seq events))
    (let [profile (behavior/profile-events events)]
      {:context/behavior-profile
       (assoc profile :behavior/fingerprint (behavior/fingerprint profile))})
    {}))

;; --- episode classification (same semantics as component) ---------------------

(defn- success?
  "A success episode: :outcome :status :completed."
  [episode]
  (= :completed (get-in episode [:outcome :status])))

(defn- failure?
  "A failure episode: any outcome other than :completed."
  [episode]
  (not (success? episode)))

(defn- episode-cost
  "The numeric cost of an episode from its :usage map: :total-cost,
  falling back to :cost, else 0.0."
  [episode]
  (let [u (:usage episode)]
    (or (some-> (:total-cost u) double)
        (some-> (:cost u) double)
        0.0)))

(defn- high-cost?
  "A high-cost episode: a strictly positive usage cost."
  [episode]
  (pos? (episode-cost episode)))

(defn- terminal-event
  "The support citation of an episode: its :trace :last-event — the
  terminal event of the causal trace where the episode's outcome was
  recorded. The deterministic adapter consumes ONLY pack metadata
  (Global Constraint 11 — it holds no store handle to dereference the
  CAS excerpt), so it cites the pack's own trace bound."
  [episode]
  (get-in episode [:trace :last-event]))

(defn- support-ref
  "One support entry for a supporting episode: the episode ref plus the
  event ids that substantiate the pattern (here: the episode's
  terminal trace event)."
  [episode]
  {:episode/id (:episode/id episode)
   :event-ids [(terminal-event episode)]})

(defn- counterevidence-ref
  "One counterevidence entry: the episode ref (the interface shows only
  the episode)."
  [episode]
  (select-keys episode [:episode/id]))

(defn- confidence-share
  "The numeric :confidence of a pattern hypothesis (roadmap E2): the
  share of the pack's episodes that substantiate the pattern — a plain
  Clojure number (ratio or integer) within [0,1] that grows with the
  evidence strength, so the same (config, pack) always yields the same
  confidence (Global Constraint 6)."
  [supporting total]
  (/ supporting (max 1 total)))

;; --- deterministic pattern rules ---------------------------------------------

(defn- task-success-hypothesis
  "The :task/success pattern: the pack's task success rate
  (successes / selected, from the pack's :summary) is BELOW
  :task/success-threshold. Supported by every failure episode (each
  citing its terminal trace event), countered by every success
  episode. Returns nil when nothing was selected or the rate is at or
  above the threshold."
  [pack threshold confidence-band]
  (let [summary (:summary pack)
        selected (or (:selected summary) 0)
        successes (or (:successes summary) 0)]
    (when (and (pos? selected)
               (< (/ (double successes) selected) threshold))
      (let [failures (filterv failure? (:episodes pack))
            successes-ep (filterv success? (:episodes pack))]
        {:pattern :task/success
         :claim (String/format (Locale/ROOT)
                               "task success rate %.1f%% (%d/%d) is below the %.2f threshold"
                               (to-array [(* 100.0 (/ (double successes) selected))
                                          successes selected (double threshold)]))
         :support (mapv support-ref failures)
         :counterevidence (mapv counterevidence-ref successes-ep)
         :target {:kind :workflow :id :task}
         :expected-effect {:metric :task/success :direction :increase}
         :confidence (confidence-share (count failures)
                                       (count (:episodes pack)))
         :confidence-band confidence-band}))))

(defn- task-high-cost-hypothesis
  "The :task/high-cost pattern: at least one episode carries a positive
  usage cost. Supported by each high-cost episode, countered by the
  zero-cost episodes. Returns nil when no episode is high-cost."
  [pack confidence-band]
  (let [episodes (:episodes pack)
        costly (filterv high-cost? episodes)]
    (when (seq costly)
      {:pattern :task/high-cost
       :claim (String/format (Locale/ROOT)
                             "task cost is high: %d episode(s) carry positive usage cost"
                             (to-array [(count costly)]))
       :support (mapv support-ref costly)
       :counterevidence (mapv counterevidence-ref
                              (filterv (complement high-cost?) episodes))
       :target {:kind :workflow :id :task}
       :expected-effect {:metric :task/cost :direction :decrease}
       :confidence (confidence-share (count costly) (count episodes))
       :confidence-band confidence-band})))

(defn- finalize-hypothesis
  "Compute the deterministic name-based :hypothesis/id over the
  hypothesis content (everything except the id), so the id is a pure
  function of what the hypothesis claims and cites."
  [hypothesis]
  (assoc hypothesis :hypothesis/id (deterministic-uuid hypothesis)))

(defn- pattern-hypotheses
  "The deterministic pattern hypotheses for the pack, in catalog order
  (:task/success, then :task/high-cost), bounded by
  :max-hypotheses. Each hypothesis carries its deterministic id and a
  numeric :confidence in [0,1] (roadmap E2)."
  [pack config]
  (let [threshold (:task/success-threshold config)
        confidence-band (:confidence-band config)
        max-hypotheses (:max-hypotheses config)
        candidates (keep identity
                          [(task-success-hypothesis pack threshold confidence-band)
                           (task-high-cost-hypothesis pack confidence-band)])]
    (mapv finalize-hypothesis (take max-hypotheses candidates))))

;; --- kernel ranking gate (roadmap E2) ---------------------------------------

(defn- validate-confidence!
  "Re-validate ONE hypothesis' :confidence at the kernel adoption gate
  (roadmap E2): it MUST be present, numeric, and within the closed
  interval [0,1]. Malformed confidence — missing, non-numeric, or
  outside [0,1] (NaN and infinities included) — is rejected with the
  typed :evolution/hypothesis-confidence-invalid error carrying the
  offending value in its data (Global Constraint 22: plain
  serializable error data). Returns the hypothesis unchanged."
  [hypothesis]
  (let [c (:confidence hypothesis)]
    (when-not (and (number? c)
                   (<= 0.0 (double c) 1.0))
      (throw (err/error :evolution/hypothesis-confidence-invalid
                        "hypothesis confidence must be a number within [0,1]"
                        {:hypothesis/id (:hypothesis/id hypothesis)
                         :confidence c})))
    hypothesis))

(defn rank-hypotheses
  "The kernel re-validation step applied to hypotheses BEFORE adoption
  (roadmap E2): validate every hypothesis' :confidence (malformed →
  typed :evolution/hypothesis-confidence-invalid), then return the
  hypotheses in DESCENDING :confidence order with ties broken
  deterministically by :hypothesis/id (the content-addressed name in
  its canonical string form). The result is a pure function of the
  hypotheses: input order never matters and repeated runs are
  byte-identical (Global Constraint 6)."
  [hypotheses]
  (let [validated (mapv validate-confidence! hypotheses)]
    (vec (sort-by (fn [h] [(- (double (:confidence h)))
                            (str (:hypothesis/id h))])
                  validated))))

;; --- the deterministic adapter ------------------------------------------------

(defrecord PatternDiagnostician [config]
  Diagnostician
  (diagnose [_ evidence-pack]
    (es/validate-pack evidence-pack)
    (let [hypotheses (->> (pattern-hypotheses evidence-pack config)
                          (mapv ds/validate-hypothesis)
                          rank-hypotheses)
          data {:evidence/id (:evidence/id evidence-pack)
                :hypotheses hypotheses}
          id (digest data)
          diagnosis (assoc data :diagnosis/id id)]
      (ds/validate-diagnosis diagnosis)
      diagnosis)))

(defn pattern-diagnostician
  "Construct the deterministic pattern adapter from a plain config map
  (component Step 3).

  The constructor receives ONLY pattern configuration — plain data:
    :task/success-threshold (number, default 1.0 — the success rate
                             below which :task/success fires)
    :max-hypotheses         (pos-int, default 3)
    :confidence-band        (:low | :medium | :high, default :medium)
  It must NEVER receive a store handle or a Selection/Audit fixture
  handle (Global Constraint 11). This is enforced by design: the
  config schema is closed, so any unknown key — a loader, a store map,
  a fixture handle — is rejected with :diagnosis/config-invalid, and
  the record holds exactly one field (:config)."
  [config]
  (let [defaults {:task/success-threshold 1.0
                  :max-hypotheses 3
                  :confidence-band :medium}]
    (->PatternDiagnostician (merge defaults (ds/validate-config config)))))

;; --- persistence (Step 5) ------------------------------------------------------

(defn- validate-store!
  "Validate the store trust boundary: the executor :stores map
  {:sqlite <db> :cas <CAS root>}."
  [store]
  (when-not (map? store)
    (throw (err/error :diagnosis/store-invalid
                      "store must be the executor :stores map {:sqlite ... :cas ...}"
                      {:reason :not-a-map :value (err/sanitize store)})))
  (when-not (contains? store :sqlite)
    (throw (err/error :diagnosis/store-invalid
                      "store must carry the :sqlite handle"
                      {:reason :sqlite-missing})))
  (when-not (contains? store :cas)
    (throw (err/error :diagnosis/store-invalid
                      "store must carry the :cas handle"
                      {:reason :cas-missing})))
  store)

(defn persist-diagnosis!
  "Persist a validated diagnosis with provenance (component Step 5).

  The diagnosis body (everything except :diagnosis/id) is
  canonicalized and stored in the filesystem CAS under its own content
  hash; :diagnosis/id must BE that content address, so a forged id is
  rejected with :diagnosis/id-mismatch before anything is written. The
  artifacts registry row is written when the tables allow (it does —
  001-init.sql creates `artifacts` with hash as PRIMARY KEY; the
  INSERT OR IGNORE keeps the write idempotent). The body embeds
  :evidence/id, so the artifact is self-provenancing back to the
  frozen evidence pack.

  Global Constraints 10/12/19: this is the ONLY store interaction in
  the diagnosis path and it touches only the CAS artifact + artifacts
  registry — never genomes, generations, kernel source, authority,
  audit, evaluator-isolation, or promotion roots. No free-form
  diagnosis can directly alter the Genome: there is no such API.

  Returns the diagnosis unchanged."
  [store diagnosis]
  (validate-store! store)
  (ds/validate-diagnosis diagnosis)
  (let [body (dissoc diagnosis :diagnosis/id)
        id (digest body)]
    (when-not (= id (:diagnosis/id diagnosis))
      (throw (err/error :diagnosis/id-mismatch
                        "diagnosis id must be the content hash of the diagnosis body"
                        {:diagnosis/id (:diagnosis/id diagnosis)
                         :expected id})))
    (let [ba (utf8-bytes (pr-str (canonical body)))
          {:keys [size]} (cas/put-bytes! (:cas store) ba
                                         {:media-type diagnosis-media-type})]
      (sqlite/exec! (:sqlite store)
                    ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
                      VALUES (?, ?, ?, ?)"
                     id diagnosis-media-type size (str (java.time.Instant/now))])
      diagnosis)))
