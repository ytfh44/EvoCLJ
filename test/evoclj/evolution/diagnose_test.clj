(ns evoclj.evolution.diagnose-test
  "Task 7.2 tests for the Diagnostician contract and structured
  hypotheses.

  The normative shape (the plan's Task 7.2 interface):

      {:diagnosis/id \"sha256:...\"
       :evidence/id \"sha256:...\"
       :hypotheses
       [{:hypothesis/id #uuid
         :pattern :premature-tool-mutation
         :claim \"...\"
         :support [{:episode/id ... :event-ids [...]}]
         :counterevidence [{:episode/id ...}]
         :target {:kind :skill :id :debugging}
         :expected-effect {:metric :task/success :direction :increase}
         :confidence-band :medium}]}

  The four normative scenarios, in the task's numbered order:

  - Step 1: the schema REQUIRES :support, :target, and
    :expected-effect; closed maps reject unknown keys.
  - Step 2: unsupported hypotheses with ZERO evidence references are
    rejected — an empty :support vector, or a support entry whose
    :event-ids is empty.
  - Step 3: the Diagnostician protocol exists; the deterministic
    pattern adapter (pattern-diagnostician) scans the pack's episode
    summaries and emits BOUNDED hypotheses. The same (config, pack)
    yields byte-for-byte the same diagnosis, including deterministic
    hypothesis ids.
  - Step 4: the adapter receives Evolution-set evidence ONLY — the
    constructor takes a single plain config map with no store /
    Selection / Audit fixture handle (assert by design: the closed
    config schema rejects a smuggled loader, and the record holds
    exactly one field, :config).
  - Step 5: persist-diagnosis! writes the diagnosis body to the CAS
    under its own content hash (a forged :diagnosis/id is rejected)
    plus the artifacts registry row; the body embeds :evidence/id, so
    the artifact is self-provenancing. Persistence touches ONLY the
    CAS + artifacts registry — never generations (no free-form
    diagnosis may directly alter the Genome; the API does not exist).

  FIXTURE DESIGN: most pattern tests hand-build schema-valid frozen
  evidence packs (compact episode refs + summary), so the adapter is
  exercised as a pure function of its input; one integration test
  builds a REAL pack through the Task 7.1 pipeline
  (evoclj.evolution.evidence/build-evidence-pack) to prove the adapter
  and the persistence path work on genuine frozen packs. The support
  citation convention of the deterministic adapter: each supporting
  episode cites its :trace :last-event — the terminal event where the
  episode's outcome was recorded (the adapter holds no store handle to
  dereference the CAS excerpt, Global Constraint 11)."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.evolution.diagnose :as diag]
            [evoclj.evolution.diagnosis-schema :as ds]
            [evoclj.evolution.evidence :as evidence]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- shared fixture identity ------------------------------------------------

(def ^:private hex64
  "64 hex chars for the canonical content-addressed ids."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private genome-id (str "sha256:" hex64))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype-id (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private generation-id "generation-1")
(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private placeholder-hash (str "sha256:" (apply str (repeat 64 "0"))))

(defn- uuid
  "A fixed, readable UUID for fixture ids."
  [n]
  (java.util.UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

;; --- temp stores ------------------------------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-diagnose-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-diagnose-cas-"
                                     (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- delete-tree!
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup!
  []
  (doseq [p @temp-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-store
  "A migrated sqlite db (path spec) plus a temp CAS root, seeded with
  the generation row sessions pin to. Returns the executor-style store
  map {:sqlite <spec> :cas <root>}."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 0
                     :created_at now}))
    {:sqlite db :cas (cas/->cas (temp-cas-dir))}))

;; --- real-pack fixtures (the Task 7.1 pipeline) -----------------------------

(defn- scene!
  "Create one complete episode under the fixture generation: session →
  events → episode row. Returns {:episode/id :session/id :trace
  :outcome :usage}."
  [db {:keys [outcome usage]}]
  (let [sid (random-uuid)]
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :sessions
                    {:id (str sid)
                     :generation_id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :phenotype_id phenotype-id
                     :state "running"
                     :created_at now}))
    (doseq [i (range 3)]
      (sqlite/exec! db
                    ["INSERT INTO events
                        (session_id, event_seq, generation_id, phenotype_id,
                         event_type, cause_event_id, payload_ref, payload,
                         prev_hash, event_hash, created_at)
                        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                     (str sid) (inc i) generation-id phenotype-id
                     (name :node/completed) nil nil
                     (pr-str {:marker "trace-payload"})
                     nil placeholder-hash now]))
    (let [row (first (sqlite/query db
                                   ["SELECT MIN(id) AS first_id, MAX(id) AS last_id
                                     FROM events WHERE session_id = ?"
                                    (str sid)]))
          eid (random-uuid)]
      (sqlite/exec! db
                    ["INSERT INTO episodes
                        (id, session_id, generation_id, genome_id, resolution_id,
                         task_ref, first_event_id, last_event_id, outcome, usage,
                         created_at)
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                     (str eid) (str sid) generation-id genome-id resolution-id
                     placeholder-hash (:first_id row) (:last_id row)
                     (pr-str outcome) (pr-str (or usage {})) now])
      {:episode/id eid
       :session/id sid
       :trace {:first-event (:first_id row) :last-event (:last_id row)}
       :outcome outcome
       :usage (or usage {})})))

(defn- build-real-pack
  "Build a REAL frozen evidence pack through the Task 7.1 pipeline."
  [store]
  (evidence/build-evidence-pack
   store
   {:generation/id generation-id
    :cutoff-event-id 10000
    :selector {:recent 10 :include-successes 5
               :include-failures 5 :include-high-cost 5}}))

;; --- hand-built pack fixtures ------------------------------------------------

(defn- ep
  "One compact episode ref for a hand-built evidence pack (schema-valid
  per the Task 7.1 EpisodeRefSchema)."
  [{:keys [id first last outcome usage]}]
  {:episode/id id
   :session/id (random-uuid)
   :generation/id generation-id
   :excerpt-ref placeholder-hash
   :outcome outcome
   :trace {:first-event first :last-event last}
   :usage (or usage {})})

(defn- pack
  "A hand-built, schema-valid frozen evidence pack with the given
  episode refs and summary overrides."
  [episodes & [summary]]
  {:evidence/id placeholder-hash
   :generation/id generation-id
   :cutoff-event-id 1000
   :episodes episodes
   :summary (merge {:selector {:recent 10 :include-successes 5
                               :include-failures 5 :include-high-cost 5}
                    :eligible (count episodes)
                    :selected (count episodes)
                    :successes 0 :failures 0 :high-cost 0}
                   summary)})

;; --- shape helpers -----------------------------------------------------------

(defn- hypothesis
  "A schema-valid hypothesis; keyword overrides win."
  [& [overrides]]
  (merge {:hypothesis/id (uuid 10)
          :pattern :task/success
          :claim "task success rate is below the threshold"
          :support [{:episode/id (uuid 1) :event-ids [12]}]
          :counterevidence []
          :target {:kind :workflow :id :task}
          :expected-effect {:metric :task/success :direction :increase}
          :confidence-band :medium}
         overrides))

(defn- diagnosis-with
  "A schema-valid diagnosis (the given hypotheses, defaulting to one
  valid hypothesis)."
  [& [hypotheses overrides]]
  (merge {:diagnosis/id placeholder-hash
          :evidence/id placeholder-hash
          :hypotheses (or hypotheses [(hypothesis)])}
         overrides))

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by `f`, or nil
  when nothing is thrown."
  [f]
  (:error/type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e)))))

;; ============================================================================
;; Step 1 — the schema requires support, target, and expected effect
;; ============================================================================

(deftest step-1-schema-requires-support-target-and-expected-effect
  (testing "a complete hypothesis validates"
    (is (map? (ds/validate-hypothesis (hypothesis)))))
  (testing "support is REQUIRED"
    (is (= :diagnosis/hypothesis-invalid
           (thrown-error-type #(ds/validate-hypothesis
                                (dissoc (hypothesis) :support))))))
  (testing "target is REQUIRED"
    (is (= :diagnosis/hypothesis-invalid
           (thrown-error-type #(ds/validate-hypothesis
                                (dissoc (hypothesis) :target))))))
  (testing "expected-effect is REQUIRED"
    (is (= :diagnosis/hypothesis-invalid
           (thrown-error-type #(ds/validate-hypothesis
                                (dissoc (hypothesis) :expected-effect))))))
  (testing "a missing required key inside an embedded hypothesis
            invalidates the whole diagnosis"
    (is (= :diagnosis/invalid
           (thrown-error-type #(ds/validate-diagnosis
                                (assoc (diagnosis-with)
                                       :hypotheses [(dissoc (hypothesis) :target)]))))))
  (testing "unknown keys are rejected (closed maps at the trust boundary)"
    (is (= :diagnosis/hypothesis-invalid
           (thrown-error-type #(ds/validate-hypothesis (assoc (hypothesis) :bogus 1)))))
    (is (= :diagnosis/invalid
           (thrown-error-type #(ds/validate-diagnosis (assoc (diagnosis-with) :bogus 1))))))
  (testing "a diagnosis carries evidence provenance and a content-addressed id"
    (let [d (diagnosis-with)]
      (is (= placeholder-hash (:evidence/id d)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:diagnosis/id d))))))

;; ============================================================================
;; Step 2 — unsupported hypotheses with ZERO evidence references are rejected
;; ============================================================================

(deftest step-2-unsupported-hypotheses-are-rejected
  (testing "a hypothesis with an EMPTY :support vector is rejected"
    (is (= :diagnosis/hypothesis-invalid
           (thrown-error-type #(ds/validate-hypothesis
                                (assoc (hypothesis) :support []))))))
  (testing "a support entry that cites an episode but ZERO event ids is rejected"
    (is (= :diagnosis/hypothesis-invalid
           (thrown-error-type #(ds/validate-hypothesis
                                (assoc (hypothesis)
                                       :support [{:episode/id (uuid 1)
                                                  :event-ids []}]))))))
  (testing "the same rejections hold inside a full diagnosis"
    (is (= :diagnosis/invalid
           (thrown-error-type #(ds/validate-diagnosis
                                (assoc (diagnosis-with)
                                       :hypotheses [(assoc (hypothesis)
                                                          :support [])])))))
    (is (= :diagnosis/invalid
           (thrown-error-type #(ds/validate-diagnosis
                                (assoc (diagnosis-with)
                                       :hypotheses
                                       [(assoc (hypothesis)
                                               :support [{:episode/id (uuid 1)
                                                          :event-ids []}])])))))))

;; ============================================================================
;; Step 3 — the Diagnostician protocol and the deterministic pattern adapter
;; ============================================================================

(deftest step-3-diagnostician-protocol-and-deterministic-pattern-adapter
  (let [d (diag/pattern-diagnostician {:task/success-threshold 0.6})
        d2 (diag/pattern-diagnostician {:task/success-threshold 0.6})
        episodes [(ep {:id (uuid 1) :first 10 :last 12
                       :outcome {:status :failed :score nil}})
                  (ep {:id (uuid 2) :first 20 :last 22
                       :outcome {:status :failed :score nil}})
                  (ep {:id (uuid 3) :first 30 :last 33
                       :outcome {:status :completed :score nil}})]
        p (pack episodes {:successes 1 :failures 2 :selected 3})
        diagnosis (diag/diagnose d p)]
    (testing "the adapter satisfies the Diagnostician protocol"
      (is (satisfies? diag/Diagnostician d)))
    (testing "diagnose returns a validated diagnosis for the evidence pack"
      (is (= (:evidence/id p) (:evidence/id diagnosis)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:diagnosis/id diagnosis)))
      (is (vector? (:hypotheses diagnosis)))
      (is (every? #(map? %) (:hypotheses diagnosis)))
      (is (map? (ds/validate-diagnosis diagnosis))))
    (testing "the :task/success pattern fires below the threshold"
      (let [h (first (:hypotheses diagnosis))]
        (is (= :task/success (:pattern h)))
        (is (re-matches #"task success rate .* is below the .* threshold"
                       (:claim h)))
        ;; support: every failure episode, each citing its terminal
        ;; trace event (:trace :last-event)
        (is (= [{:episode/id (uuid 1) :event-ids [12]}
                {:episode/id (uuid 2) :event-ids [22]}]
               (:support h)))
        ;; counterevidence: the success episodes
        (is (= [{:episode/id (uuid 3)}] (:counterevidence h)))
        (is (= {:kind :workflow :id :task} (:target h)))
        (is (= {:metric :task/success :direction :increase}
               (:expected-effect h)))
        (is (= :medium (:confidence-band h)))))
    (testing "the diagnosis is deterministic: same config + same pack,
              byte-for-byte, including the hypothesis ids"
      (is (= diagnosis (diag/diagnose d2 p)))
      (is (= (:diagnosis/id diagnosis) (:diagnosis/id (diag/diagnose d2 p))))
      (is (= (:hypotheses diagnosis) (:hypotheses (diag/diagnose d2 p)))))))

(deftest step-3-patterns-fire-only-when-supported
  (testing "an all-success pack never fires :task/success"
    (let [d (diag/pattern-diagnostician {:task/success-threshold 1.0})
          p (pack [(ep {:id (uuid 1) :first 10 :last 12
                         :outcome {:status :completed :score nil}})
                   (ep {:id (uuid 2) :first 20 :last 22
                        :outcome {:status :completed :score nil}})]
                  {:successes 2 :failures 0 :selected 2})
          diagnosis (diag/diagnose d p)]
      (is (empty? (:hypotheses diagnosis)))
      (is (map? (ds/validate-diagnosis diagnosis)))))
  (testing "a rate AT the threshold does not fire (strictly below)"
    (let [d (diag/pattern-diagnostician {:task/success-threshold 0.5})
          p (pack [(ep {:id (uuid 1) :first 10 :last 12
                         :outcome {:status :failed :score nil}})
                   (ep {:id (uuid 2) :first 20 :last 22
                        :outcome {:status :completed :score nil}})]
                  {:successes 1 :failures 1 :selected 2})
          diagnosis (diag/diagnose d p)]
      (is (empty? (:hypotheses diagnosis)))))
  (testing "high-cost episodes trigger the :task/high-cost pattern"
    (let [d (diag/pattern-diagnostician {})
          p (pack [(ep {:id (uuid 1) :first 10 :last 12
                         :outcome {:status :completed :score nil}
                         :usage {:total-cost 55}})
                   (ep {:id (uuid 2) :first 20 :last 22
                        :outcome {:status :completed :score nil}})]
                  {:successes 2 :failures 0 :selected 2 :high-cost 1})
          h (first (:hypotheses (diag/diagnose d p)))]
      (is (= :task/high-cost (:pattern h)))
      (is (= [{:episode/id (uuid 1) :event-ids [12]}] (:support h)))
      (is (= [{:episode/id (uuid 2)}] (:counterevidence h)))
      (is (= {:metric :task/cost :direction :decrease} (:expected-effect h)))))
  (testing "an empty pack yields a valid diagnosis with no hypotheses"
    (let [d (diag/pattern-diagnostician {})
          p (pack [] {:selected 0 :successes 0 :failures 0})
          diagnosis (diag/diagnose d p)]
      (is (empty? (:hypotheses diagnosis)))
      (is (map? (ds/validate-diagnosis diagnosis))))))

(deftest step-3-hypotheses-are-bounded-by-config
  (let [d (diag/pattern-diagnostician {:task/success-threshold 1.0
                                       :max-hypotheses 1})
        p (pack [(ep {:id (uuid 1) :first 10 :last 12
                      :outcome {:status :failed :score nil}})
                 (ep {:id (uuid 2) :first 20 :last 22
                      :outcome {:status :completed :score nil}
                      :usage {:total-cost 55}})]
                {:successes 1 :failures 1 :selected 2 :high-cost 1})
        diagnosis (diag/diagnose d p)]
    (testing "both patterns trigger but :max-hypotheses bounds the emission"
      (is (<= (count (:hypotheses diagnosis)) 1))
      (is (= :task/success (:pattern (first (:hypotheses diagnosis))))))))

;; ============================================================================
;; Step 4 — the adapter receives Evolution-set evidence only
;; ============================================================================

(deftest step-4-adapter-receives-evolution-set-evidence-only
  (testing "the constructor takes ONLY a plain pattern-config map — no
            store, Selection, or Audit handle"
    (let [d (diag/pattern-diagnostician {:task/success-threshold 0.6
                                         :max-hypotheses 5})]
      (is (satisfies? diag/Diagnostician d))
      ;; assert by design: the record's ONLY field is the config map —
      ;; no selection/audit loader was captured anywhere
      (is (= #{:config} (set (keys (into {} d)))))
      (is (= {:task/success-threshold 0.6 :max-hypotheses 5
              :confidence-band :medium}
             (:config d)))))
  (testing "defaults apply when the config is empty"
    (is (= {:task/success-threshold 1.0 :max-hypotheses 3
            :confidence-band :medium}
           (:config (diag/pattern-diagnostician {})))))
  (testing "a Selection/Audit fixture handle smuggled into the
            constructor is rejected by the closed config schema"
    (is (= :diagnosis/config-invalid
           (thrown-error-type #(diag/pattern-diagnostician
                                {:selection-loader identity}))))
    (is (= :diagnosis/config-invalid
           (thrown-error-type #(diag/pattern-diagnostician
                                {:audit-handle {:db "/tmp/audit.db"}}))))
    (is (= :diagnosis/config-invalid
           (thrown-error-type #(diag/pattern-diagnostician
                                {:store {:cas "/tmp" :sqlite "/tmp/x.db"}}))))
    (is (= :diagnosis/config-invalid
           (thrown-error-type #(diag/pattern-diagnostician :not-a-map)))))
  (testing "diagnose consumes ONLY the evidence pack — no store is consulted"
    (let [d (diag/pattern-diagnostician {})
          p (pack [] {:selected 0})
          diagnosis (diag/diagnose d p)]
      (is (map? diagnosis))
      (is (map? (ds/validate-diagnosis diagnosis))))))

;; ============================================================================
;; Step 5 — diagnosis artifacts are persisted with provenance
;; ============================================================================

(deftest step-5-persists-a-real-frozen-pack-diagnosis
  (let [store (fresh-store)
        db (:sqlite store)
        f1 (scene! db {:outcome {:status :failed :score nil}})
        f2 (scene! db {:outcome {:status :failed :score nil}})
        c1 (scene! db {:outcome {:status :completed :score nil}
                       :usage {:total-cost 55}})
        p (build-real-pack store)
        diagnosis (diag/diagnose (diag/pattern-diagnostician
                                  {:task/success-threshold 1.0})
                                 p)]
    (testing "the deterministic adapter diagnoses a REAL frozen pack"
      (is (= (:evidence/id p) (:evidence/id diagnosis)))
      (is (= :task/success (:pattern (first (:hypotheses diagnosis)))))
      (is (= :task/high-cost (:pattern (second (:hypotheses diagnosis)))))
      (is (map? (ds/validate-diagnosis diagnosis))))
    (testing "support cites the real episodes by id with their terminal events"
      ;; pack order is recency order (:last-event desc): c1, f2, f1
      (is (= [{:episode/id (:episode/id f2)
               :event-ids [(get-in f2 [:trace :last-event])]}
              {:episode/id (:episode/id f1)
               :event-ids [(get-in f1 [:trace :last-event])]}]
             (:support (first (:hypotheses diagnosis)))))
      (is (= [{:episode/id (:episode/id c1)}]
             (:counterevidence (first (:hypotheses diagnosis)))))
      (is (= [{:episode/id (:episode/id c1)
               :event-ids [(get-in c1 [:trace :last-event])]}]
             (:support (second (:hypotheses diagnosis))))))
    (testing "persist writes the CAS artifact under the content hash"
      (let [persisted (diag/persist-diagnosis! store diagnosis)]
        (is (= diagnosis persisted))
        (is (cas/exists? (:cas store) (:diagnosis/id diagnosis)))
        (is (= (dissoc diagnosis :diagnosis/id)
               (edn/read-string
                (String. (cas/get-bytes (:cas store) (:diagnosis/id diagnosis))
                         StandardCharsets/UTF_8))))
        (let [meta (cas/get-meta (:cas store) (:diagnosis/id diagnosis))]
          (is (= (:diagnosis/id diagnosis) (:artifact/id meta)))
          (is (= "application/edn" (:media-type meta))))))
    (testing "a store row records the artifact in the artifacts registry"
      (let [row (first (sqlite/query db
                                     ["SELECT hash, media_type, size FROM artifacts
                                       WHERE hash = ?" (:diagnosis/id diagnosis)]))]
        (is (= (:diagnosis/id diagnosis) (:hash row)))
        (is (= "application/edn" (:media_type row)))
        (is (pos? (:size row)))))
    (testing "persisting twice is idempotent: one artifact, one row"
      (diag/persist-diagnosis! store diagnosis)
      (is (= 1 (count (sqlite/query db
                                    ["SELECT hash FROM artifacts
                                      WHERE hash = ?" (:diagnosis/id diagnosis)])))))
    (testing "no free-form diagnosis may directly alter the Genome —
              persist touches only CAS + artifacts, never generations"
      (is (= 1 (count (sqlite/query db ["SELECT id FROM generations"])))))))

;; ============================================================================
;; Error contract
;; ============================================================================

(deftest error-contract
  (testing "an invalid evidence pack is rejected at the diagnose boundary"
    (is (= :evidence/pack-invalid
           (thrown-error-type #(diag/diagnose (diag/pattern-diagnostician {})
                                              {:bogus 1}))))
    (is (= :evidence/pack-invalid
           (thrown-error-type #(diag/diagnose (diag/pattern-diagnostician {})
                                              nil)))))
  (testing "a forged :diagnosis/id that is not the content address is rejected"
    (let [store (fresh-store)
          p (pack [] {:selected 0})
          diagnosis (diag/diagnose (diag/pattern-diagnostician {}) p)]
      (is (= :diagnosis/id-mismatch
             (thrown-error-type #(diag/persist-diagnosis!
                                  store (assoc diagnosis
                                               :diagnosis/id placeholder-hash)))))))
  (testing "a malformed store is rejected by persist"
    (let [p (pack [] {:selected 0})
          diagnosis (diag/diagnose (diag/pattern-diagnostician {}) p)]
      (is (= :diagnosis/store-invalid
             (thrown-error-type #(diag/persist-diagnosis! {} diagnosis))))
      (is (= :diagnosis/store-invalid
             (thrown-error-type #(diag/persist-diagnosis! nil diagnosis)))))))
