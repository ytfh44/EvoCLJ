(ns evoclj.evolution.llm-mutator-test
  "Tests for the LLM-driven Mutator adapter (feature 2 of 3).

  The adapter sits at the Mutator contract of evoclj.evolution.core,
  but sources mutation proposals from a real language model via the
  host-injected :model-call closure. These tests use fake :model-call
  closures that return canned provider :values, so the adapter's JSON
  parsing, per-mutation noise tolerance, op keyword coercion, THE
  KERNEL-COMPUTED :expect/hash completion (the security property: a
  model can never name a preimage it does not know), and fail-loud
  error contract are exercised without any provider.

  The fixture parent Genome is a hand-built, minimal loaded-genome map
  (the shape evoclj.genome.load/load-genome returns): :genome/id,
  :manifest {:evolution {:mutable #{...}}}, and :files keyed by
  canonical relative path with {:digest :bytes :kind}. The canonical
  digest used everywhere is \"sha256:<64 zero digits>\".

  Error contract under test: :mutation/config-invalid,
  :mutation/context-invalid, :mutation/llm-failed,
  :mutation/llm-response-invalid.

  JSON responses are built with cheshire generate-string so the test
  text exactly matches what a real model (post keywordize) would emit —
  op names, risk, metrics, and payload keys arrive as STRINGS and the
  adapter must coerce them into the keyword schema vocabulary."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.capability.evolution-tools :as evo-tools]
            [evoclj.evolution.core :as core]
            [evoclj.evolution.llm-mutator :as llm]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.evolution.mutation-schema :as ms]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.registry :as registry]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

;; --- shared fixture identity --------------------------------------------------

(def ^:private placeholder-hash
  (str "sha256:" (apply str (repeat 64 "0"))))

(def ^:private skills-file "skills/debugging.edn")

(defn- uuid-of
  "A fixed, readable UUID for fixture ids."
  [n]
  (UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(defn- fixture-parent
  "A minimal hand-built loaded-genome map: one mutable EDN skill file
  with digest placeholder-hash and a manifest declaring :skills as the
  only mutable asset class."
  []
  {:genome/id placeholder-hash
   :manifest {:genome/format 1
              :agent/id :fixture/agent
              :evolution {:max-risk :behavioral :mutable #{:skills}}
              :modules {:evolution "evolution-policy.edn"}}
   :files {skills-file
           {:digest placeholder-hash
            :bytes (vec (.getBytes "{:steps []}\n" StandardCharsets/UTF_8))
            :kind :edn}}})

(defn- fixture-context
  "The full, closed Mutator context the orchestrator hands the adapter
  (see core/propose-candidates!): a schema-shaped Diagnosis, an empty
  history, and the v0 budget profile."
  []
  {:generation/id "generation-1"
   :parent/genome-id placeholder-hash
   :parent-genome (fixture-parent)
   :diagnosis {:diagnosis/id placeholder-hash
               :evidence/id placeholder-hash
               :hypotheses [{:hypothesis/id (uuid-of 7)
                             :pattern :task/success
                             :claim "task success rate is below threshold"
                             :support [{:episode/id (uuid-of 9) :event-ids [1]}]
                             :counterevidence []
                             :target {:kind :workflow :id :task}
                             :expected-effect {:metric :task/success
                                               :direction :increase}
                             :confidence-band :medium}]}
   :history []
   :budget-profile {:parameter {:max-ops 8}}})

;; --- canned :model-call closures ----------------------------------------------

(defn- value-with
  "A provider :value wrapping the given text."
  [text]
  {:value {:model/output {:text text}
           :usage {:prompt-tokens 10 :completion-tokens 10}}})

(defn- canned-call
  "A fake :model-call closure returning the given :value and recording
  the (model-id messages options) it was invoked with (for asserting the
  adapter passes its config through)."
  [value]
  (let [calls (atom [])]
    [(fn [id messages options]
       (swap! calls conj {:id id :messages messages :options options})
       value)
     calls]))

(defn- mutation-text
  "Render a vector of raw mutation maps as the model's JSON text."
  [mutations]
  (json/generate-string {:mutations mutations}))

(defn- valid-mutation
  "A raw (pre-normalization) mutation map in the flat-key vocabulary the
  system prompt asks for: one :set-edn op on the fixture's skill file,
  no :expect/hash (the kernel computes it). Keyword overrides win."
  [& [overrides]]
  (merge {:ops [{"op" "set-edn"
                 "file" skills-file
                 "path" ["bar"]
                 "value" [:a :b]}]
          :risk "behavioral"}
         overrides))

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by f, or nil."
  [f]
  (:error/type (ex-data (try (f) nil
                             (catch clojure.lang.ExceptionInfo e e)))))

(defn- propose
  "Call the protocol method on an adapter built from cfg against the
  fixture context."
  [cfg & [context]]
  (core/propose-mutations (llm/llm-mutator cfg)
                          (or context (fixture-context))))

(defn- completed-shape
  "The Mutation IR the orchestrator's complete-mutation! would build
  from an adapter-returned mutation (lineage fields added), so a full
  envelope validation is possible in the test. A default
  :expected-effect is supplied when the adapter-returned mutation
  omits one (the adapter treats it as optional; the envelope schema
  requires it) — an adapter mutation that the model gave no effect
  stays valid for the envelope gate."
  [mutation]
  (merge {:mutation/id (UUID/randomUUID)
          :parent/genome-id placeholder-hash
          :evidence/id placeholder-hash
          :hypothesis/id (uuid-of 7)
          :expected-effect {:primary-metric :task/success
                            :direction :increase}}
         mutation))


;; --- happy path ------------------------------------------------------------

(deftest happy-path
  (testing "a canned JSON model response yields one hash-completed,
            schema-valid mutation with lineage fields left for the
            orchestrator"
    (let [[mc calls] (canned-call (value-with (mutation-text [(valid-mutation)])))
          out (propose {:model-call mc :model/id "lmstudio/fake"})
          m (first out)]
      (is (= 1 (count out)))
      ;; the kernel computes :expect/hash from the parent :files digest —
      ;; a model can never name a preimage it does not know
      (is (= placeholder-hash (get-in m [:ops 0 :expect/hash])))
      (is (= :set-edn (get-in m [:ops 0 :op])))          ; string -> keyword
      (is (= :behavioral (:risk m)))                     ; config default stamped
      ;; only the keys the adapter owns are returned
      (is (= #{:risk :ops} (set (keys m))))
      ;; each op is schema-valid
      (is (= (get-in m [:ops 0]) (ms/validate-op (get-in m [:ops 0]))))
      ;; the whole (completed) mutation envelope validates against the
      ;; orchestrator's validate-mutation with the parent as context
      (is (map? (mutation/validate-mutation (completed-shape m)
                                            (fixture-parent))))
      ;; the model id, message roles, and options pass through
      (is (= "lmstudio/fake" (:id (first @calls))))
      (is (= :system (-> (first @calls) (get-in [:messages 0 :role]))))
      (is (= :user (-> (first @calls) (get-in [:messages 1 :role]))))
      (is (= 0.2 (get-in (first @calls) [:options :temperature])))
      (is (= 8192 (get-in (first @calls) [:options :max-tokens]))))))

(deftest risk-config-default
  (testing "the config :risk defaults onto a mutation that omits :risk"
    (let [[mc _] (canned-call
                  (value-with (mutation-text [{:ops [{"op" "set-edn"
                                                     "file" skills-file
                                                     "path" ["x"] "value" 1}]}])))
          out (propose {:model-call mc :model/id "m" :risk :program})]
      (is (= :program (:risk (first out)))))))

(deftest expected-effect-and-hypothesis-id
  (testing "an optional :expected-effect is coerced to keywords and an
            optional :hypothesis/id is kept"
    (let [[mc _] (canned-call
                  (value-with
                   (mutation-text
                    [(assoc (valid-mutation)
                            "expected-effect" {"primary-metric" "task/success"
                                               "direction" "increase"}
                            "hypothesis-id" (str (uuid-of 5)))])))
          out (propose {:model-call mc :model/id "m"})
          m (first out)]
      (is (= {:primary-metric :task/success :direction :increase}
             (:expected-effect m)))
      (is (= (uuid-of 5) (:hypothesis/id m))))))

(deftest expected-effect-slash-spelling
  (testing "the schema's slash-spelled :expected/effect key is accepted
            alongside the prompt's hyphen spelling"
    (let [[mc _] (canned-call
                  (value-with
                   (mutation-text
                    [{:ops [{"op" "set-edn" "file" skills-file
                             "path" ["bar"] "value" 1}]
                      :risk "behavioral"
                      :expected/effect {:primary-metric "task/cost"
                                        :direction "decrease"}}])))
          out (propose {:model-call mc :model/id "m"})
          m (first out)]
      (is (= {:primary-metric :task/cost :direction :decrease}
             (:expected-effect m))))))

;; --- LLM-noise tolerance / dropped-mutation policy --------------------------

(deftest unknown-file-dropped-sibling-kept
  (testing "a mutation referencing a hallucinated file is dropped; a valid
            sibling mutation survives"
    (let [[mc _] (canned-call
                  (value-with
                   (mutation-text
                    [;; hallucinated file -> dropped
                     {:ops [{"op" "set-edn" "file" "skills/nope.edn"
                             "path" ["a"] "value" 1}]}
                     ;; valid sibling -> kept
                     (valid-mutation)])))
          out (propose {:model-call mc :model/id "m"})]
      (is (= 1 (count out)))
      (is (= skills-file (get-in out [0 :ops 0 :file]))))))

(deftest all-unknown-files-fail-loud
  (testing "a NON-EMPTY mutations array whose every mutation is dropped
            (all hallucinated files) throws :mutation/llm-response-invalid"
    (let [[mc _] (canned-call
                  (value-with
                   (mutation-text
                    [{:ops [{"op" "set-edn" "file" "skills/a.edn"
                             "path" ["a"] "value" 1}]}
                     {:ops [{"op" "set-edn" "file" "skills/b.edn"
                             "path" ["a"] "value" 1}]}])))
          e (try (propose {:model-call mc :model/id "m"}) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :mutation/llm-response-invalid (:error/type (ex-data e))))
      (is (= 2 (get-in (ex-data e) [:count]))))))

(deftest all-structurally-broken-fail-loud
  (testing "a NON-EMPTY array that is ALL noise (no :ops) fails loud"
    (let [[mc _] (canned-call
                  (value-with (mutation-text [{:risk "behavioral"}
                                              {:risk "behavioral"}])))
          e (try (propose {:model-call mc :model/id "m"}) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :mutation/llm-response-invalid (:error/type (ex-data e)))))))

(deftest partial-noise-is-tolerated
  (testing "a non-map entry and an entry with a missing :ops vector are
            skipped; the valid sibling survives"
    (let [[mc _] (canned-call
                  (value-with
                   (mutation-text
                    ["just-a-string"
                     {:risk "behavioral"}
                     (valid-mutation)])))
          out (propose {:model-call mc :model/id "m"})]
      (is (= 1 (count out)))
      (is (= skills-file (get-in out [0 :ops 0 :file]))))))

(deftest empty-mutations-returns-nil
  (testing "an empty, structurally valid \"mutations\": [] array is the
            honest way to propose nothing"
    (let [[mc _] (canned-call (value-with (mutation-text [])))]
      (is (nil? (propose {:model-call mc :model/id "m"}))))))


;; --- response shape ----------------------------------------------------------

(deftest non-json-fails-loud
  (testing "non-JSON text is rejected"
    (let [[mc _] (canned-call (value-with "Sorry, I cannot do that."))]
      (is (= :mutation/llm-response-invalid
             (thrown-error-type #(propose {:model-call mc :model/id "m"})))))))

(deftest json-in-code-fences
  (testing "JSON wrapped in code fences is parsed"
    (let [bt (char 96)   ; the ASCII backtick, kept out of the source
          wrapped (str "Here is my proposal:\n" bt bt bt "json\n"
                       (mutation-text [(valid-mutation)])
                       "\n" bt bt bt)
          [mc _] (canned-call (value-with wrapped))
          out (propose {:model-call mc :model/id "m"})]
      (is (= 1 (count out)))
      (is (= placeholder-hash (get-in out [0 :ops 0 :expect/hash]))))))

;; --- model-call failures ------------------------------------------------------

(deftest model-call-throws-exception-info
  (testing "a thrown ExceptionInfo becomes :mutation/llm-failed with the
            cause :error/type preserved"
    (let [mc (fn [& _]
               (throw (ex-info "provider down"
                                {:error/type :provider/transient-error})))
          e (try (propose {:model-call mc :model/id "m"}) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :mutation/llm-failed (:error/type (ex-data e))))
      (is (= :provider/transient-error
             (get-in (ex-data e) [:cause :error/type])))))
  (testing "a generic Throwable also becomes :mutation/llm-failed"
    (let [mc (fn [& _] (throw (RuntimeException. "oops")))]
      (is (= :mutation/llm-failed
             (thrown-error-type #(propose {:model-call mc :model/id "m"})))))))

(deftest non-string-output-fails-loud
  (testing "a non-string :text is a typed response failure"
    (let [mc (fn [& _] {:value {:model/output {:text 123}}})]
      (is (= :mutation/llm-response-invalid
             (thrown-error-type #(propose {:model-call mc :model/id "m"})))))))

(deftest non-ok-dispatch-result-fails-loud
  (testing "a dispatch result with a non-ok :result/status is a typed
            :mutation/llm-failed, never mistaken for model output"
    (let [mc (fn [& _] {:result/status :provider/not-found
                        :error/type :provider/not-found
                        :value nil})
          e (try (propose {:model-call mc :model/id "m"}) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :mutation/llm-failed (:error/type (ex-data e))))
      (is (= :provider/not-found
             (get-in (ex-data e) [:result/status]))))))

;; --- constructor config ---------------------------------------------------------

(deftest config-validation
  (testing "unknown config keys are rejected (closed map)"
    (is (= :mutation/config-invalid
           (thrown-error-type #(llm/llm-mutator
                                {:model-call identity :model/id "m"
                                 :store {:cas "/tmp"}})))))
  (testing "missing :model-call is rejected"
    (is (= :mutation/config-invalid
           (thrown-error-type #(llm/llm-mutator {:model/id "m"})))))
  (testing "missing :model/id is rejected"
    (is (= :mutation/config-invalid
           (thrown-error-type #(llm/llm-mutator {:model-call identity})))))
  (testing "non-map config is rejected"
    (is (= :mutation/config-invalid
           (thrown-error-type #(llm/llm-mutator :not-a-map)))))
  (testing "a bad :risk value is rejected"
    (is (= :mutation/config-invalid
           (thrown-error-type #(llm/llm-mutator
                                {:model-call identity :model/id "m"
                                 :risk :not-a-risk}))))))

(deftest max-mutations-cap
  (testing "model returns 5 mutations but only :max-mutations are kept"
    (let [[mc _] (canned-call
                  (value-with (mutation-text
                               (mapv (fn [_] (valid-mutation)) (range 5)))))
          out (propose {:model-call mc :model/id "m" :max-mutations 2})]
      (is (= 2 (count out))))
    (testing "fewer than :max-mutations is fine"
      (let [[mc _] (canned-call (value-with (mutation-text [(valid-mutation)])))
            out (propose {:model-call mc :model/id "m" :max-mutations 5})]
        (is (= 1 (count out)))))))

;; --- context validation ----------------------------------------------------------

(deftest context-validation
  (testing "a non-map context is a typed :mutation/context-invalid"
    (let [[mc _] (canned-call (value-with (mutation-text [(valid-mutation)])))
          m (llm/llm-mutator {:model-call mc :model/id "m"})]
      (is (= :mutation/context-invalid
             (:error/type (ex-data (try (core/propose-mutations m :nope) nil
                                        (catch clojure.lang.ExceptionInfo e e)))))))))

;; --- a pure-add op also receives the kernel-computed :expect/hash ----------------

(deftest pure-add-op-gets-hash
  (testing "a pure-add :insert-text op receives the kernel-computed
            :expect/hash when its file digest is known"
    (let [[mc _] (canned-call
                  (value-with
                   (mutation-text
                    [{:ops [{"op" "insert-text"
                             "file" skills-file
                             "position" "after"
                             "anchor" "steps"
                             "text" "(defn debug [] 1)"}]}])))
          out (propose {:model-call mc :model/id "m"})
          op (get-in out [0 :ops 0])]
      (is (= placeholder-hash (:expect/hash op)))
      (is (= :after (:position op)))            ; string -> keyword coerced
      (is (= op (ms/validate-op op))))))

(deftest model-style-colon-keywords-are-sanitized
  (testing "models emit keywords with leading colons in JSON (\"::type\",
            \":steps\"); cheshire keywordizes them into names that would
            pr-str to ILLEGAL EDN (::steps). The adapter rewrites them to
            plain keywords so the mutation never compile-fails the
            candidate."
    (let [[mc _] (canned-call
                  (value-with
                   (mutation-text
                    [{:ops [{"op" "set-edn"
                             "file" skills-file
                             "path" ["steps"]
                             "value" {":steps" [{":name" "a"
                                                 ":action" "b"}]}}]}]
                   )))
          out (propose {:model-call mc :model/id "m"})
          op (get-in out [0 :ops 0])
          value (:value op)]
      (is (= placeholder-hash (:expect/hash op)))
      (is (= {:steps [{:name "a" :action "b"}]} value))
      (is (= op (ms/validate-op op)))
      (is (not (re-find #"::" (pr-str value)))
          (str "no illegal :: token survives: " (pr-str value))))))

;; --- user message carries the bounded context ------------------------------------

(deftest user-message-rendered
  (testing "the model receives a user message that is a bounded rendering
            of the diagnosis, mutable surface, history, and budget"
    (let [[mc calls] (canned-call (value-with (mutation-text [(valid-mutation)])))
          _ (propose {:model-call mc :model/id "m"})
          user (:content (second (get-in (first @calls) [:messages])))]
      (is (string? user))
      (is (not= "" user))
      (is (re-find #"Diagnosis" user))
      (is (re-find #"Parent genome" user))
      (is (re-find #"Budget profile" user))
      (is (re-find #"Recent mutation history" user)))))



;; ============================================================================
;; Task E1 — :evolution/evidence and :evolution/history broker tools
;;
;; The two retrieval tools are READ-ONLY (Global Constraint 8: every
;; external effect crosses the broker; these tools only READ the frozen
;; evidence pack from the CAS and the durable lineage rows), and each is
;; SUBJECT-BOUND: a tool-call intent carries the requesting phenotype's
;; attribution (Global Constraint 20) and the broker authorizes it
;; against a host-owned lease binding that exact phenotype (a sibling
;; phenotype is denied with the standard deny codes).
;;
;; Fixture design mirrors the store tests: a migrated temp database
;; seeded with the parent generation row, a temp CAS root, the pack
;; body written to the CAS exactly as build-evidence-pack freezes it
;; (canonical pr-str WITHOUT :evidence/id — the id IS the content
;; address), and durable lineage rows inserted directly for the
;; candidate/history paths.
;; ============================================================================

(def ^:private mutator-phenotype-id
  "The deterministic content-addressed subject the evolution adapters
  are attributed to (the same derivation as
  evoclj.kernel.system/build-model-call: sha256 of the
  \"evoclj/evolution\" prefix — here a fixed fixture digest)."
  (str "sha256:" (apply str (repeat 64 "e"))))

(def ^:private sibling-phenotype-id
  "A different phenotype id — a sibling from the same Genome is a
  different subject and must NOT match a lease for mutator-phenotype-id
  (Global Constraint 9)."
  (str "sha256:" (apply str (repeat 64 "f"))))

(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private issued-at (java.util.Date. 0))
(def ^:private expires-at (java.util.Date. 4102444800000)) ; year 2100
(def ^:private now (java.util.Date. 1700000000000))

;; --- temp stores ----------------------------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-mutator-tools-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-mutator-tools-cas-"
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
  "A migrated temp database seeded with the parent generation row
  (current = 1, Database Invariant 6) plus a temp CAS root. Returns the
  executor :stores map {:sqlite ... :cas ...}."
  []
  (let [path (temp-db-path)
        db (sqlite/spec path)
        cas-root (temp-cas-dir)]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id "generation-1"
                     :genome_id placeholder-hash
                     :resolution_id placeholder-hash
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    {:sqlite db :cas (cas/->cas cas-root)}))

(defn- fixture-pack
  "A schema-valid frozen EvidencePack (the shape build-evidence-pack
  freezes): compact episode refs only — no trace payload bytes ever
  cross into the pack (Global Constraint 21)."
  []
  {:evidence/id placeholder-hash
   :generation/id "generation-1"
   :cutoff-event-id 1
   :episodes [{:episode/id (uuid-of 21)
               :session/id (uuid-of 22)
               :generation/id "generation-1"
               :excerpt-ref placeholder-hash
               :outcome {:status :completed}
               :trace {:first-event 1 :last-event 1}
               :usage {}}]
   :summary {:selector {:recent 1 :include-successes 0
                        :include-failures 0 :include-high-cost 0}
             :seed nil
             :eligible 1 :selected 1
             :successes 1 :failures 0 :high-cost 0}})

(defn- freeze-pack!
  "Write the pack body to the CAS exactly as build-evidence-pack does —
  the canonical pr-str WITHOUT :evidence/id — and return the pack with
  the REAL content address (:evidence/id IS the content hash of the
  stored body, so a get-bytes under the returned id resolves it)."
  [store]
  (let [pack (fixture-pack)
        put (cas/put-bytes! (:cas store)
                            (.getBytes (pr-str (dissoc pack :evidence/id))
                                       StandardCharsets/UTF_8)
                            {:media-type "application/edn"})]
    (assoc pack :evidence/id (:artifact/id put))))

(defn- insert-candidate!
  "Insert the durable lineage rows (mutations + candidates) for a
  candidate of generation-1 whose frozen evidence pack is
  `evidence-id`; returns the candidate uuid."
  [store evidence-id]
  (let [mid (uuid-of 91)
        cid (uuid-of 92)]
    (sqlite/with-db [conn (:sqlite store)]
      (jdbc/insert! conn :mutations
                    {:id (str mid)
                     :parent_genome_id placeholder-hash
                     :hypothesis_id (str (uuid-of 7))
                     :evidence_id evidence-id
                     :risk "behavioral"
                     :ops (pr-str [{:op :set-edn
                                    :file skills-file
                                    :path [:workflow :before-edit]
                                    :expect/hash placeholder-hash
                                    :value [:reproduce :localize]}])
                     :expected_effect (pr-str {:primary-metric :task/success
                                               :direction :increase})
                     :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :candidates
                    {:id (str cid)
                     :parent_generation_id "generation-1"
                     :parent_genome_id placeholder-hash
                     :genome_id placeholder-hash
                     :mutation_id (str mid)
                     :evidence_id evidence-id
                     :risk "behavioral"
                     :state "materialized"
                     :created_at "2025-01-01T00:00:00Z"}))
    cid))

(defn- insert-history-entry!
  "Insert one REJECTED history entry (mutations + candidates +
  eval_runs rows) for generation-1. `n` varies the mutation content and
  the timestamps, so every entry has a distinct :mutation/hash and a
  deterministic newest-first order."
  [store n]
  (let [mutation-id (str (uuid-of (+ 100 n)))
        candidate-id (str (uuid-of (+ 200 n)))
        eval-id (str (uuid-of (+ 300 n)))
        ops (pr-str [{:op :set-edn
                      :file skills-file
                      :path [:workflow :before-edit]
                      :expect/hash placeholder-hash
                      :value [:entry n]}])
        created-at (str (.plusSeconds (java.time.Instant/parse
                                       "2025-01-01T00:00:00Z")
                                      (* n 60)))]
    (sqlite/with-db [conn (:sqlite store)]
      (jdbc/insert! conn :mutations
                    {:id mutation-id
                     :parent_genome_id placeholder-hash
                     :hypothesis_id (str (uuid-of 7))
                     :evidence_id placeholder-hash
                     :risk "behavioral"
                     :ops ops
                     :expected_effect (pr-str {:primary-metric :task/success
                                               :direction :increase})
                     :created_at created-at})
      (jdbc/insert! conn :candidates
                    {:id candidate-id
                     :parent_generation_id "generation-1"
                     :parent_genome_id placeholder-hash
                     :genome_id placeholder-hash
                     :mutation_id mutation-id
                     :evidence_id placeholder-hash
                     :risk "behavioral"
                     :state "materialized"
                     :created_at created-at})
      (jdbc/insert! conn :eval_runs
                    {:id eval-id
                     :candidate_id candidate-id
                     :parent_generation_id "generation-1"
                     :profile_id "default-v1"
                     :gates (pr-str [:g0-parse])
                     :paired_results_ref nil
                     :summary (pr-str {:utility {:task/success
                                                 {:parent 0.5 :candidate 0.25}}})
                     :eligibility (pr-str {:eligible? false
                                           :reasons ["utility regression"]})
                     :status "finalized"
                     :created_at created-at}))))

;; --- broker plumbing ------------------------------------------------------

(defn- tool-intent
  "A validated :intent/tool-call for one evolution retrieval tool,
  attributed to `phenotype-id`."
  [phenotype-id tool-id args]
  (intent/tool-call session-id phenotype-id :node/evolution 0
                    {:tool/id tool-id :args args}
                    {:wall-ms 1000 :max-steps 1}))

(defn- broker-context
  "A dispatcher broker context over a fresh registry with the given
  providers registered, the given leases, a fresh usage atom, and a
  pinned decision clock."
  [providers leases]
  (let [reg (registry/create-registry)]
    (doseq [p providers]
      (registry/register! reg p))
    (dispatch/make-broker-context
     {:registry reg :leases leases :usage (atom {}) :now (constantly now)})))

(defn- store-snapshot
  "A plain-data snapshot of the store's writable surface: every CAS
  body plus the row counts of the lineage tables. Two equal snapshots
  prove a tool run mutated nothing."
  [store]
  (let [cas-root (Paths/get (str (:root (:cas store))) (make-array String 0))
        bodies (with-open [stream (Files/walk cas-root
                                              (make-array FileVisitOption 0))]
                 (->> (iterator-seq (.iterator stream))
                      (filter #(.endsWith (.toString %) "body"))
                      (mapv (fn [p] (String. (Files/readAllBytes p)
                                             StandardCharsets/UTF_8)))
                      sort
                      vec))
        rows (into {}
                   (map (fn [t]
                          [t (count (sqlite/query (:sqlite store)
                                                  [(str "SELECT 1 FROM " (name t))]))]))
                   [:generations :mutations :candidates :eval_runs
                    :episodes :events :sessions :promotions])]
    {:cas-bodies bodies :rows rows}))

;; --- :evolution/evidence ---------------------------------------------------

(deftest evidence-tool-returns-pack-fields-by-evidence-id
  (testing "a tool call through the broker resolves the frozen evidence
            pack by :evidence/id and returns its scoped fields"
    (let [store (fresh-store)
          pack (freeze-pack! store)
          ctx (broker-context
               [(evo-tools/evidence-provider store)]
               [(evo-tools/evolution-tool-lease
                 mutator-phenotype-id :evolution/evidence
                 {:issued-at issued-at :expires-at expires-at})])
          result (dispatch/dispatch!
                  ctx (tool-intent mutator-phenotype-id :evolution/evidence
                                   {:evidence/id (:evidence/id pack)}))]
      (is (= :ok (:result/status result)))
      (is (= :allow (get-in result [:authorization :decision])))
      (let [v (:value result)]
        (is (= (:evidence/id pack) (:evidence/id v)))
        (is (= "generation-1" (:generation/id v)))
        (is (= 1 (:cutoff-event-id v)))
        (is (= (:episodes pack) (:episodes v)))
        (is (= (:summary pack) (:summary v)))))))

(deftest evidence-tool-resolves-by-candidate-id
  (testing "the pack is resolved by :candidate/id through the durable
            candidates row's :evidence_id"
    (let [store (fresh-store)
          pack (freeze-pack! store)
          cid (insert-candidate! store (:evidence/id pack))
          ctx (broker-context
               [(evo-tools/evidence-provider store)]
               [(evo-tools/evolution-tool-lease
                 mutator-phenotype-id :evolution/evidence
                 {:issued-at issued-at :expires-at expires-at})])
          result (dispatch/dispatch!
                  ctx (tool-intent mutator-phenotype-id :evolution/evidence
                                   {:candidate/id cid}))]
      (is (= :ok (:result/status result)))
      (is (= (:evidence/id pack) (get-in result [:value :evidence/id])))
      (is (= (:episodes pack) (get-in result [:value :episodes]))))))

(deftest evidence-tool-missing-candidate-is-a-typed-value
  (testing "an unknown :candidate/id resolves to {:found false} — a
            value, never a crash"
    (let [store (fresh-store)
          _ (freeze-pack! store)
          ctx (broker-context
               [(evo-tools/evidence-provider store)]
               [(evo-tools/evolution-tool-lease
                 mutator-phenotype-id :evolution/evidence
                 {:issued-at issued-at :expires-at expires-at})])
          result (dispatch/dispatch!
                  ctx (tool-intent mutator-phenotype-id :evolution/evidence
                                   {:candidate/id (uuid-of 99)}))]
      (is (= :ok (:result/status result)))
      (is (= {:found false :reason :candidate-not-found
              :candidate/id (uuid-of 99)}
             (:value result))))))

;; --- :evolution/history ----------------------------------------------------

(deftest history-tool-returns-rejection-window
  (testing "the tool returns the rejection-history window for the
            lineage, newest first, with verdicts and reasons"
    (let [store (fresh-store)
          _ (doseq [n (range 3)] (insert-history-entry! store n))
          ctx (broker-context
               [(evo-tools/history-provider store)]
               [(evo-tools/evolution-tool-lease
                 mutator-phenotype-id :evolution/history
                 {:issued-at issued-at :expires-at expires-at})])
          result (dispatch/dispatch!
                  ctx (tool-intent mutator-phenotype-id :evolution/history
                                   {:generation-lineage ["generation-1"]}))]
      (is (= :ok (:result/status result)))
      (let [entries (:value result)]
        (is (= 3 (count entries)))
        (is (= (str (uuid-of 102)) (str (get-in entries [0 :mutation/id]))))
        (is (every? #(= :rejected (:state %)) entries))
        (is (every? #(= ["utility regression"] (:reason %)) entries)))))
    (testing "an explicit :limit bounds the window"
      (let [store (fresh-store)
            _ (doseq [n (range 3)] (insert-history-entry! store n))
            ctx (broker-context
                 [(evo-tools/history-provider store)]
                 [(evo-tools/evolution-tool-lease
                   mutator-phenotype-id :evolution/history
                   {:issued-at issued-at :expires-at expires-at})])
            result (dispatch/dispatch!
                    ctx (tool-intent mutator-phenotype-id :evolution/history
                                     {:generation-lineage ["generation-1"]
                                      :limit 2}))]
        (is (= :ok (:result/status result)))
        (is (= 2 (count (:value result))))
        (is (= (str (uuid-of 102)) (str (get-in result [:value 0 :mutation/id])))))))

(deftest history-tool-window-default-50-and-max-500
  (testing "the default window is 50 entries even when more history
            exists"
    (let [store (fresh-store)
          _ (doseq [n (range 55)] (insert-history-entry! store n))
          ctx (broker-context
               [(evo-tools/history-provider store)]
               [(evo-tools/evolution-tool-lease
                 mutator-phenotype-id :evolution/history
                 {:issued-at issued-at :expires-at expires-at})])
          result (dispatch/dispatch!
                  ctx (tool-intent mutator-phenotype-id :evolution/history
                                   {:generation-lineage ["generation-1"]}))]
      (is (= :ok (:result/status result)))
      (is (= 50 (count (:value result))))))
  (testing "a window over the 500 cap is rejected at the input gate
            (:provider/input-invalid)"
    (let [store (fresh-store)
          ctx (broker-context
               [(evo-tools/history-provider store)]
               [(evo-tools/evolution-tool-lease
                 mutator-phenotype-id :evolution/history
                 {:issued-at issued-at :expires-at expires-at})])
          result (dispatch/dispatch!
                  ctx (tool-intent mutator-phenotype-id :evolution/history
                                   {:generation-lineage ["generation-1"]
                                    :limit 501}))]
      (is (= :error (:result/status result)))
      (is (= :provider/input-invalid (:error/type result))))))

;; --- subject binding -------------------------------------------------------

(deftest out-of-scope-subject-denied-with-standard-deny-codes
  (testing "a lease binds ONE phenotype: a sibling phenotype is denied
            with :capability/subject-mismatch and the provider never runs"
    (let [store (fresh-store)
          _ (freeze-pack! store)
          ctx (broker-context
               [(evo-tools/evidence-provider store)]
               [(evo-tools/evolution-tool-lease
                 mutator-phenotype-id :evolution/evidence
                 {:issued-at issued-at :expires-at expires-at})])
          result (dispatch/dispatch!
                  ctx (tool-intent sibling-phenotype-id :evolution/evidence
                                   {:evidence/id placeholder-hash}))]
      (is (= :error (:result/status result)))
      (is (= :capability/denied (:error/type result)))
      (is (= :capability/subject-mismatch (get-in result [:error/data :reason])))))
  (testing "no lease at all is denied with :capability/missing"
    (let [store (fresh-store)
          ctx (broker-context
               [(evo-tools/history-provider store)]
               [])
          result (dispatch/dispatch!
                  ctx (tool-intent mutator-phenotype-id :evolution/history
                                   {:generation-lineage ["generation-1"]}))]
      (is (= :error (:result/status result)))
      (is (= :capability/denied (:error/type result)))
      (is (= :capability/missing (get-in result [:error/data :reason]))))))

;; --- read-only guarantee ---------------------------------------------------

(deftest evolution-tools-are-read-only
  (testing "both tools declare :effect :pure and never change the
            store: CAS bodies and lineage row counts are identical
            before and after"
    (let [store (fresh-store)
          pack (freeze-pack! store)
          _ (insert-candidate! store (:evidence/id pack))
          _ (doseq [n (range 2)] (insert-history-entry! store n))
          ctx (broker-context
               [(evo-tools/evidence-provider store)
                (evo-tools/history-provider store)]
               [(evo-tools/evolution-tool-lease
                 mutator-phenotype-id :evolution/evidence
                 {:issued-at issued-at :expires-at expires-at})
                (evo-tools/evolution-tool-lease
                 mutator-phenotype-id :evolution/history
                 {:issued-at issued-at :expires-at expires-at})])
          before (store-snapshot store)
          _ (dispatch/dispatch!
             ctx (tool-intent mutator-phenotype-id :evolution/evidence
                              {:evidence/id (:evidence/id pack)}))
          _ (dispatch/dispatch!
             ctx (tool-intent mutator-phenotype-id :evolution/evidence
                              {:candidate/id (uuid-of 92)}))
          _ (dispatch/dispatch!
             ctx (tool-intent mutator-phenotype-id :evolution/history
                              {:generation-lineage ["generation-1"]}))
          after (store-snapshot store)]
      (is (= :pure (:effect evo-tools/evidence-tool-descriptor)))
      (is (= :pure (:effect evo-tools/history-tool-descriptor)))
      (is (= before after)))))

;; --- the mutator's tool catalog --------------------------------------------

(deftest mutator-tool-catalog-includes-evolution-tools
  (testing "the mutator's tool catalog IS the two read-only evolution
            retrieval tools, in the wire form the tool loop consumes"
    (is (= evo-tools/mutator-tool-catalog llm/mutator-tool-catalog))
    (is (= #{:evolution/evidence :evolution/history}
           (set (map :tool llm/mutator-tool-catalog))))
    (is (= ["evolution_evidence" "evolution_history"]
           (mapv :name llm/mutator-tool-catalog))))
  (testing "the model-call options declare the tool-loop round bound"
    (let [[mc calls] (canned-call (value-with (mutation-text [(valid-mutation)])))
          _ (propose {:model-call mc :model/id "m"})
          opts (get-in (first @calls) [:options])]
      (is (= 4 (:max-tool-rounds opts))))))

(deftest unexecuted-tool-calls-fail-loud
  (testing "a model response requesting tools that the host :model-call
            closure did not execute is a typed response failure — the
            adapter holds no broker and never guesses at unexecuted
            tool calls"
    (let [mc (fn [& _] {:value {:model/output {:text ""}
                                :usage {:prompt-tokens 1 :completion-tokens 1}
                                :tool-calls [{:tool/call-id "call-1"
                                              :tool/name "evolution_evidence"
                                              :tool/arguments {}}]}})
          e (try (propose {:model-call mc :model/id "m"}) nil
                 (catch clojure.lang.ExceptionInfo e e))
          data (ex-data e)]
      (is (= :mutation/llm-response-invalid (:error/type data)))
      (is (= :tool-calls-unexecuted (:reason data))))))
