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
            [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.core :as core]
            [evoclj.evolution.llm-mutator :as llm]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.evolution.mutation-schema :as ms])
  (:import (java.nio.charset StandardCharsets)
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


