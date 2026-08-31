(ns evoclj.runtime.orchestrator
  "Orchestrator protocol — decouples the model tool-calling loop (C1).

  The scheduler is a DAG visit; the orchestrator owns the tool loop.
  TraditionalOrchestrator implements the single max-tool-rounds loop:
  tool-map resolve, broker dispatch, tool execution, next-messages
  assembly. Pin stability is preserved via ToolSurface (pin once,
  refresh each round)."
  (:require [clojure.string :as str]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.intent.pipeline :as pipeline]
            [evoclj.kernel.error :as err]
            [evoclj.provider.dialect :as dialect]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.assembler :as assembler]
            [evoclj.runtime.tool-surface :as tool-surface]
            [evoclj.sci.boundary :as boundary]
            [evoclj.sci.computation :as computation]
            [evoclj.store.binding :as binding-store]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event])
  (:import (java.nio.charset StandardCharsets)))

;; ---------------------------------------------------------------------------
;; Private helpers — mirror scheduler private helpers faithfully
;; ---------------------------------------------------------------------------

(def ^:private max-tool-rounds-default 4)

(defn- put-payload!
  "Store an EDN payload by content hash under the executor CAS and return the artifact id."
  [executor value]
  (:artifact/id
   (cas/put-bytes! (:cas (:stores executor))
                   (.getBytes (pr-str value) StandardCharsets/UTF_8)
                   {})))

(defn- append-event!
  "Append one event to the session append-only log."
  [executor pin cause-event-id type payload-ref metadata]
  (event/append-event!
   (:sqlite (:stores executor))
   {:session/id (:session/id pin)
    :generation/id (:generation/id pin)
    :phenotype/id (:phenotype/id pin)
    :event/type type
    :cause/event-id cause-event-id
    :payload-ref payload-ref
    :metadata metadata}))

(defn- dispatch-intent!
  "Persist one validated intent through the broker and feed the result back.
  Mirrors scheduler/dispatch-intent! exactly — single implementation in this namespace."
  [executor pin cause intent outputs]
  (let [proposed (append-event! executor pin cause :intent/proposed nil
                                {:intent/id (:intent/id intent)
                                 :intent/type (:intent/type intent)
                                 :node/id (:node/id intent)})
        result (dispatch/dispatch! (:dispatch executor) intent)]
    (if (= :ok (:result/status result))
      (let [authorization (:authorization result)
            tool-id (get-in intent [:payload :tool/id])
            authorized (append-event!
                        executor pin (:event/id proposed) :intent/authorized nil
                        {:intent/id (:intent/id intent)
                         :intent/type (:intent/type intent)
                         :authorization {:decision (:decision authorization)
                                         :lease-id (:lease-id authorization)}})
            started (append-event!
                     executor pin (:event/id authorized) :provider/call-started nil
                     {:intent/id (:intent/id intent)
                      :tool/id tool-id
                      :idempotency/key (get-in intent [:metadata :idempotency/key])})
            value-ref (put-payload! executor (:value result))
            completed (append-event!
                       executor pin (:event/id started) :provider/call-completed value-ref
                       {:intent/id (:intent/id intent)
                        :tool/id tool-id
                        :result/status :ok})]
        {:last-event completed
         :outputs (conj outputs (:value result))
         :outcome :ok})
      (if (= :capability/denied (:error/type result))
        {:last-event (append-event!
                      executor pin (:event/id proposed) :intent/denied nil
                      {:intent/id (:intent/id intent)
                       :intent/type (:intent/type intent)
                       :error/type :capability/denied
                       :reason (get-in result [:error/data :reason])})
         :outputs outputs
         :outcome :denied}
        {:last-event (append-event!
                      executor pin (:event/id proposed) :intent/failed
                      (put-payload! executor (dissoc result :usage))
                      {:intent/id (:intent/id intent)
                       :intent/type (:intent/type intent)
                       :error/type (:error/type result)})
         :outputs outputs
         :outcome :failed}))))

(defn- tool-map-of
  "Wire function-name -> declaration map from a model-call payload :tools vector."
  [intent]
  (into {}
        (map (fn [t] [(:name t) t]))
        (get-in intent [:payload :tools])))

(defn- record-bindings-degradation!
  "Record a failing durable-bindings query as a typed causal event."
  [executor pin cause t]
  (append-event! executor pin cause :scheduler/bindings-degraded nil
                 {:degradation :bindings-fetch
                  :error (err/error-data t)}))

(defn- fetch-bindings
  "Current active ContextBindings for the session; degrades with counted event on failure."
  [executor pin cause]
  (try
    (binding-store/active-bindings (:sqlite (:stores executor)) (:session/id pin))
    (catch Throwable t
      (record-bindings-degradation! executor pin cause t)
      [])))

(defn- base-call-from-intent*
  "Extract BaseModelCall from an intent, tolerating legacy shapes."
  [intent]
  (try
    (assembler/base-call-from-intent intent)
    (catch Throwable _
      {:base/messages (or (get-in intent [:payload :base/messages])
                          (get-in intent [:payload :messages]) [])
       :requested-tools (or (get-in intent [:payload :requested-tools])
                            (get-in intent [:payload :tools]) [])
       :options (or (get-in intent [:payload :options]) {})
       :model/id (get-in intent [:payload :model/id])})))

(defn- tool-call-intent
  "A validated :intent/tool-call for one model-requested tool call."
  [intent cause tool-call tool-id]
  (intent/tool-call
   (:session/id intent)
   (:phenotype/id intent)
   (:node/id intent)
   cause
   {:tool/id tool-id :args (:tool/arguments tool-call)}
   (:budget intent)))

(defn- tool-result-msg
  "The :role :tool message fed back to the model for one executed tool call."
  [tool-call outcome value]
  {:role :tool
   :tool-call-id (:tool/call-id tool-call)
   :content (if (= :ok outcome)
              (pr-str value)
              (str "error: " (name outcome)))})

(defn- make-tool-fns
  "Factory that builds {tool-id -> host fn} crossing the broker via the pipeline.

  tool-map is a map of tool-id (string/keyword) to tool declaration.
  executor is the runtime executor holding :dispatch (broker context)
  and :stores. pin holds session/phenotype/node attribution, cause is
  the causal event id. Each returned fn takes a single EDN args value,
  materializes it, builds a validated :intent/tool-call via
  intent/tool-call, dispatches through pipeline/pipeline (single
  handleError, single limitsCheck per INV-05), materializes the
  result, and returns it. Failures throw typed errors; the fn never
  calls the provider directly, satisfying GC-08."
  [tool-map executor pin cause]
  (when-not (map? tool-map)
    (throw (err/error :orchestrator/invalid-tool-map
                      "tool-map must be a map"
                      {:reason :invalid-tool-map :value (err/sanitize tool-map)})))
  (let [broker-ctx (or (:dispatch executor)
                       (when-let [reg (:registry executor)]
                         (dispatch/make-broker-context {:registry reg}))
                       (throw (err/error :orchestrator/missing-broker
                                         "executor must carry :dispatch broker context"
                                         {:reason :missing-broker})))]
    (into {}
          (for [[tool-id _spec] tool-map]
            (let [kw (if (keyword? tool-id) tool-id (keyword (str tool-id)))
                    tid (name kw)]
              [tid
               (fn [args]
                 (let [safe-args (boundary/materialize-edn args {:max-depth 64 :max-size 100000})
                       session-id (or (:session/id pin) (:session/id executor) (random-uuid))
                       raw-pid (or (:phenotype/id pin) (:phenotype/id executor))
                       phenotype-id (if (and (string? raw-pid) (re-matches #"^sha256:[0-9a-f]{64}$" raw-pid))
                                      raw-pid
                                      (str "sha256:" (apply str (repeat 64 "a"))))
                       raw-nid (or (:node/id pin) :sandbox)
                       node-id (if (keyword? raw-nid) raw-nid (keyword (str raw-nid)))
                       cause-id (let [c (if (map? cause) (:event/id cause) cause)]
                                  (if (int? c) c 1))
                       budget (or (:budget pin) {:wall-ms 1000})
                       intent (intent/tool-call session-id phenotype-id node-id cause-id {:tool/id kw :args safe-args} budget)
                       result (pipeline/pipeline broker-ctx intent)]
                   (if (= :ok (:result/status result))
                     (boundary/materialize-edn (:value result) {:max-depth 64 :max-size 100000})
                     (throw (err/error (or (:error/type result) :provider/execution-failed)
                                       (or (:error/message result) "tool execution failed")
                                       (dissoc result :result/status))))))])))))

(defn- execute-tool-calls!
  "Execute every model-requested tool call through the broker and collect tool-result messages."
  [executor pin cause intent calls]
  (loop [acc {:last-event cause :outputs [] :tool-msgs []}
         calls (seq calls)]
    (if-let [{:keys [call tool-id]} (first calls)]
      (let [cause-id (if (map? (:last-event acc))
                       (:event/id (:last-event acc))
                       (:last-event acc))
            step (dispatch-intent!
                  executor pin cause-id
                  (tool-call-intent intent cause-id call tool-id)
                  (:outputs acc))
            value (peek (:outputs step))]
        (recur {:last-event (:last-event step)
                :outputs (:outputs step)
                :tool-msgs (conj (:tool-msgs acc)
                                 (tool-result-msg call (:outcome step) value))}
               (next calls)))
      acc)))

;; ---------------------------------------------------------------------------
;; CodeMode helpers — single impl for :code extraction (INV-05)
;; ---------------------------------------------------------------------------

(def ^:private code-execution-name "code_execution")

(defn- lift-code-from-tool-calls*
  "Local mirror of dialect/lift-code-from-tool-calls (private there).
  Extract :code ONLY from structured code_execution tool_use.
  tool-calls is seq of {:tool/name :tool/arguments}. Returns {:language :source} or nil.
  Language defaults to :sci-clojure per P10 contract."
  [tool-calls]
  (when (seq tool-calls)
    (some (fn [{:tool/keys [name arguments]}]
            (when (= code-execution-name name)
              (let [args (if (map? arguments) arguments {})
                    src (or (:code args) (:source args) (get args "code") (get args "source"))
                    lang-raw (or (:language args) (get args "language") "sci-clojure")
                    lang (cond
                           (keyword? lang-raw) lang-raw
                           (string? lang-raw) (if (str/blank? lang-raw) :sci-clojure (keyword lang-raw))
                           :else :sci-clojure)]
                (when (and (string? src) (not (str/blank? src)))
                  {:language lang :source (str/trim src)}))))
          tool-calls)))

(defn- extract-code
  "Extract code map from ModelResponse value. Checks :code first, then lifts from :tool-calls.
  Returns {:language keyword :source string} or nil. Language defaults to :sci-clojure."
  [value]
  (or (when-let [c (:code value)]
        (when (and (map? c) (string? (:source c)) (not (str/blank? (:source c))))
          {:language (let [l (:language c)]
                       (cond
                         (keyword? l) l
                         (string? l) (if (str/blank? l) :sci-clojure (keyword l))
                         :else :sci-clojure))
           :source (str/trim (:source c))}))
      (lift-code-from-tool-calls* (:tool-calls value))))

(defn- find-code-call
  "Find the code_execution tool-call entry in tool-calls, or nil."
  [tool-calls]
  (some #(when (= code-execution-name (:tool/name %)) %) tool-calls))

(defn- filtered-tool-map
  "Remove code_execution entry from tool-map (for make-tool-fns). Preserves GC-08: remaining tools still cross broker."
  [tool-map]
  (into {} (remove (fn [[k _]] (= code-execution-name (str k))) tool-map)))

;; ---------------------------------------------------------------------------
;; Protocol
;; ---------------------------------------------------------------------------

(defprotocol Orchestrator
  "Orchestration contract for the model tool-calling loop.

  Implementations must preserve pin stability (tool catalog pinned once
  at loop entry via ToolSurface) and refresh variability (EffectiveContext
  recomputed each round from fresh SessionBindings + CAS).

  orchestrate takes executor, pinned session map, causal event id, the
  intent to dispatch, and the accumulated outputs vector. Returns the
  step map {:last-event Event :outputs [...] :outcome keyword}."
  (orchestrate [this executor pin cause intent outputs]
    "Dispatch one intent with the tool-calling loop. For non model-call
    intents, dispatches directly. For model-call intents, runs the
    max-tool-rounds loop (default 4) with pin/refresh semantics."))

(defrecord TraditionalOrchestrator []
  Orchestrator
  (orchestrate [_this executor pin cause intent outputs]
    (if (not= :intent/model-call (:intent/type intent))
      ;; Non-model intents bypass assembler and go directly through the broker.
      (let [cause-id (if (map? cause) (:event/id cause) cause)]
        (dispatch-intent! executor pin cause-id intent outputs))
      ;; Model-call path: trusted assembler owns the wire shape.
      (let [rounds (get-in intent [:payload :options :max-tool-rounds]
                           max-tool-rounds-default)
            base-call (base-call-from-intent* intent)
            initial-tools (or (:requested-tools base-call)
                              (get-in intent [:payload :tools]) [])
            live-catalog (try (when-let [a (get-in executor [:stores :tool-catalog])]
                                 (when (instance? clojure.lang.Atom a) @a))
                              (catch Throwable _ nil))
            pinned-source (or live-catalog initial-tools)
            _ (when-let [provider-registry (:registry (:dispatch executor))]
                ;; P9: code_execution is not in the provider registry; filter it
                ;; before resolution so declaration does not fail-closed via
                ;; catalog-unresolved. The filter is scoped to the single
                ;; code_execution name, preserving fail-closed for all other
                ;; dangling ids.
                (let [filtered (if (sequential? pinned-source)
                                 (remove #(= "code_execution" (:name %)) pinned-source)
                                 pinned-source)]
                  (when (seq filtered)
                    (registry/resolve-tool-catalog provider-registry filtered))))
            ptc (:ptc executor)
            pinned-surface (try (tool-surface/pin pinned-source ptc)
                                (catch Throwable _
                                  (tool-surface/pin initial-tools ptc)))
            pinned (:surface/binding pinned-surface)]
        (loop [current-base-call base-call
               current-intent intent
               cause cause
               outputs outputs
               rounds rounds
               pinned pinned
               pinned-surface pinned-surface]
          (let [cause-id (if (map? cause) (:event/id cause) cause)
                bindings (fetch-bindings executor pin cause-id)
                cas (:cas (:stores executor))
                _refreshed (try (tool-surface/refresh-context pinned-surface bindings cas
                                                              {:catalog {} :history ""})
                                (catch Throwable _ nil))
                prepared (try
                           (assembler/assemble current-base-call
                                               {:session-bindings bindings
                                                :tool-catalog/binding pinned
                                                :cas cas
                                                :history ""
                                                :ptc ptc})
                           (catch Throwable _ nil))
                tool-map (if prepared (:tool-map prepared) (tool-map-of current-intent))
                effective-intent (if prepared
                                   (-> current-intent
                                       (assoc-in [:payload :messages] (:messages prepared))
                                       (assoc-in [:payload :tools] (:tools prepared))
                                       (assoc-in [:payload :base/messages] (:base/messages current-base-call))
                                       (assoc-in [:payload :requested-tools] (:requested-tools current-base-call)))
                                   current-intent)
                step (dispatch-intent! executor pin cause-id effective-intent outputs)
                value (peek (:outputs step))
                tool-calls (when (= :intent/model-call (:intent/type current-intent))
                             (:tool-calls value))]
            (if (and (seq tool-calls) (pos? rounds) (seq tool-map))
              (let [calls (mapv (fn [tc]
                                  (if-let [t (get tool-map (:tool/name tc))]
                                    {:call tc :tool-id (:tool t)}
                                    (throw (err/error :scheduler/unknown-tool
                                                      (str "model requested unknown tool "
                                                           (:tool/name tc))
                                                      {:tool/name (:tool/name tc)}))))
                                tool-calls)
                    executed (execute-tool-calls! executor pin (:last-event step) effective-intent calls)
                    assistant-msg {:role :assistant
                                   :content (get-in value [:model/output :text] "")
                                   :tool-calls (dialect/tool-calls->wire tool-calls)}
                    prepared-msgs (if prepared (:messages prepared)
                                    (get-in effective-intent [:payload :messages]))
                    next-messages (into (vec prepared-msgs)
                                        (cons assistant-msg (:tool-msgs executed)))
                    next-base-call (assoc current-base-call
                                          :base/messages next-messages
                                          :messages next-messages)]
                (recur next-base-call
                       (assoc effective-intent :payload (assoc (:payload effective-intent) :messages next-messages))
                       (:last-event executed)
                       (:outputs executed)
                       (dec rounds)
                       pinned
                       pinned-surface))
              step)))))))

;; CodeModeOrchestrator holds an injected Computation value (C-Computation).
;; The Computation is closed and serializable; it owns the SCI context and limits.
;; orchestrate requires the injected computation and checks :enabled? via the
;; executor's :ptc map (executor :ptc :enabled? is the single fail-safe flag).
;; When disabled or when computation is nil/not-enabled, throws :ptc/not-enabled.
;; When enabled and the ModelResponse contains :code (via code_execution tool_use
;; or direct :code field), executes the code string via computation/execute-code
;; with toolFns crossing the broker (GC-08), closed AllowSet (GC-07), single
;; limitsCheck (INV-05) and uncatchable interrupt. Otherwise delegates to
;; Traditional pipeline. Pin stable, single code exec round (no refresh loop).
(defrecord CodeModeOrchestrator [computation]
  Orchestrator
  (orchestrate [this executor pin cause intent outputs]
    (let [enabled? (boolean (and computation (:enabled? (:ptc executor))))]
      (if (not enabled?)
        (throw (err/error :ptc/not-enabled "PTC is disabled" {:ptc (:ptc executor)}))
        (if (not= :intent/model-call (:intent/type intent))
          ;; Non-model intents go directly through the broker (same as Traditional).
          (let [cause-id (if (map? cause) (:event/id cause) cause)]
            (dispatch-intent! executor pin cause-id intent outputs))
          ;; Model-call true loop: pin stable, one prepare/dispatch, then code vs tool branch.
          (let [rounds (get-in intent [:payload :options :max-tool-rounds]
                               max-tool-rounds-default)
                base-call (base-call-from-intent* intent)
                initial-tools (or (:requested-tools base-call)
                                  (get-in intent [:payload :tools]) [])
                live-catalog (try (when-let [a (get-in executor [:stores :tool-catalog])]
                                     (when (instance? clojure.lang.Atom a) @a))
                                  (catch Throwable _ nil))
                pinned-source (or live-catalog initial-tools)
                _ (when-let [provider-registry (:registry (:dispatch executor))]
                    (let [filtered (if (sequential? pinned-source)
                                     (remove #(= code-execution-name (:name %)) pinned-source)
                                     pinned-source)]
                      (when (seq filtered)
                        (registry/resolve-tool-catalog provider-registry filtered))))
                ptc (:ptc executor)
                pinned-surface (try (tool-surface/pin pinned-source ptc)
                                    (catch Throwable _
                                      (tool-surface/pin initial-tools ptc)))
                pinned (:surface/binding pinned-surface)
                ;; Single-round loop for CodeMode: we still loop for traditional fallback,
                ;; but code execution is terminal (no further rounds).
                ]
            (loop [current-base-call base-call
                   current-intent intent
                   cause cause
                   outputs outputs
                   rounds rounds
                   pinned pinned
                   pinned-surface pinned-surface]
              (let [cause-id (if (map? cause) (:event/id cause) cause)
                    bindings (fetch-bindings executor pin cause-id)
                    cas (:cas (:stores executor))
                    _refreshed (try (tool-surface/refresh-context pinned-surface bindings cas
                                                                  {:catalog {} :history ""})
                                    (catch Throwable _ nil))
                    prepared (try
                               (assembler/assemble current-base-call
                                                   {:session-bindings bindings
                                                    :tool-catalog/binding pinned
                                                    :cas cas
                                                    :history ""
                                                    :ptc ptc})
                               (catch Throwable _ nil))
                    tool-map (if prepared (:tool-map prepared) (tool-map-of current-intent))
                    effective-intent (if prepared
                                       (-> current-intent
                                           (assoc-in [:payload :messages] (:messages prepared))
                                           (assoc-in [:payload :tools] (:tools prepared))
                                           (assoc-in [:payload :base/messages] (:base/messages current-base-call))
                                           (assoc-in [:payload :requested-tools] (:requested-tools current-base-call)))
                                       current-intent)
                    step (dispatch-intent! executor pin cause-id effective-intent outputs)
                    value (peek (:outputs step))
                    tool-calls (when (= :intent/model-call (:intent/type current-intent))
                                 (:tool-calls value))
                    code (extract-code value)]
                (if code
                  ;; ---------- Code path: SandboxExecute with toolFns ----------
                  (let [raw-source (:source code)
                        ;; language defaults to :sci-clojure already; source string materialized via EDN boundary
                        safe-source (boundary/materialize-edn raw-source {:max-depth 64 :max-size 100000})
                        source-str (if (string? safe-source) safe-source (str safe-source))
                        filtered (filtered-tool-map tool-map)
                        ;; cause for toolFns is the completed provider event
                        cause-for-tools (:last-event step)
                        tool-fns (make-tool-fns filtered executor pin cause-for-tools)
                        ;; Single limitsCheck is inside execute-code (codeBytes, toolCalls pre-check + wall/steps via interrupt)
                        ;; Interrupt is uncatchable inside SCI; execute-code surfaces as :error.
                        result (computation/execute-code computation source-str tool-fns)
                        code-call (find-code-call tool-calls)
                        call-id (or (:tool/call-id code-call) "code_execution")
                        ;; Materialized tool result message with attribution
                        content (if (= :ok (:status result))
                                  (pr-str (boundary/materialize-edn (:value result) {:max-depth 64 :max-size 100000}))
                                  (pr-str (boundary/materialize-edn (:error result) {:max-depth 64 :max-size 100000})))
                        tool-msg {:role :tool
                                  :tool-call-id call-id
                                  :content content
                                  :code/status (:status result)}
                        assistant-msg {:role :assistant
                                       :content (get-in value [:model/output :text] "")
                                       :tool-calls (when (seq tool-calls) (dialect/tool-calls->wire tool-calls))}
                        prepared-msgs (if prepared (:messages prepared)
                                        (get-in effective-intent [:payload :messages]))
                        next-messages (into (vec (or prepared-msgs []))
                                            (if (seq tool-calls)
                                              [assistant-msg tool-msg]
                                              [tool-msg]))
                        ;; Attribution: outputs get the sandbox value/error (materialized EDN already)
                        next-output (if (= :ok (:status result))
                                      (:value result)
                                      (:error result))
                        next-outputs (conj (:outputs step) next-output)
                        outcome (if (= :ok (:status result)) :ok :failed)]
                    {:last-event (:last-event step)
                     :outputs next-outputs
                     :outcome outcome
                     :messages next-messages
                     :code-result result
                     :tool-result tool-msg})
                  ;; ---------- No code: fallback to Traditional tool handling ----------
                  (if (and (seq tool-calls) (pos? rounds) (seq tool-map))
                    (let [calls (mapv (fn [tc]
                                        (if-let [t (get tool-map (:tool/name tc))]
                                          {:call tc :tool-id (:tool t)}
                                          (throw (err/error :scheduler/unknown-tool
                                                            (str "model requested unknown tool "
                                                                 (:tool/name tc))
                                                            {:tool/name (:tool/name tc)}))))
                                      tool-calls)
                          executed (execute-tool-calls! executor pin (:last-event step) effective-intent calls)
                          assistant-msg {:role :assistant
                                         :content (get-in value [:model/output :text] "")
                                         :tool-calls (dialect/tool-calls->wire tool-calls)}
                          prepared-msgs (if prepared (:messages prepared)
                                          (get-in effective-intent [:payload :messages]))
                          next-messages (into (vec prepared-msgs)
                                              (cons assistant-msg (:tool-msgs executed)))
                          next-base-call (assoc current-base-call
                                                :base/messages next-messages
                                                :messages next-messages)]
                      (recur next-base-call
                             (assoc effective-intent :payload (assoc (:payload effective-intent) :messages next-messages))
                             (:last-event executed)
                             (:outputs executed)
                             (dec rounds)
                             pinned
                             pinned-surface))
                    step)))))))))
)
