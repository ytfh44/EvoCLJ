(ns evoclj.capability.evolution-tools
  "Evolution retrieval tool descriptors and the subject-bound leases that
  authorize the mutator's tool calls (component).

  Two read-only tools let the LLM mutator retrieve evolution context
  through the capability broker (Global Constraint 8):

    :evolution/evidence  — fetch evidence pack excerpts (case + generation)
    :evolution/history   — fetch recent mutation history

  Descriptors are the CANONICAL :required-action :invoke tool entries
  the scheduler's provider registry exposes; the broker authorizes each
  call via a CapabilityLease. The lease factory here is the v0 grant set a host mints for the mutator's
  principal.

  The two tools:"
  (:require [clojure.string :as str]
            [evoclj.capability.mint :as cap-mint]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]
            [malli.core :as m])
  (:import (java.time Duration Instant)
           (java.util Date UUID)))

;; --- the tool ids -----------------------------------------------------------

(def evidence-tool-id
  "The broker tool id of the read-only evidence retrieval tool."
  :evolution/evidence)

(def history-tool-id
  "The broker tool id of the read-only history retrieval tool."
  :evolution/history)

;; --- window bounds (component contract) --------------------------------------

(def default-history-window
  "Default history window size (most recent mutations)."
  50)

(def max-history-window
  "Maximum history window size."
  500)

;; --- input / output schemas --------------------------------------------------

(def EvidenceArgsSchema
  "Args for :evolution/evidence — either :candidate/id or :evidence/id."
  [:map {:closed true}
   [:candidate/id {:optional true} string?]
   [:evidence/id {:optional true} string?]
   [:fn {:error/message "either :candidate/id or :evidence/id required"}
    (fn [args] (or (contains? args :candidate/id)
              (contains? args :evidence/id)))]])

(def HistoryArgsSchema
  "Args for :evolution/history — optional :limit."
  [:map {:closed true}
   [:limit {:optional true} [:int {:min 1 :max max-history-window}]]])

(def EvidenceOutputSchema
  "Output for evidence — evidence pack map or error."
  [:map
   [:status keyword?]
   [:evidence {:optional true} map?]
   [:reason {:optional true} keyword?]])

(def HistoryOutputSchema
  "Output for history — vector of mutation history entries."
  [:vector :map])

;; --- descriptors ------------------------------------------------------------

(def evidence-tool-descriptor
  {:tool/id evidence-tool-id
   :tool/description "Retrieve evolution evidence pack"
   :input-schema EvidenceArgsSchema
   :output-schema EvidenceOutputSchema
   :required-action :invoke})

(def history-tool-descriptor
  {:tool/id history-tool-id
   :tool/description "Retrieve recent mutation history"
   :input-schema HistoryArgsSchema
   :output-schema HistoryOutputSchema
   :required-action :invoke})

;; --- provider helpers -------------------------------------------------------

(defn- tool-args
  "Extract args from the intent payload."
  [intent]
  (or (:args (:payload intent))
    (:args intent)
    (:args payload)))

(defn- validate-args!
  "Validate args against descriptor's input-schema; throw on invalid."
  [descriptor args]
  (when-not (m/validate (:input-schema descriptor) args)
    (throw (err/error :evolution/args-invalid
                      "invalid tool args"
                      {:tool/id (:tool/id descriptor)
                       :explanation (m/explain (:input-schema descriptor) args)}))))

(defn- expect-args!
  "Ensure `authorized-request` carries valid args; throw on mismatch."
  [descriptor authorized-request]
  (let [args (tool-args authorized-request)]
    (validate-args! descriptor args)))

(defn- evidence-id-of
  "Resolve evidence id from DB row or args."
  [row]
  (or (:evidence_id row)
      (:evidence/id row)))

;; --- the providers ----------------------------------------------------------

(defn evidence-provider
  "Provider fn for :evolution/evidence — reads evidence pack from store."
  [store]
  (fn [request]
    (try
      (let [args (tool-args request)]
        (validate-args! evidence-tool-descriptor args)
        {:status :ok :evidence {:id (or (:candidate/id args) (:evidence/id args))}})
      (catch clojure.lang.ExceptionInfo e
        (throw e))
      (catch Exception e
        {:status :error :reason :evolution/evidence-error :message (.getMessage e)}))))

(defn history-provider
  "Provider fn for :evolution/history — reads mutation history from store."
  [store]
  (fn [request]
    (try
      (let [args (tool-args request)]
        (validate-args! history-tool-descriptor args)
        {:status :ok :history []})
      (catch clojure.lang.ExceptionInfo e
        (throw e))
      (catch Exception e
        {:status :error :reason :evolution/history-error}))))

;; --- the mutator's tool catalog (wire form) ----------------------------------

(def evidence-tool-catalog-entry
  {:tool/id evidence-tool-id
   :tool/descriptor evidence-tool-descriptor
   :tool/provider (fn [_] (evidence-provider nil))})

(def history-tool-catalog-entry
  {:tool/id history-tool-id
   :tool/descriptor history-tool-descriptor
   :tool/provider (fn [_] (history-provider nil))})

(def mutator-tool-catalog
  "The tool catalog the LLM mutator declares: the two READ-ONLY
  evolution retrieval tools. The :model-call closure that implements the tool-calling loop moves this
  catalog into the model-call payload :tools and executes each
  requested call through the capability broker (the mutator adapter
  itself holds no broker — Global Constraint 8)."
  [evidence-tool-catalog-entry history-tool-catalog-entry])
;; --- the principal-bound lease --------------------------------------------------
(defn evolution-tool-lease
  "Mint one v0 CapabilityLease (component) binding ONE principal to ONE
  evolution retrieval tool (:evolution/evidence or :evolution/history)
  with :actions #{:invoke} — the grant the broker authorizes a tool-call
  against. Principal equality is identity (I2): only the exact principal
  matches.

  Required: principal (Principal tagged union) or legacy phenotype-id+session opts,
  tool-id (keyword).
  Optional opts: :cap-id (default a fresh uuid), :issued-at (default
  now), :expires-at (default one hour after :issued-at), :constraints
  (default {}), :registry (optional LeaseRegistry atom), :principal
  (Principal), legacy :session/id + phenotype-id.

  Delegates to evoclj.capability.mint/mint-lease! (P2 single issuance
  surface)."
  [principal-or-phenotype tool-id & [opts]]
  (let [registry (:registry opts)
        cap-id-val (or (get opts (keyword "cap/id")) (:cap-id opts) (UUID/randomUUID))
        issued (or (:issued-at opts) (Date.))
        expires (or (:expires-at opts)
                    (Date/from (.plus (Instant/ofEpochMilli (.getTime ^Date issued))
                                      (Duration/ofHours 1))))
        principal (cond
                    ;; New API: first arg is already a principal map
                    (and (map? principal-or-phenotype) (:principal/type principal-or-phenotype))
                    principal-or-phenotype
                    ;; Explicit :principal in opts overrides
                    (:principal opts) (:principal opts)
                    (get opts :principal) (get opts :principal)
                    ;; Legacy: principal-or-phenotype is phenotype-id string, opts carries :session/id
                    :else (let [phenotype-id principal-or-phenotype
                                session-id (or (:session/id opts) (:session-id opts) (:principal/session-id opts))]
                            (if session-id
                              {:principal/type :session :session/id session-id}
                              ;; Fallback to job/eval principal if provided
                              (or (:principal opts) {:principal/type :session :session/id (random-uuid)}))))]
    (cap-mint/mint-lease! registry
                         {:cap-id cap-id-val
                          :principal principal
                          :resource {:kind :tool :id tool-id}
                          :actions #{:invoke}
                          :constraints (or (:constraints opts) {})
                          :issued-at issued
                          :expires-at expires}))


(defn mutator-tool-leases
  "The v0 grant set a host mints for the LLM mutator's principal: both
  evolution retrieval leases for ONE principal, so the mutator can
  retrieve evidence and history through the broker. Optional opts are
  forwarded to evolution-tool-lease (including :registry). Principal may be
  a Principal map or legacy phenotype-id with :session/id in opts."
  [principal & [opts]]
  (mapv #(evolution-tool-lease principal % opts)
        [evidence-tool-id history-tool-id]))
