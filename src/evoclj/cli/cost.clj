(ns evoclj.cli.cost
  "The cost-report CLI command (feature O2): `evoclj cost`.

  Aggregates the REAL model usage of one generation from its causal
  event log: every :provider/call-completed event whose payload (a
  CAS artifact) is a model-call result value carries :usage
  {:model-input-tokens n :model-output-tokens n} and/or
  :model-cost-units — the component counters (evoclj.runtime.usage).
  Tool-call results (fixture echo etc.) carry no model counters and
  are skipped.

  The command is READ-ONLY: it reads the events table (joined to the
  generation's sessions) and the CAS artifacts; it writes nothing.
  Failures to read one artifact are collected, not thrown (evidence)."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.cli.session :as session]
            [evoclj.kernel.error :as err]
            [evoclj.runtime.usage :as usage]
            [evoclj.store.cas :as cas]
            [evoclj.store.sqlite :as sqlite]))

(defn- required-opt
  [opts k usage]
  (or (get-in opts [:options k])
      (throw (err/error :cli/usage-invalid
                        (str "missing required option --" (name k))
                        {:usage usage}))))

(defn- generation-events
  "Every :provider/call-completed event row of the generation's
  sessions (the join keeps the bound generation-local)."
  [db generation-id]
  (sqlite/query db
                ["SELECT e.id AS event_id, e.payload_ref
                  FROM events e
                  JOIN sessions s ON s.id = e.session_id
                  WHERE s.generation_id = ? AND e.event_type = ':provider/call-completed'
                 " generation-id]))

(defn- payload-usage
  "The model usage sample from one call-completed payload artifact
  (the provider result value), or nil when the artifact carries no
  model counters. A missing/unreadable artifact yields {:error ...}
  so the report can surface it without throwing."
  [cas-store payload-ref]
  (when payload-ref
    (try
      (let [v (edn/read-string
               (String. ^bytes (cas/get-bytes cas-store payload-ref)
                        java.nio.charset.StandardCharsets/UTF_8))]
        (when (map? v)
          (let [u (:usage v)
                input (get-in u [:model-input-tokens])
                output (get-in u [:model-output-tokens])
                cost (or (:model-cost-units u)
                         (:model-cost-units v)
                         (:provider-reported-cost v))]
            (when (or input output cost)
              (cond-> {}
                input (assoc :model-input-tokens input)
                output (assoc :model-output-tokens output)
                cost (assoc :model-cost-units (double cost)))))))
      (catch Throwable t
        {:error {:event/payload-ref payload-ref
                 :error/type (some-> (ex-data t) :error/type)
                 :message (.getMessage t)}}))))

(defn cost-report!
  "evoclj cost --generation <id|current>

  The model usage of one generation, aggregated from its causal event
  log (component counters). Returns plain EDN-safe data:

    {:generation/id <id>
     :sessions n
     :model-calls n              ; call-completed events with model usage
     :usage {:model-input-tokens n
             :model-output-tokens n
             :model-cost-units n}
     :artifact-errors [<maps>]}  ; unreadable payload refs, if any"
  [opts]
  (let [generation (required-opt opts :generation "evoclj cost --generation <id|current>")
        system (session/build-system opts)
        db (session/db-of system)
        cas-store (session/cas-of system)
        generation-id (if (= generation "current")
                        (if-let [cg (session/current-generation-info system)]
                          (:generation/id cg)
                          (throw (err/error :cli/generation-not-found
                                            "no CURRENT generation to report"
                                            {})))
                        generation)
        events (generation-events db generation-id)
        session-count (first (sqlite/query db
                                         ["SELECT COUNT(*) AS n FROM sessions
                                           WHERE generation_id = ?"
                                          generation-id]))
        samples (keep (fn [e]
                       (payload-usage cas-store (:payload_ref e)))
                     events)
        errors (vec (keep :error samples))
        model-samples (remove :error samples)
        total (usage/aggregate model-samples)]
    {:generation/id generation-id
     :sessions (or (:n session-count) 0)
     :model-calls (count model-samples)
     :usage {:model-input-tokens (long (or (:model-input-tokens total) 0))
             :model-output-tokens (long (or (:model-output-tokens total) 0))
             :model-cost-units (double (or (:model-cost-units total) 0.0))}
     :artifact-errors errors}))
