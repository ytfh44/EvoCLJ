(ns evoclj.store.invariant
  "Narrow durable boundary for generated-invariant evidence and decisions.

  CAS is written and verified before any dependent row. Core rows are
  append-only; activation, its event, and outbox are one BEGIN IMMEDIATE
  transaction. This namespace never evaluates generated code."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.eval.static :as static]
            [evoclj.evolution.invariant :as invariant]
            [evoclj.kernel.error :as err]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.existence :as existence]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.support.failpoint :as fault])
  (:import (java.nio.charset StandardCharsets)
           (java.time Instant)
           (java.util UUID)))

(defn- now [] (str (Instant/now)))
(defn- id [] (str (UUID/randomUUID)))
(declare verify-durable-activation! get-proposal current-state list-runs
         read-edn decode-artifact! verify-decision-row!
         verify-approved-decision! verify-disable-row! verify-event-outbox!
         verify-activation! verify-target! verify-run-row!)

(defn- stores [store]
  (when-not (map? store) (throw (err/error :invariant/store-invalid "store must be {:sqlite ... :cas ...}" {:reason :not-a-map})))
  (when-not (contains? store :sqlite) (throw (err/error :invariant/store-invalid "store is missing :sqlite" {:reason :sqlite-missing})))
  (when-not (contains? store :cas) (throw (err/error :invariant/store-invalid "store is missing :cas" {:reason :cas-missing})))
  (static/install-durable-verifier! #(verify-durable-activation! store %1 %2))
  store)
(defn- encoded-bytes [x] (.getBytes (pr-str x) StandardCharsets/UTF_8))
(defn- verifying-cas [store]
  (let [c (:cas store)]
    (cas/->cas (if (map? c) (:root c) c) {:verify true})))
(defn- cas-put! [store value media-type]
  (let [{:keys [cas]} (stores store)
        out (cas/put-bytes! cas (encoded-bytes value) {:media-type media-type})
        aid (:artifact/id out)]
    (artifact/ensure-artifact! (:sqlite store) aid media-type (:size out))
    aid))
(defn- ensure-ref! [store ref]
  "Every persisted external reference must arrive as a sealed existence proof and
  be rehashed before it is accepted."
  (let [vd (existence/ensure-proof ref)
        digest (existence/digest-of vd)]
    (cas/get-bytes (verifying-cas store) digest)
    digest))
(defn- decode-artifact!
  [store ref context]
  (when-not (invariant/digest? ref)
    (throw (err/error :invariant/artifact-invalid
                      "referenced artifact id is not canonical"
                      (assoc context :artifact/id ref))))
  (let [bytes (cas/get-bytes (verifying-cas store) ref)
        value (read-edn (String. bytes StandardCharsets/UTF_8))]
    (when-not (and (some? value) (invariant/safe-edn-value? value 0))
      (throw (err/error :invariant/artifact-invalid
                        "referenced artifact is not closed EDN"
                        (assoc context :artifact/id ref))))
    value))

(defn- verify-durable-activation!
  [store descriptor _proof]
  (let [aid (:activation/id descriptor)
        digest (:activation/digest descriptor)
        row (first (sqlite/query (:sqlite store)
                                 ["SELECT * FROM invariant_activations WHERE id = ? AND status = 'active'"
                                  (str aid)]))]
    (when-not row
      (throw (err/error :invariant/activation-invalid
                        "active activation row is missing"
                        {:activation/id aid})))
    (verify-activation! store row)
    (when-not (and (= (str (:proposal_id row)) (str (:proposal/id descriptor)))
                   (= (:version row) (:version descriptor))
                   (= (:predicate_digest row) (:predicate/digest (:predicate descriptor)))
                   (= (:registry_revision row) (:registry/revision descriptor))
                   (= (:activation_digest row) digest)
                   (nil? (first (sqlite/query (:sqlite store)
                                              ["SELECT id FROM invariant_events WHERE activation_id = ? AND event_type = 'quarantined'"
                                               (str aid)]))))
      (throw (err/error :invariant/activation-invalid
                        "durable activation facts do not match publication"
                        {:activation/id aid})))
    true))

(defn- json [x] (pr-str x))
(defn- read-edn [s]
  (try (edn/read-string s) (catch Exception _ nil)))
(defn- verify-proposal-row! [store r]
  (let [proposal (decode-artifact! store (:proposal_digest r) {:table :invariant_proposals :column :proposal_digest :proposal/id (:id r)})
        predicate (decode-artifact! store (:predicate_digest r) {:table :invariant_proposals :column :predicate_digest :proposal/id (:id r)})
        predicate-json (read-edn (:predicate_json r))
        evidence-refs (read-edn (:evidence_refs r))
        replay-refs (read-edn (:replay_refs r))
        adversarial-refs (read-edn (:adversarial_refs r))]
    (when-not (and (map? proposal) (map? predicate)
                   (every? #(and (vector? %) (every? invariant/digest? %))
                           [evidence-refs replay-refs adversarial-refs]))
      (throw (err/error :invariant/proposal-invalid
                        "proposal or reference columns are malformed"
                        {:proposal/id (:id r)})))
    (doseq [[column refs] [[:evidence_refs evidence-refs]
                           [:replay_refs replay-refs]
                           [:adversarial_refs adversarial-refs]]
            ref refs]
      (decode-artifact! store ref {:table :invariant_proposals :column column :proposal/id (:id r)}))
    (when-not (invariant/validate-predicate! predicate-json)
      (throw (err/error :invariant/proposal-invalid
                        "proposal predicate is invalid"
                        {:proposal/id (:id r)})))
    (when-not (and (= (:proposal/id proposal) (:id r))
                   (= (:proposer proposal) (:proposer r))
                   (= (:reviewer proposal) (:reviewer r))
                   (= (:scope proposal) (keyword (:scope r)))
                   (= (:risk proposal) (keyword (:risk r)))
                   (= (:version proposal) (:version r))
                   (= (:registry/revision proposal) (:registry_revision r))
                   (= (:predicate proposal) predicate-json)
                   (= (:evidence/refs proposal) evidence-refs)
                   (= (:replay/refs proposal) replay-refs)
                   (= (:adversarial/refs proposal) adversarial-refs)
                   (= (:predicate/digest proposal) (:predicate_digest r))
                   (= (:predicate/digest predicate-json) (:predicate_digest r))
                   (= (dissoc predicate-json :predicate/digest) predicate)
                   (or (nil? (:proposal_json r))
                       (= proposal (read-edn (:proposal_json r)))))
      (throw (err/error :invariant/proposal-invalid
                        "proposal and predicate CAS artifacts do not match durable SQL row"
                        {:proposal/id (:id r)})))
    true))
(defn- row->proposal [store r]
  (when r
    (let [p (invariant/proposal
             {:proposal/id (:id r)
              :proposer (:proposer r)
              :reviewer (:reviewer r)
              :scope (keyword (:scope r))
              :risk (keyword (:risk r))
              :version (:version r)
              :registry/revision (:registry_revision r)
              :predicate (read-edn (:predicate_json r))
              :evidence/refs (read-edn (:evidence_refs r))
              :replay/refs (read-edn (:replay_refs r))
              :adversarial/refs (read-edn (:adversarial_refs r))})]
      (assoc p :proposal/id (:id r)
               :proposal/digest (:proposal_digest r)
               :predicate/digest (:predicate_digest r)
               :status (or (current-state store (:id r)) :proposed)))))

(defn get-proposal [store proposal-id]
  (stores store)
  (when-let [r (first (sqlite/query (:sqlite store) ["SELECT * FROM invariant_proposals WHERE id = ?" (str proposal-id)]))]
    (verify-proposal-row! store r)
    (doseq [d (sqlite/query (:sqlite store) ["SELECT * FROM invariant_decisions WHERE proposal_id = ?" (str proposal-id)])]
      (verify-decision-row! store d))
    (doseq [run (sqlite/query (:sqlite store) ["SELECT * FROM invariant_runs WHERE proposal_id = ?" (str proposal-id)])]
      (verify-run-row! store run))
    (row->proposal store r)))
(defn- verify-decision-bindings! [store row]
  (let [decoded (decode-artifact! store (:decision_digest row)
                                  {:table :invariant_decisions :column :decision_digest :decision/id (:id row)})
        expected-status (keyword (:decision row))]
    (when-not (and (= (str (:proposal/id decoded)) (str (:proposal_id row)))
                   (= (str (:reviewer decoded)) (str (:reviewer row)))
                   (= (:status decoded) expected-status)
                   (= (boolean (:activation-qualified? decoded)) (= 1 (:activation_qualified row)))
                   (= (some-> (:reason decoded) str) (some-> (:reason row) str)))
      (throw (err/error :invariant/decision-invalid
                        "decision CAS artifact does not match durable SQL row"
                        {:decision/id (:id row)})))
    decoded))

(defn verify-approved-decision!
  "Verify an approved decision against its proposal and every durable run.
  Recomputes approval qualification from exact CAS-selected evidence; no
  decision CAS field or SQL qualification flag is trusted on its own."
  [store row]
  (stores store)
  (let [decoded (verify-decision-bindings! store row)
        proposal-row (first (sqlite/query (:sqlite store)
                                          ["SELECT * FROM invariant_proposals WHERE id = ?"
                                           (str (:proposal_id row))]))]
    (when-not (= :approved (:status decoded))
      (throw (err/error :invariant/decision-invalid
                        "approved decision CAS artifact is not approved"
                        {:decision/id (:id row)})))
    (when-not proposal-row
      (throw (err/error :invariant/proposal-missing
                        "approved decision proposal row is missing"
                        {:proposal/id (:proposal_id row)})))
    (verify-proposal-row! store proposal-row)
    (let [proposal (row->proposal store proposal-row)
          runs (list-runs store (:proposal_id row))
          selected (select-keys decoded [:reviewer :replay/ref :adversarial/ref])
          recomputed (invariant/approval proposal selected runs)
          selected-runs [(some #(when (and (= :replay (:kind %))
                                             (= (:replay/ref decoded) (:result-ref %))) %)
                               runs)
                         (some #(when (and (= :adversarial (:kind %))
                                             (= (:adversarial/ref decoded) (:result-ref %))) %)
                               runs)]]
      (when-not (and (= (str (:proposal/id decoded)) (str (:proposal/id proposal)))
                     (= (:proposal/id recomputed) (:proposal/id decoded))
                     (= (:replay/ref recomputed) (:replay/ref decoded))
                     (= (:adversarial/ref recomputed) (:adversarial/ref decoded))
                     (= (:status decoded) :approved)
                     (true? (:activation-qualified? decoded))
                     (= :recorded-only (get-in (first selected-runs) [:result :model/policy]))
                     (= :recorded-only (get-in (second selected-runs) [:result :model/policy]))
                     (= :G3-deterministic-suites (get-in (first selected-runs) [:result :gate/id]))
                     (= :G3-deterministic-suites (get-in (second selected-runs) [:result :gate/id]))
                     (not-any? #(= :counterexample (:kind %)) runs))
        (throw (err/error :invariant/decision-invalid
                          "approved decision evidence is not activation-qualified"
                          {:decision/id (:id row) :proposal/id (:proposal_id row)})))
      decoded)))

(defn- verify-decision-row! [store row]
  (let [decoded (verify-decision-bindings! store row)]
    (when (= :approved (:status decoded))
      (verify-approved-decision! store row))
    decoded))

(defn- verify-event-outbox! [store activation-id event-type payload-ref]
  (let [events (sqlite/query (:sqlite store)
                             ["SELECT * FROM invariant_events WHERE activation_id = ? AND event_type = ?"
                              (str activation-id) (name event-type)])
        event (first events)
        outbox (when event
                 (sqlite/query (:sqlite store)
                               ["SELECT * FROM invariant_outbox WHERE activation_id = ? AND event_id = ? AND event_type = ?"
                                (str activation-id) (:id event) (name event-type)]))]
    (when-not (and (= 1 (count events)) (= 1 (count outbox))
                   (= (str payload-ref) (str (:payload_ref event)))
                   (= (str activation-id) (str (:activation_id (first outbox))))
                   (= (:id event) (:event_id (first outbox)))
                   (= (name event-type) (:event_type (first outbox))))
      (throw (err/error :invariant/event-invalid
                        "event and outbox rows do not match durable activation evidence"
                        {:activation/id activation-id :event/type event-type})))
    event))

(defn verify-disable-row! [store row]
  (let [decoded (decode-artifact! store (:disable_digest row)
                                  {:table :invariant_disables :column :disable_digest :proposal/id (:proposal_id row)})
         activations (sqlite/query (:sqlite store)
                                   ["SELECT * FROM invariant_activations WHERE proposal_id = ?"
                                    (str (:proposal_id row))])]
    (when-not (and (= (str (:proposal/id decoded)) (str (:proposal_id row)))
                   (= (str (:reviewer decoded)) (str (:reviewer row)))
                   (= (str (:reason decoded)) (str (:reason row)))
                   (= :disabled (:status decoded)))
      (throw (err/error :invariant/disable-invalid
                        "disable CAS artifact does not match durable SQL row"
                        {:proposal/id (:proposal_id row)})))
    (when-not (= 1 (count activations))
      (throw (err/error :invariant/disable-invalid
                        "disable row does not have exactly one current activation"
                        {:proposal/id (:proposal_id row)})))
    (verify-event-outbox! store (:id (first activations)) :disabled (:disable_digest row))
    decoded))

(defn verify-activation! [store row]
  "Verify one activation row and all of its durable evidence bindings."
  (when-not (= "active" (:status row))
    (throw (err/error :invariant/activation-invalid
                      "only active activation rows may be verified for publication"
                      {:activation/id (:id row) :status (:status row)})))
  (let [p (get-proposal store (:proposal_id row))
        decision (first (sqlite/query (:sqlite store)
                                      ["SELECT * FROM invariant_decisions WHERE id = ? AND proposal_id = ?"
                                       (:decision_id row) (str (:proposal_id row))]))
        activation (decode-artifact! store (:activation_digest row)
                                     {:table :invariant_activations :column :activation_digest :activation/id (:id row)})
        disable-rows (sqlite/query (:sqlite store)
                                   ["SELECT * FROM invariant_disables WHERE proposal_id = ?"
                                    (str (:proposal_id row))])
        disabled-events (sqlite/query (:sqlite store)
                                      ["SELECT * FROM invariant_events WHERE activation_id = ? AND event_type = 'disabled'"
                                       (str (:id row))])
        disabled-outbox (sqlite/query (:sqlite store)
                                      ["SELECT * FROM invariant_outbox WHERE activation_id = ? AND event_type = 'disabled'"
                                       (str (:id row))])
        quarantine-events (sqlite/query (:sqlite store)
                                        ["SELECT * FROM invariant_events WHERE activation_id = ? AND event_type = 'quarantined'"
                                         (str (:id row))])
        quarantine-outbox (sqlite/query (:sqlite store)
                                        ["SELECT * FROM invariant_outbox WHERE activation_id = ? AND event_type = 'quarantined'"
                                         (str (:id row))])]
    (when-not (and p decision
                   (= (:status p) (current-state store (:proposal_id row)))
                   (= 1 (:activation_qualified decision))
                   (= (:proposal_id row) (:proposal/id p))
                   (= (:version row) (:version p))
                   (= (:predicate_digest row) (:predicate/digest (:predicate p)))
                   (= (:registry_revision row) (:registry/revision p))
                   (= (str (:activation/id activation)) (str (:id row)))
                   (= (:proposal/id activation) (:proposal_id row))
                   (= (:version activation) (:version row))
                   (= (:predicate activation) (:predicate p))
                   (= (:registry/revision activation) (:registry_revision row))
                   (= (:decision/id activation) (:decision_id row)))
      (throw (err/error :invariant/activation-invalid
                        "activation CAS artifact does not match durable SQL row"
                        {:activation/id (:id row)})))
    (when (or (seq disabled-events) (seq disabled-outbox))
      (when-not (= 1 (count disable-rows))
        (throw (err/error :invariant/disable-invalid
                          "disabled event/outbox requires exactly one durable disable decision"
                          {:activation/id (:id row) :proposal/id (:proposal_id row)})))
      (when (or (seq quarantine-events) (seq quarantine-outbox))
        (throw (err/error :invariant/terminal-conflict
                          "disabled and quarantined terminal evidence cannot coexist"
                          {:activation/id (:id row)})))
      (verify-disable-row! store (first disable-rows)))
    (when (and (seq disable-rows) (empty? disabled-events))
      ;; The disable row itself must carry a matching disabled event and outbox.
      (verify-disable-row! store (first disable-rows)))
    (verify-approved-decision! store decision)
    (verify-event-outbox! store (:id row) :activated (:activation_digest row))
    (when (or (seq quarantine-events) (seq quarantine-outbox))
      (when (or (seq disabled-events) (seq disabled-outbox))
        (throw (err/error :invariant/terminal-conflict
                          "disabled and quarantined terminal evidence cannot coexist"
                          {:activation/id (:id row)})))
      (verify-event-outbox! store (:id row) :quarantined (:activation_digest row)))
    true))
(defn list-proposals [store]
  (stores store)
  (mapv (fn [r]
          (let [p (get-proposal store (:id r))]
            (list-runs store (:id r))
            p))
        (sqlite/query (:sqlite store) ["SELECT * FROM invariant_proposals ORDER BY created_at, id"])))

(defn propose!
  "Persist a normalized proposal as immutable evidence. The proposal and its
  predicate are put in CAS before the foreign-keyed proposal row."
  [store p]
  (let [p (reduce (fn [p k]
                    (update p k #(mapv (partial ensure-ref! store) (or % []))))
                  p [:evidence/refs :replay/refs :adversarial/refs])
        p (invariant/proposal p)
        proposal-id (str (or (:proposal/id p) (:invariant/id p) (id)))
        predicate-id (cas-put! store (dissoc (:predicate p) :predicate/digest) "application/edn")
        predicate (assoc (:predicate p) :predicate/digest predicate-id)
        p (assoc p :proposal/id proposal-id :predicate predicate :predicate/digest predicate-id)
        proposal-id-artifact (cas-put! store p "application/edn")
        refs (concat (:evidence/refs p) (:replay/refs p) (:adversarial/refs p))]
    ;; refs were sealed while normalizing the proposal
    (sqlite/with-write-tx [conn (:sqlite store)]
      (if-let [existing (first (sqlite/query-raw! conn "SELECT * FROM invariant_proposals WHERE id = ?" [proposal-id]))]
        (if (= proposal-id-artifact (:proposal_digest existing))
          (row->proposal store existing)
          (throw (err/error :invariant/idempotency-conflict "proposal id already names different evidence" {:proposal/id proposal-id})))
        (do
          (sqlite/insert-raw! conn
            "INSERT INTO invariant_proposals
             (id,proposer,reviewer,scope,risk,version,registry_revision,predicate_digest,predicate_json,proposal_digest,evidence_refs,replay_refs,adversarial_refs,status,created_at)
             VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
            [proposal-id (str (:proposer p)) (some-> (:reviewer p) str) (name (:scope p)) (name (:risk p)) (:version p)
             (str (:registry/revision p)) predicate-id (json (:predicate p)) proposal-id-artifact (json (:evidence/refs p))
             (json (:replay/refs p)) (json (:adversarial/refs p)) "proposed" (now)])
          (assoc p :proposal/digest proposal-id-artifact))))))

(defn- row->run [r]
  (when r
    (let [decoded (or (:decoded-run r) (read-edn (:run_json r)))]
      (assoc decoded :run/id (:id r) :proposal/id (:proposal_id r)
             :kind (keyword (:kind r)) :result-ref (:result_ref r)
             :run/digest (:run_digest r)))))
(defn- verify-run-row! [store r]
  (let [result-value (decode-artifact! store (:result_ref r) {:table :invariant_runs :column :result_ref :run/id (:id r)})
        decoded (decode-artifact! store (:run_digest r) {:table :invariant_runs :column :run_digest :run/id (:id r)})]
    (when-not (and (map? result-value) (map? decoded))
      (throw (err/error :invariant/run-invalid "run and result artifacts must be maps" {:run/id (:id r)})))
    (when-not (= (:result-ref decoded) (:result_ref r))
      (throw (err/error :invariant/run-invalid
                        "run digest result ref does not match durable SQL row"
                        {:run/id (:id r)})))
    (let [normalized (invariant/run decoded)
          result (verify-target! store (:result normalized))
          detail-refs (distinct (remove nil? [(:details-ref result) (:details/ref result)]))
          sql-values {:run/id (:id r) :proposal/id (:proposal_id r) :kind (keyword (:kind r))
                      :result-ref (:result_ref r) :gate (some-> (:gate r) keyword)
                      :model/policy (some-> (:model_policy r) keyword)
                      :deterministic? (= 1 (:deterministic r)) :fresh-model? (= 1 (:fresh_model r))
                      :passed? (= 1 (:passed r))}]
      (doseq [ref detail-refs]
        (decode-artifact! store ref {:table :invariant_runs :column :details_ref :run/id (:id r)}))
      (when-not (= result-value result)
        (throw (err/error :invariant/run-invalid "run digest result does not match result artifact" {:run/id (:id r)})))
      (when-not (= (json decoded) (:run_json r))
        (throw (err/error :invariant/run-invalid "run_json does not match run digest" {:run/id (:id r)})))
      (doseq [[k expected] sql-values]
        (when-not (= expected (get normalized k))
          (throw (err/error :invariant/run-invalid "run SQL column does not match decoded run" {:run/id (:id r) :key k}))))
      (assoc r :decoded-run normalized))))
(defn list-runs [store proposal-id]
  (stores store)
  (mapv (fn [r] (row->run (verify-run-row! store r)))
        (sqlite/query (:sqlite store) ["SELECT * FROM invariant_runs WHERE proposal_id = ? ORDER BY created_at, id" (str proposal-id)])))
(def ^:private caller-claim-keys
  #{:gate :gate/id :model/policy :deterministic? :fresh-model? :passed?})

(defn- verify-target! [store result]
  (let [target (:target/digest result)
        evaluation-id (:evaluation/id result)
        candidate-id (:candidate/id result)
        v (verifying-cas store)
        evaluation (when evaluation-id
                     (first (sqlite/query (:sqlite store) ["SELECT id,candidate_id FROM eval_runs WHERE id = ?" (str evaluation-id)])))
        candidate (when candidate-id
                   (first (sqlite/query (:sqlite store) ["SELECT id FROM candidates WHERE id = ?" (str candidate-id)])))]
    (when (and target
               (not (invariant/digest? target)))
      (throw (err/error :invariant/target-invalid "target digest must be a canonical CAS artifact id" {})))
    (when target
      (let [bytes (cas/get-bytes v target)
            value (read-edn (String. bytes StandardCharsets/UTF_8))]
        (when-not (and (some? value) (invariant/safe-edn-value? value 0))
          (throw (err/error :invariant/target-invalid "target artifact is not closed EDN" {})))))
    (when (and evaluation-id (nil? evaluation))
      (throw (err/error :invariant/target-invalid "evaluation identity does not name a durable eval run" {:evaluation/id evaluation-id})))
    (when (and candidate-id (nil? candidate))
      (throw (err/error :invariant/target-invalid "candidate identity does not name a durable candidate" {:candidate/id candidate-id})))
    (when (and evaluation candidate-id (not= (str (:candidate_id evaluation)) (str candidate-id)))
      (throw (err/error :invariant/target-invalid "evaluation and candidate identities are not bound" {})))
    (when-not (or target evaluation candidate)
      (throw (err/error :invariant/target-invalid "result requires a verified target identity" {})))
    result))
(defn- result-artifact [store proposal kind raw]
  (when (contains? raw :result)
    (throw (err/error :invariant/unattested-result
                      "inline result maps cannot authorize durable evidence"
                      {:reason :inline-result-rejected})))
  (when (some #(contains? raw %) caller-claim-keys)
    (throw (err/error :invariant/unattested-result
                      "qualification claims must be inside a verified result artifact"
                      {:reason :caller-only-claim})))
  (when-not (contains? raw :result/ref)
    (throw (err/error :invariant/unattested-result
                      "durable runs require a sealed result artifact reference"
                      {:reason :inline-result-rejected})))
  (let [result-id (ensure-ref! store (:result/ref raw))
        result-bytes (cas/get-bytes (verifying-cas store) result-id)
        decoded (read-edn (String. result-bytes StandardCharsets/UTF_8))]
    (when-not (and (map? decoded) (invariant/safe-edn-value? decoded 0))
      (throw (err/error :invariant/result-invalid "result artifact is not closed EDN" {})))
    (let [result (:result (invariant/run {:run/id "artifact-validation"
                                          :proposal/id (:proposal/id decoded)
                                          :kind kind
                                          :result-ref result-id
                                          :result decoded}))]
      (verify-target! store result)
      (doseq [k [:details-ref :details/ref]]
        (when-let [ref (get result k)]
          (when-not (invariant/digest? ref)
            (throw (err/error :invariant/result-invalid "nested result reference is not a canonical digest" {:key k})))
          (let [details (cas/get-bytes (verifying-cas store) ref)
                value (read-edn (String. details StandardCharsets/UTF_8))]
            (when-not (and (some? value) (invariant/safe-edn-value? value 0))
              (throw (err/error :invariant/result-invalid "details artifact is not closed EDN" {:key k :ref ref}))))))
      [result-id result])))

(defn append-run!
  "Append evidence backed by a closed result artifact. Raw caller booleans or
  arbitrary result refs never authorize a run; result refs must be sealed
  VerifiedDigest proofs."
  [store proposal-id r]
  (stores store)
  (let [p (get-proposal store proposal-id)]
    (when-not p
      (throw (err/error :invariant/proposal-missing "proposal does not exist" {:proposal/id proposal-id})))
    (when-not (map? r) (throw (err/error :invariant/run-invalid "run must be a map" {})))
    (when-not (contains? invariant/run-kinds (:kind r))
      (throw (err/error :invariant/run-kind-invalid "run kind is required" {:kind (:kind r)})))
    (let [[result-id result] (result-artifact store p (:kind r) r)
          _ (when-not (= (str proposal-id) (str (:proposal/id result)))
              (throw (err/error :invariant/result-proposal-mismatch "result envelope names another proposal" {})))
          _ (when-not (= (:predicate/digest (:predicate p)) (:predicate/digest result))
              (throw (err/error :invariant/result-predicate-mismatch "result envelope names another predicate" {})))
          _ (when-not (= (:registry/revision p) (:registry/revision result))
              (throw (err/error :invariant/result-registry-mismatch "result envelope names another registry revision" {})))
          raw (assoc (select-keys r [:run/id :kind]) :proposal/id proposal-id :result result :result-ref result-id)
          run-id (str (or (:run/id raw) (id)))
          r (invariant/run (assoc raw :run/id run-id))
          run-id-artifact (cas-put! store r "application/edn")]
      (sqlite/with-write-tx [conn (:sqlite store)]
        (if-let [existing (first (sqlite/query-raw! conn "SELECT * FROM invariant_runs WHERE id = ?" [run-id]))]
          (if (= run-id-artifact (:run_digest existing))
            (row->run (verify-run-row! store existing))
            (throw (err/error :invariant/idempotency-conflict "run id already names different evidence" {:run/id run-id})))
          (do
            (sqlite/insert-raw! conn
              "INSERT INTO invariant_runs
               (id,proposal_id,kind,result_ref,run_digest,gate,model_policy,deterministic,fresh_model,passed,run_json,created_at)
               VALUES (?,?,?,?,?,?,?,?,?,?,?,?)"
              [run-id (str proposal-id) (name (:kind r)) result-id run-id-artifact (some-> (:gate r) name)
               (some-> (:model/policy r) name) (if (:deterministic? r) 1 0) (if (:fresh-model? r) 1 0)
               (if (:passed? r) 1 0) (json r) (now)])
            (assoc r :run/id run-id :result-ref result-id :run/digest run-id-artifact)))))))


(defn- current-state [store proposal-id]
  (cond
    (seq (sqlite/query (:sqlite store) ["SELECT id FROM invariant_events WHERE activation_id IN (SELECT id FROM invariant_activations WHERE proposal_id = ?) AND event_type = 'quarantined' LIMIT 1" (str proposal-id)]))
    :quarantined
    (seq (sqlite/query (:sqlite store) ["SELECT id FROM invariant_disables WHERE proposal_id = ? LIMIT 1" (str proposal-id)]))
    :disabled
    (seq (sqlite/query (:sqlite store) ["SELECT id FROM invariant_activations WHERE proposal_id = ? AND status = 'active' AND NOT EXISTS (SELECT 1 FROM invariant_events q WHERE q.activation_id = invariant_activations.id AND q.event_type = 'quarantined') LIMIT 1" (str proposal-id)]))
    :active
    :else
    (let [row (first (sqlite/query (:sqlite store) ["SELECT decision FROM invariant_decisions WHERE proposal_id = ? ORDER BY created_at DESC, id DESC LIMIT 1" (str proposal-id)]))
          d (some-> row :decision str)]
      (cond (= d "approved") :approved (= d "rejected") :rejected :else :proposed))))

(defn- decision [store proposal-id decision reviewer opts]
  (let [p (get-proposal store proposal-id)
        runs (list-runs store proposal-id)
        opts (reduce-kv (fn [m k v]
                          (if (#{:replay/ref :adversarial/ref} k)
                            (assoc m k (ensure-ref! store v))
                            (assoc m k v))) {} opts)
        a (assoc opts :reviewer reviewer)]
    (when-not p (throw (err/error :invariant/proposal-missing "proposal does not exist" {:proposal/id proposal-id})))
    (when (= (str reviewer) (str (:proposer p)))
      (throw (err/error :invariant/reviewer-conflict "proposer cannot reject/approve its own proposal" {})))
    (if (= :approved decision)
      (invariant/approval p a runs)
      (assoc a :proposal/id (str proposal-id) :status :rejected :activation-qualified? false))))

(defn- commit-decision!
  "The one durable decision-write path shared by approve! and reject!. `d` is the
  validated decision map, `label` the :invariant_decisions.decision literal,
  `did` the row id, `qualified?` the activation_qualified flag, and
  `on-existing` maps a pre-existing row to the caller's own idempotent return.
  A repeat whose digest matches the stored decision is idempotent; any other
  decision on an already-decided proposal fails closed as
  :invariant/decision-conflict."
  [store proposal-id reviewer d label did qualified? on-existing]
  (let [digest (cas-put! store d "application/edn")]
    (sqlite/with-write-tx [conn (:sqlite store)]
      (if-let [existing (first (sqlite/query-raw! conn "SELECT * FROM invariant_decisions WHERE proposal_id = ? LIMIT 1" [(str proposal-id)]))]
        (if (= digest (:decision_digest existing))
          (on-existing existing)
          (throw (err/error :invariant/decision-conflict "proposal already has a terminal decision" {:proposal/id proposal-id :decision (:decision existing)})))
        (do
          (sqlite/insert-raw! conn
            "INSERT INTO invariant_decisions (id,proposal_id,decision,reviewer,decision_digest,activation_qualified,reason,created_at) VALUES (?,?,?,?,?,?,?,?)"
            [did (str proposal-id) label (str reviewer) digest qualified? (:reason d) (now)])
          (assoc d :decision/id did :decision/digest digest))))))

(defn approve!
  "Append a reviewer approval. Approval never mutates a proposal row."
  [store proposal-id reviewer opts]
  (stores store)
  (let [d (decision store proposal-id :approved reviewer opts)
        did (str (or (:decision/id d) (id)))]
    (commit-decision! store proposal-id reviewer d "approved" did
                      (if (:activation-qualified? d) 1 0)
                      identity)))

(defn reject!
  [store proposal-id reviewer reason]
  (stores store)
  (let [d (decision store proposal-id :rejected reviewer {:reason (str reason)})
        did (str (id))]
    (commit-decision! store proposal-id reviewer d "rejected" did 0
                      (fn [existing]
                        (assoc d :decision/id (:id existing) :decision/digest (:decision_digest existing))))))

(defn- latest-approval [store proposal-id]
  (when-let [row (first (sqlite/query (:sqlite store)
                                      ["SELECT * FROM invariant_decisions WHERE proposal_id = ? AND decision = 'approved' ORDER BY created_at DESC, id DESC LIMIT 1"
                                       (str proposal-id)]))]
    (verify-decision-row! store row)
    row))

(defn activate!
  "Commit an approved invariant and its event/outbox atomically, then publish
  only after the durable transaction commits."
  ([store proposal-id reviewer]
   (activate! store proposal-id reviewer {}))
  ([store proposal-id reviewer opts]
   (stores store)
   (let [p (get-proposal store proposal-id)
         d (latest-approval store proposal-id)]
     (when-not p (throw (err/error :invariant/proposal-missing "proposal does not exist" {:proposal/id proposal-id})))
     (when-not d (throw (err/error :invariant/approval-missing "activation requires an approval decision" {:proposal/id proposal-id})))
     (when (= (str reviewer) (:proposer p)) (throw (err/error :invariant/reviewer-conflict "proposer cannot activate its own invariant" {})))
     (when-not (= 1 (:activation_qualified d)) (throw (err/error :invariant/not-qualified "activation requires deterministic replay and adversarial G3 evidence" {})))
     (let [aid (str (id))
           activation-digest (cas-put! store {:activation/id aid :proposal/id proposal-id
                                              :decision/id (:id d) :version (:version p)
                                              :predicate (:predicate p)
                                              :registry/revision (:registry/revision p)
                                              :reviewer reviewer} "application/edn")
           desc {:invariant/id (or (:invariant/id p) proposal-id) :proposal/id proposal-id :version (:version p)
                 :predicate (:predicate p) :registry/revision (:registry/revision p)
                 :decision/id (:id d) :activation/id aid :activation/digest activation-digest :activation/committed? true}]
       (let [result (sqlite/with-write-tx [conn (:sqlite store)]
                      (when (seq (sqlite/query-raw! conn "SELECT id FROM invariant_disables WHERE proposal_id = ? LIMIT 1" [(str proposal-id)]))
                        (throw (err/error :invariant/activation-disabled "disabled proposals cannot reactivate" {})))
                      (if-let [old (first (sqlite/query-raw! conn "SELECT * FROM invariant_activations WHERE version = ? AND predicate_digest = ?" [(:version p) (:predicate/digest (:predicate p))]))]
                        {:row old :idempotent? true}
                        (do
                          (sqlite/insert-raw! conn
                            "INSERT INTO invariant_activations (id,proposal_id,decision_id,version,predicate_digest,activation_digest,registry_revision,status,committed_at) VALUES (?,?,?,?,?,?,?,?,?)"
                            [aid (str proposal-id) (:id d) (:version p) (:predicate/digest (:predicate p)) activation-digest (str (:registry/revision p)) "active" (now)])
                          (sqlite/insert-raw! conn
                            "INSERT INTO invariant_events (activation_id,event_type,payload_ref,created_at) VALUES (?,?,?,?)"
                            [aid "activated" activation-digest (now)])
                          (let [ev (first (sqlite/query-raw! conn "SELECT id FROM invariant_events WHERE activation_id = ? AND event_type = 'activated'" [aid]))]
                            (sqlite/insert-raw! conn
                              "INSERT INTO invariant_outbox (id,activation_id,event_id,event_type,dispatched,created_at) VALUES (?,?,?,?,0,?)"
                              [(id) aid (:id ev) "activated" (now)]))
                          (fault/trigger! opts :before-event-append)
                          {:id aid :idempotent? false})))]
         (let [desc (if (:idempotent? result)
                      (assoc desc :activation/id (:id (:row result)) :activation/digest (:activation_digest (:row result)) :decision/id (:decision_id (:row result)))
                      desc)]
           (when (seq (sqlite/query (:sqlite store) ["SELECT id FROM invariant_disables WHERE proposal_id = ? LIMIT 1" (str proposal-id)]))
             (throw (err/error :invariant/activation-disabled "activation was disabled before publication" {})))
           (static/publish-active-invariant! desc ((var-get #'static/activation-proof) (:activation/digest desc)))
           (assoc desc :status :active :idempotent? (:idempotent? result))))))))
(defn disable!
  [store proposal-id reviewer reason]
  (stores store)
  (let [p (get-proposal store proposal-id)]
    (when-not p (throw (err/error :invariant/proposal-missing "proposal does not exist" {})))
    (when (= (str reviewer) (str (:proposer p)))
      (throw (err/error :invariant/reviewer-conflict "proposer cannot disable its own proposal" {})))
    (let [state (:status p)
           existing (first (sqlite/query (:sqlite store)
                                         ["SELECT * FROM invariant_disables WHERE proposal_id = ? LIMIT 1"
                                         (str proposal-id)]))
          d (when (contains? #{:active :disabled} state)
              (invariant/disable proposal-id reviewer reason))
          digest (when d (cas-put! store d "application/edn"))]
      (cond
        (= :disabled state)
        (do
          (when-not existing
            (throw (err/error :invariant/disable-invalid-state
                              "disabled invariant is missing its durable disable decision"
                              {:proposal/id proposal-id})))
          (verify-disable-row! store existing)
          (when-not (= digest (:disable_digest existing))
            (throw (err/error :invariant/idempotency-conflict
                              "already-disabled invariant has a different durable decision"
                              {:proposal/id proposal-id})))
          (let [activations (sqlite/query (:sqlite store)
                                          ["SELECT * FROM invariant_activations WHERE proposal_id = ? AND status = 'active'"
                                           (str proposal-id)])]
            (when-not (= 1 (count activations))
              (throw (err/error :invariant/disable-invalid-state
                                "disabled invariant must retain exactly one current active activation"
                                {:proposal/id proposal-id :activation/count (count activations)})))
            (verify-activation! store (first activations))
            (static/disable-active-invariant! proposal-id))
          (assoc d :decision/digest (:disable_digest existing) :status :disabled :idempotent? true))

        (not= :active state)
        (throw (err/error :invariant/disable-invalid-state
                          "disable is only valid for the current active activation"
                          {:proposal/id proposal-id :status state}))

        existing
        (throw (err/error :invariant/disable-invalid-state
                          "active invariant already has a durable disable decision"
                          {:proposal/id proposal-id}))

        :else
        (let [activations (sqlite/query (:sqlite store)
                                         ["SELECT * FROM invariant_activations WHERE proposal_id = ? AND status = 'active'"
                                          (str proposal-id)])
              _ (when-not (= 1 (count activations))
                  (throw (err/error :invariant/disable-invalid-state
                                    "disable requires exactly one current active activation"
                                    {:proposal/id proposal-id :activation/count (count activations)})))
              _ (verify-activation! store (first activations))
              result (sqlite/with-write-tx [conn (:sqlite store)]
                       (let [activations (sqlite/query-raw! conn
                                                            "SELECT * FROM invariant_activations WHERE proposal_id = ? AND status = 'active'"
                                                            [(str proposal-id)])]
                         (when-not (= 1 (count activations))
                           (throw (err/error :invariant/disable-invalid-state
                                             "disable requires exactly one current active activation"
                                             {:proposal/id proposal-id :activation/count (count activations)})))
                         (let [activation (first activations)
                               disable-id (id)]
                           (sqlite/insert-raw! conn
                             "INSERT INTO invariant_disables (id,proposal_id,reviewer,reason,disable_digest,created_at) VALUES (?,?,?,?,?,?)"
                             [disable-id (str proposal-id) (str reviewer) (str reason) digest (now)])
                           (sqlite/insert-raw! conn
                             "INSERT INTO invariant_events (activation_id,event_type,payload_ref,created_at) VALUES (?,?,?,?)"
                             [(:id activation) "disabled" digest (now)])
                           (let [ev (first (sqlite/query-raw! conn
                                                              "SELECT * FROM invariant_events WHERE activation_id = ? AND event_type = 'disabled'"
                                                              [(:id activation)]))]
                             (sqlite/insert-raw! conn
                               "INSERT INTO invariant_outbox (id,activation_id,event_id,event_type,dispatched,created_at) VALUES (?,?,?,?,0,?)"
                               [(id) (:id activation) (:id ev) "disabled" (now)]))
                           {:id disable-id :activation activation})))]
          (static/disable-active-invariant! proposal-id)
          (assoc d :decision/digest digest :status :disabled :idempotent? false))))))

(defn- quarantine-activation! [store row]
  (sqlite/with-write-tx [conn (:sqlite store)]
    (let [aid (str (:id row))
          digest (:activation_digest row)
          disabled? (seq (sqlite/query-raw! conn
                                             "SELECT id FROM invariant_events WHERE activation_id = ? AND event_type = 'disabled' LIMIT 1"
                                             [aid]))
          disabled-outbox? (seq (sqlite/query-raw! conn
                                                    "SELECT id FROM invariant_outbox WHERE activation_id = ? AND event_type = 'disabled' LIMIT 1"
                                                    [aid]))
          quarantined? (seq (sqlite/query-raw! conn
                                                "SELECT id FROM invariant_events WHERE activation_id = ? AND event_type = 'quarantined' LIMIT 1"
                                                [aid]))]
      (when (or disabled? disabled-outbox?)
        (throw (err/error :invariant/terminal-conflict
                          "disabled and quarantined terminal evidence cannot coexist"
                          {:activation/id aid})))
      (when-not quarantined?
        (sqlite/insert-raw! conn
          "INSERT INTO invariant_events (activation_id,event_type,payload_ref,created_at) VALUES (?,?,?,?)"
          [aid "quarantined" digest (now)]))
      (let [ev (first (sqlite/query-raw! conn
                                         "SELECT * FROM invariant_events WHERE activation_id = ? AND event_type = 'quarantined'"
                                         [aid]))]
        (when-not ev
          (throw (err/error :invariant/quarantine-failed
                            "quarantine event was not durably written"
                            {:activation/id aid})))
        (when-not quarantined?
          (sqlite/insert-raw! conn
            "INSERT INTO invariant_outbox (id,activation_id,event_id,event_type,dispatched,created_at) VALUES (?,?,?,?,0,?)"
            [(id) aid (:id ev) "quarantined" (now)]))))))

(defn- recovery-descriptor [store row]
  (when-not (= "active" (:status row))
    (throw (err/error :invariant/recovery-invalid
                      "only active activation rows may be recovered"
                      {:activation/id (:id row) :status (:status row)})))
  (verify-activation! store row)
  (let [p (get-proposal store (:proposal_id row))]
    {:invariant/id (or (:invariant/id p) (:proposal_id row))
     :proposal/id (:proposal_id row) :version (:version row)
     :predicate (:predicate p) :registry/revision (:registry_revision row)
     :decision/id (:decision_id row)
     :activation/id (:id row) :activation/digest (:activation_digest row)}))

(defn recover-activations!
  "Validate the complete durable activation snapshot before publishing any
  runtime descriptor. Invalid rows are durably quarantined; valid rows in the
  same snapshot are not published when any row is invalid."
  [store]
  (stores store)
  (static/clear-active-invariants!)
  (let [rows (sqlite/query (:sqlite store)
                           ["SELECT a.* FROM invariant_activations a
                             WHERE a.status = 'active'
                               AND NOT EXISTS (SELECT 1 FROM invariant_disables d WHERE d.proposal_id = a.proposal_id)
                               AND NOT EXISTS (SELECT 1 FROM invariant_events q WHERE q.activation_id = a.id AND q.event_type = 'quarantined')
                             ORDER BY a.committed_at, a.id"])
        checked (mapv (fn [row]
                        (try
                          {:row row :descriptor (recovery-descriptor store row)}
                          (catch Exception e {:row row :error e}))) rows)
        invalid (filterv :error checked)]
    (when (seq invalid)
      ;; This write is deliberately not caught: a failed quarantine must fail
      ;; closed rather than silently retrying a corrupt activation on restart.
      (doseq [{:keys [row]} invalid]
        (quarantine-activation! store row))
      (static/clear-active-invariants!))
    (if (seq invalid)
      (mapv (fn [{:keys [row error descriptor]}]
              {:activation/id (:id row)
               :status (if error :quarantined :blocked)
               :error (when error (err/error-data error))}) checked)
      (try
        (doseq [{:keys [descriptor]} checked]
          (static/publish-active-invariant! descriptor ((var-get #'static/activation-proof) (:activation/digest descriptor))))
        (mapv (fn [{:keys [row]}] {:activation/id (:id row) :status :published}) checked)
        (catch Exception e
          (static/clear-active-invariants!)
          (throw e))))))

;; Stable narrow aliases keep callers on the data-only boundary; no registry
;; or executable-code handles are exposed.
(def propose propose!)
(def append-run append-run!)
(defn append-counterexample!
  [store proposal-id result]
  (append-run! store proposal-id
               (assoc (if (map? result) result {:result result})
                      :kind :counterexample)))
(def approve-invariant! approve!)
(def reject-invariant! reject!)
(def activate-invariant! activate!)
(def disable-invariant! disable!)