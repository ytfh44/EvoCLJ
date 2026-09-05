(ns evoclj.mcp.fallback-attestation-test
  "Audit item 4 end-to-end: an MCP tool with no declared projection is
  denied by default, allowed with an explicit coarse-grant lease and
  journaled as fallback-classified, while a declared-projection call
  authorizes at fine granularity as before. Attestation is honest:
  remote effects are recorded :unverifiable-remote, never confined.

  Uses a local fake provider that normalizes through the REAL
  evoclj.mcp.canonical/canonical-resource and dispatches through the
  REAL pipeline — no injected auth fns."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.mcp.canonical :as canonical]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]))

(def ^:private session-id #uuid "c01c01c0-c01c-4c01-8c01-c01c01c01c01")
(def ^:private phenotype
  "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")
(def ^:private cause-event-id 42)
(def ^:private budget {:wall-ms 1000})
(def ^:private now (java.util.Date. 1700000000000))
(def ^:private issued-at (java.util.Date. 0))
(def ^:private expires-at (java.util.Date. 4102444800000))

(def ^:private fallback-tool-id :mcp/unprojected)
(def ^:private declared-tool-id :mcp/declared-tool)
(def ^:private lease-id #uuid "d0d0d0d0-d0d0-4d0d-8d0d-d0d0d0d0d0d0")

(defn- fake-descriptor
  [tool-id projections]
  (cond-> {:tool/id tool-id
           :effect :remote
           :input-schema [:map [:path :string]]
           :output-schema [:map [:text :string]]
           :required-action :invoke}
    (some? projections)
    (assoc :mcp/param-projections projections)))

(defn- fake-provider
  "A local stand-in for an MCP-backed tool: normalize-request runs the
  real canonical projection; execute echoes without touching a server.
  Projections live on the normalize-time descriptor only: the provider
  registry validates describe output against a closed schema, exactly
  as in production."
  [tool-id projections counter]
  (let [normalize-descriptor (fake-descriptor tool-id projections)
        describe-descriptor (dissoc normalize-descriptor
                                    :mcp/param-projections)]
    (reify proto/Provider
      (describe [_] describe-descriptor)
      (normalize-request [_ intent]
        (let [raw-args (get-in intent [:payload :args])
              args (canonical/value->canonical raw-args)]
          {:tool/id tool-id
           :resource (canonical/canonical-resource normalize-descriptor
                                                   args)
           :args args}))
      (execute-request! [_ _]
        (swap! counter inc)
        {:text "done"}))))

(defn- lease-for
  [tool-id resource-extra]
  {:cap/id lease-id
   :principal {:principal/type :session :session/id session-id}
   :resource (merge {:kind :tool :id tool-id} resource-extra)
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})

(defn- run-with
  [tool-id projections leases args]
  (let [counter (atom 0)
        reg (registry/create-registry)
        _ (registry/register! reg (fake-provider tool-id projections counter))
        ctx (dispatch/make-broker-context
             {:registry reg
              :leases leases
              :usage (atom {})
              :now (constantly now)})
        base (intent/tool-call session-id phenotype :node/tool
                               cause-event-id
                               {:tool/id tool-id :args args}
                               budget)
        keyed (assoc-in base [:metadata :idempotency/key] "req-fallback")]
    {:result (dispatch/dispatch! ctx keyed)
     :executions @counter}))

(deftest undeclared-projection-denied-by-default
  (testing "no declared projection + plain whole-tool lease -> denied
           with :capability/coarse-invoke-denied; provider never runs;
           journal records the fallback provenance as not-executed"
    (let [{:keys [result executions]}
          (run-with fallback-tool-id nil
                    [(lease-for fallback-tool-id nil)]
                    {:path "/etc/shadow"})
          journal (:effect-journal result)
          attestation (:effect/attestation journal)]
      (is (= :error (:result/status result)))
      (is (= :capability/denied (:error/type result)))
      (is (= :capability/coarse-invoke-denied
             (get-in result [:authorization :reason])))
      (is (= 0 executions) "denied requests never execute")
      (is (= :effect/rejected (:effect/final journal)))
      (is (= :invoke-fallback (:mcp/classification attestation)))
      (is (= :not-executed (:verdict attestation))))))

(deftest same-request-allowed-with-explicit-coarse-grant
  (testing "the SAME call with :mcp/allow-coarse-invoke true executes,
           and the journal records fallback provenance honestly:
           allowed but remote-unverifiable, never confined"
    (let [{:keys [result executions]}
          (run-with fallback-tool-id nil
                    [(lease-for fallback-tool-id
                                {:mcp/allow-coarse-invoke true})]
                    {:path "/etc/shadow"})
          journal (:effect-journal result)
          attestation (:effect/attestation journal)]
      (is (= :ok (:result/status result)))
      (is (= {:text "done"} (:value result)))
      (is (= 1 executions) "the coarse grant really executed once")
      (is (= :allow (get-in result [:authorization :decision])))
      (is (= :invoke-fallback
             (get-in result [:authorization :mcp/classification])))
      (is (= :effect/committed (:effect/final journal)))
      (is (= :invoke-fallback (:mcp/classification attestation)))
      (is (= :unverifiable-remote (:verdict attestation)))
      (is (= :unavailable (:observed attestation))))))

(deftest declared-projection-authorizes-fine-grained-as-before
  (testing "a declared :tool projection needs no coarse opt-in: the
           plain whole-tool lease allows exactly as before the change"
    (let [projections [{:param "path"
                        :resource-kind :tool
                        :resource-id declared-tool-id
                        :resource-action :invoke
                        :remote-effect :invoke}]
          {:keys [result executions]}
          (run-with declared-tool-id projections
                    [(lease-for declared-tool-id nil)]
                    {:path "/work/a"})
          journal (:effect-journal result)
          attestation (:effect/attestation journal)]
      (is (= :ok (:result/status result)))
      (is (= 1 executions))
      (is (= :allow (get-in result [:authorization :decision])))
      (is (= :declared-projection
             (get-in result [:authorization :mcp/classification])))
      (is (= :declared-projection (:mcp/classification attestation)))
      (is (= :unverifiable-remote (:verdict attestation))
          "even declared remote effects stay honestly unverifiable"))))
