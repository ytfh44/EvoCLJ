(ns evoclj.capability.broker
  "The kernel-owned capability broker: authorize as a pure decision
  (component).

  `authorize` is the NORMATIVE broker entry point of Milestone 4 and
  is deliberately PURE: it validates its inputs and composes the
  policy module (evoclj.capability.policy) with the lease model
  (evoclj.capability.lease / .schema) into a deterministic decision,
  performing no I/O — no provider invocation, no registry read, no
  state change. The effectful dispatcher that turns an :allow into a
  real provider effect arrives in component

  (authorize {:intent intent
              :normalized-request request
              :leases leases
              :usage usage
              :now now})
  ;; => {:decision :allow :lease-id ...}
  ;; or {:decision :deny :reason :capability/missing}

  Input contract:

  - :intent             a validated v0 Intent (re-validated here with
                        evoclj.intent.schema; carries the Global
                        Constraint 20 attribution the decision
                        principal is derived from).
  - :normalized-request the CANONICAL resource descriptor produced by
                        provider normalize-request (component):
                        {:tool/id ... :resource {...} ...}. It must
                        carry :resource; authorization is decided on
                        this canonical form, never on raw user input
                        (Global Constraint 9). The resource's :action
                        (when present) is the first-class ResourceAction
                        component of the authorization tuple (INV-07):
                        the broker authorizes a :request target with that
                        action (falling back to the intent action only
                        when the resource carries none).
  - :leases             a collection of CapabilityLease values
                        (evoclj.capability.schema; validated by the
                        policy). nil means no grant at all.
  - :usage              per-lease usage: a map from :cap/id to an entry
                        {:calls N :bytes B} with the calls and bytes
                        already consumed under that lease, so
                        :constraints {:max-calls N} / {:max-bytes B}
                        are enforced on their OWN dimensions by the
                        policy. nil means no usage consumed.
  - :now                the decision instant, an #inst value.

  Fail-closed: no lease in :leases grants the request -> deny with
  :capability/missing; an UNREGISTERED resource kind -> deny with
  :capability/unknown-resource-kind; merely registering or exposing a
  tool never authorizes it (Global Constraint 9). The deny reason codes
  are stable and documented in evoclj.capability.policy (plus
  :capability/unknown-resource-kind, which is raised by the broker
  before any policy decision when the resource kind is not registered).

  Coarse fallback (audit item 4): an MCP request whose resource carries
  :mcp/classification :invoke-fallback (no declared projection covered
  the call) is denied with :capability/coarse-invoke-denied unless a
  lease explicitly opts into the coarse whole-tool scope with
  :mcp/allow-coarse-invoke true on its :resource grant. A whole-tool
  grant alone never implies the fallback scope.

  Malformed input is never silently judged: it throws
  :capability/schema-invalid (or :intent/schema-invalid for a malformed
  intent), because garbage never authorizes and never hides a caller
  bug."
  (:require [clojure.set :as set]
            [evoclj.capability.policy :as policy]
            [evoclj.capability.resource-kind :as rk]
            [evoclj.capability.semantic :as semantic]
            [evoclj.intent.schema :as intent-schema]
            [evoclj.kernel.error :as err]))

(defn- semantic-state
  "Validate the frozen semantic request identity without allowing malformed
  semantic data to reach the ordinary provider/resource policy."
  [resource]
  (if-not (contains? resource :semantic/spec)
    {:required? false :valid? true}
    (try
      (let [spec (:semantic/spec resource)
            digest (:semantic/digest resource)]
        {:required? true
         :valid? (= digest (semantic/spec-digest spec))
         :spec spec})
      (catch clojure.lang.ExceptionInfo _
        {:required? true :valid? false}))))

(defn- semantic-grant
  [lease]
  (or (get-in lease [:resource :semantic/spec])
      (:semantic/spec lease)))

(defn- semantic-compatible-leases
  [leases request-spec]
  (filter #(semantic/subsumes? (semantic-grant %) request-spec) leases))

;; --- resource-kind registry -------------------------------------------------
;;
;; The single source of resource-kind definitions is the descriptor
;; registry in evoclj.capability.resource-kind (`builtin-descriptors`,
;; installed at load time). The broker consults it per request via
;; rk/authorization-targets-for and rk/allowed-actions-by-kind; a kind
;; with no targets is unregistered and denied fail-closed below. There is
;; no separate broker allowlist.

(defn- resolve-target-resource
  "The canonical resource a target authorizes against."
  [target normalized-request]
  (case (:source target)
    :request (:resource normalized-request)
    :tool {:kind :tool :id (:tool/id normalized-request)}))

(defn- resolve-target-action
  "The action a target is authorized with. :request honors the
  resource's classification (first-class ResourceAction of the tuple,
  INV-07) and falls back to the intent action when the resource carries
  none; :intent always uses the intent action (the tool is invoked
  regardless of the resource operation). P6: :tool honors distinct
  :invoke/:read/:write from the request's :action (resource or top-level);
  :filesystem honors :read/:write etc; :model is always :invoke."
  [target normalized-request intent]
  (let [kind (:kind (:resource normalized-request))]
    (case (:action-from target)
      :request (let [req-action (or (:action (:resource normalized-request))
                                   (:action normalized-request))
                     fallback (policy/intent-action intent)]
                 (cond
                   (= :model kind) :invoke
                   req-action req-action
                   fallback fallback
                   :else nil))
      :intent (policy/intent-action intent))))

(defn authorize
  "The pure broker authorization decision for one request (normative,
  component).

  Validates the intent and the normalized request, then dispatches the
  decision through the resource-kind REGISTRY (evoclj.capability.policy/
  decide + evoclj.capability.lease/resource-covers?): each registered
  target for the request's resource :kind must allow. The ResourceAction
  (the operation performed on the resource) is a first-class component of
  the authorization tuple (INV-07) - a :request target is authorized with
  the resource's own :action (projected by the provider, e.g. :read /
  :write / :delete), falling back to the intent action only when the
  resource carries none. Pure: no I/O, no state change, no provider
  invocation - the effectful dispatcher arrives in component

  Optional inputs: :lease-registry (P5) is an optional atomLeaseRegistry
  (as created by capability/mint create-lease-registry, same shape as
  mount/filesystem) that records revocation; when supplied a revoked
  lease yields :capability/revoked fail-closed for ANY kind
  (tool/model/memory/filesystem).
  Also accepted as :leases-registry / :revocation-registry for compat.

  See the namespace docstring for the input contract and the stable
  deny reason codes; an unregistered resource kind is denied with
  :capability/unknown-resource-kind (fail closed)."
  [{:keys [intent normalized-request leases usage now lease-registry leases-registry revocation-registry]}]
  (intent-schema/validate-intent intent)
  (when-not (and (map? normalized-request)
                 (map? (:resource normalized-request)))
    (throw (err/error :capability/schema-invalid
                      "normalized request must carry a :resource map"
                      {:value (err/sanitize normalized-request)})))
    (let [lease-reg (or lease-registry leases-registry revocation-registry)
          revoked? (fn [lease] (when lease-reg (boolean (get-in @lease-reg [(:cap/id lease) :revoked?]))))
          principal (policy/intent-principal intent)
          resource (:resource normalized-request)
          semantic (semantic-state resource)
          kind (:kind resource)
          targets (rk/authorization-targets-for kind)]
    (if (nil? targets)
      {:decision :deny :reason :capability/unknown-resource-kind}
      (loop [remaining targets
             best nil]
        (if-let [t (first remaining)]
          (let [res (resolve-target-resource t normalized-request)
                act (resolve-target-action t normalized-request intent)
                ;; P6: fail-closed for unknown / non-allowlisted actions (C1: descriptor registry)
                unknown-action? (or (nil? act)
                                   (not (keyword? act))
                                   (not (contains? (apply set/union (vals (rk/allowed-actions-by-kind))) act)))
                all-leases (or leases [])
                semantic-leases (if (:required? semantic)
                                  (semantic-compatible-leases all-leases (:spec semantic))
                                  all-leases)
                ;; partition leases into non-revoked and revoked for this registry
                non-revoked (if lease-reg (remove revoked? semantic-leases) semantic-leases)
                d (cond
                    (and (:required? semantic) (not (:valid? semantic)))
                    {:decision :deny :reason :capability/semantic-invalid}

                    (and (:required? semantic) (empty? semantic-leases))
                    {:decision :deny :reason :capability/semantic-denied}

                    unknown-action?
                    {:decision :deny :reason :capability/unknown-action}

                    :else
                    (policy/decide non-revoked principal res act now (or usage {})))
                d (if (= :deny (:decision d))
                    ;; no non-revoked lease allowed; check if a revoked one would have allowed
                    (let [revoked-leases (if lease-reg (filter revoked? semantic-leases) [])
                          rd (when (and (seq revoked-leases) (not unknown-action?))
                               (policy/decide revoked-leases principal res act now (or usage {})))]
                      (if (and rd (= :allow (:decision rd)))
                        {:decision :deny :reason :capability/revoked}
                        d))
                    d)]
            (if (= :deny (:decision d))
              d
              (recur (rest remaining) (or best d))))
          (let [result (or best {:decision :deny :reason :capability/missing})
                classification (:mcp/classification
                                (:resource normalized-request))]
            ;; Multi-target kinds (e.g. :filesystem/path authorizes a
            ;; :tool target first) return the first allow, which may come
            ;; from an unclassified target resource. Echo the request
            ;; provenance so the decision — and therefore the journal —
            ;; always records how the request classified. Additive key,
            ;; present only when the request carried one.
            (if (and (= :allow (:decision result))
                     (some? classification)
                     (nil? (:mcp/classification result)))
              (assoc result :mcp/classification classification)
              result)))))))
