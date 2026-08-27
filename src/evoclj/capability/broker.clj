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
                        subject is derived from).
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
  - :usage              per-lease call usage: a map from :cap/id to
                        the number of calls already consumed under
                        that lease, so :constraints {:max-calls N}
                        is enforced by the policy. nil means no usage
                        consumed.
  - :now                the decision instant, an #inst value.
  - :registry           (optional) a resource-kind registry overriding
                        the built-in default. Extensibility hook: new
                        resource kinds are registered here, never by
                        branching inside authorize.

  Fail-closed: no lease in :leases grants the request -> deny with
  :capability/missing; an UNREGISTERED resource kind -> deny with
  :capability/unknown-resource-kind; merely registering or exposing a
  tool never authorizes it (Global Constraint 9). The deny reason codes
  are stable and documented in evoclj.capability.policy (plus
  :capability/unknown-resource-kind, which is raised by the broker
  before any policy decision when the resource kind is not registered).
  Malformed input is never silently judged: it throws
  :capability/schema-invalid (or :intent/schema-invalid for a malformed
  intent), because garbage never authorizes and never hides a caller
  bug."
  (:require [evoclj.capability.policy :as policy]
            [evoclj.intent.schema :as intent-schema]
            [evoclj.kernel.error :as err]))

;; --- resource-kind registry -------------------------------------------------
;;
;; The broker's former hard-coded :filesystem/path dual authorization
;; (tool grant AND resource grant) is generalized into a REGISTRY of
;; resource kinds. Each registered kind maps to a vector of authorization
;; targets; every target must allow (evoclj.capability.policy/decide
;; returns :allow) for the request as a whole to be allowed. The first
;; target that denies short-circuits with that deny decision. This is the
;; single dispatch mechanism: a request whose kind is NOT in the registry
;; is rejected fail-closed (see authorize).
;;
;; A target is one of:
;;   {:source :request  :action-from :request|:intent}
;;     authorize the normalized request's :resource with the action taken
;;     from the resource's :action (the operation projected by the
;;     provider, e.g. evoclj.mcp.canonical/canonical-resource sets
;;     :action :read for a filesystem read), falling back to the intent
;;     action when the resource carries no :action;
;;   {:source :tool     :action-from :request|:intent}
;;     authorize a derived {:kind :tool :id <tool/id>} resource with the
;;     given action source - this is the tool-grant half of the dual
;;     authorization.
;;
;; The :filesystem/path entry is now just ONE registry entry whose
;; target vector has two members (tool + request); there is no special
;; case. New kinds are added by extending this map - never by branching
;; inside authorize (extensibility, not two hard-coded branches).

(def ^:private default-resource-kind-registry
  "The built-in resource-kind registry (fail-closed default). Unregistered
  kinds are denied with :capability/unknown-resource-kind by authorize."
  {:tool [{:source :request :action-from :request}]
   :model [{:source :request :action-from :request}]
   :memory [{:source :request :action-from :request}]
   :filesystem [{:source :request :action-from :request}]
   :filesystem/path [{:source :tool :action-from :intent}
                     {:source :request :action-from :request}]})

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
  regardless of the resource operation)."
  [target normalized-request intent]
  (case (:action-from target)
    :request (or (:action (:resource normalized-request))
                 (policy/intent-action intent))
    :intent (policy/intent-action intent)))

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

  Optional input: :registry overrides the built-in resource-kind registry
  (extensibility / testing); when absent the
  default-resource-kind-registry is used.

  See the namespace docstring for the input contract and the stable
  deny reason codes; an unregistered resource kind is denied with
  :capability/unknown-resource-kind (fail closed)."
  [{:keys [intent normalized-request leases usage now registry]}]
  (intent-schema/validate-intent intent)
  (when-not (and (map? normalized-request)
                 (map? (:resource normalized-request)))
    (throw (err/error :capability/schema-invalid
                      "normalized request must carry a :resource map"
                      {:value (err/sanitize normalized-request)})))
  (let [subject (policy/intent-subject intent)
        reg (or registry default-resource-kind-registry)
        kind (:kind (:resource normalized-request))
        targets (get reg kind)]
    (if (nil? targets)
      ;; unregistered resource kind -> fail closed (no implicit default)
      {:decision :deny :reason :capability/unknown-resource-kind}
      (loop [remaining targets
             best nil]
        (if-let [t (first remaining)]
          (let [res (resolve-target-resource t normalized-request)
                act (resolve-target-action t normalized-request intent)
                d (policy/decide (or leases [])
                                 subject res act now (or usage {}))]
            (if (= :deny (:decision d))
              d
              (recur (rest remaining) (or best d))))
          (or best {:decision :deny :reason :capability/missing}))))))
