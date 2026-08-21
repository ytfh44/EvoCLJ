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
                        (Global Constraint 9).
  - :leases             a collection of CapabilityLease values
                        (evoclj.capability.schema; validated by the
                        policy). nil means no grant at all.
  - :usage              per-lease call usage: a map from :cap/id to
                        the number of calls already consumed under
                        that lease, so :constraints {:max-calls N}
                        is enforced by the policy. nil means no usage
                        consumed.
  - :now                the decision instant, an #inst value.

  Fail-closed: no lease in :leases grants the request -> deny with
  :capability/missing; merely registering or exposing a tool never
  authorizes it (Global Constraint 9). The deny reason codes are
  stable and documented in evoclj.capability.policy. Malformed input
  is never silently judged: it throws :capability/schema-invalid (or
  :intent/schema-invalid for a malformed intent), because garbage
  never authorizes and never hides a caller bug."
  (:require [evoclj.capability.policy :as policy]
            [evoclj.intent.schema :as intent-schema]
            [evoclj.kernel.error :as err]))

(defn authorize
  "The pure broker authorization decision for one request (normative,
  component).

  Validates the intent and the normalized request, then delegates the
  decision to evoclj.capability.policy/decide with the subject and
  action derived from the intent and the canonical resource taken
  from the normalized request. Pure: no I/O, no state change, no
  provider invocation — the effectful dispatcher arrives in component

  See the namespace docstring for the input contract and the stable
  deny reason codes."
  [{:keys [intent normalized-request leases usage now]}]
  (intent-schema/validate-intent intent)
  (when-not (and (map? normalized-request)
                 (map? (:resource normalized-request)))
    (throw (err/error :capability/schema-invalid
                      "normalized request must carry a :resource map"
                      {:value (err/sanitize normalized-request)})))
  (let [resource (:resource normalized-request)
        tool-resource {:kind :tool :id (:tool/id normalized-request)}
        has-fs-policy? (some #(#{:filesystem :filesystem/path} (:kind (:resource %))) (or leases []))]
    (if (and (= :filesystem/path (:kind resource)) has-fs-policy?)
      (let [tool-decision (policy/decide (or leases []) (policy/intent-subject intent) tool-resource (policy/intent-action intent) now (or usage {}))
            res-decision (policy/decide (or leases []) (policy/intent-subject intent) resource (policy/intent-action intent) now (or usage {}))]
        (cond
          (= :deny (:decision tool-decision)) tool-decision
          (= :deny (:decision res-decision)) res-decision
          :else tool-decision))
      (policy/decide (or leases [])
                     (policy/intent-subject intent)
                     resource
                     (policy/intent-action intent)
                     now
                     (or usage {})))))
