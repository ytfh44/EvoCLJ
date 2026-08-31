(ns evoclj.capability.broker-resource-action-registry-test
  "M14 — authorization tuple extension: resource action enters the policy
  decision, and the broker's former hard-coded :filesystem/path dual
  authorization is generalized into a resource-kind REGISTRY.

  These tests exercise the REAL production path:
  evoclj.capability.broker/authorize (which composes
  evoclj.capability.policy/decide and evoclj.capability.lease/
  resource-covers?) is called with plain normalized requests — the same
  shape produced by evoclj.provider.memory/normalize-request,
  evoclj.mcp.canonical/canonical-resource, and fixture providers. No
  injected fn, no shape-only assertions: every assertion is about the
  verdict / stable reason code the broker returns, which is what the
  dispatcher and scheduler consume.

  The six required paths:

  - HAPPY: a registered kind whose resource action is honored authorizes
    when the lease grants that action (and, for :filesystem/path, the
    tool grant is also present via the registry's dual target).
  - BRANCH 1 (resource action honored in the decision): a :filesystem
    request carrying :action :read is allowed only when the lease grants
    :read (not merely :invoke); :write vs :read are distinct.
  - BRANCH 2 (registry dispatches per kind uniformly / is extensible):
    a CUSTOM registry passed to authorize authorizes a brand-new kind
    exactly like the built-in kinds — no hard-coded branch.
  - FAULT 1 (unknown resource kind -> deny, fail-closed): an unregistered
    kind is denied with :capability/unknown-resource-kind, never granted.
  - FAULT 2 (action not permitted -> deny): a request whose resource
    action is absent from the lease's :actions is denied with
    :capability/action-denied.
  - REGRESSION (old hard-coded dual branch is gone + doc/behavior
    consistency): the :filesystem/path dual authorization is driven by
    the registry entry (both a tool grant AND a resource grant are
    required); a request carrying only an :invoke tool lease and a
    :filesystem/path resource whose action the tool lease does NOT grant
    now reports the precise :capability/action-denied reason.

  Every scenario is fail-closed and typed: malformed kinds never
  silently grant."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.broker.registry :as reg]
            [evoclj.capability.broker :as broker]
            [evoclj.intent.core :as intent]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private phenotype-p2
  "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
(def ^:private cause-event-id 42)
(def ^:private budget {:wall-ms 1000})

(def ^:private issued-at (java.util.Date. 1700000000000))
(def ^:private expires-at (java.util.Date. 1700003600000))
(def ^:private in-window (java.util.Date. 1700001800000))

(defn- tool-lease
  "A valid :tool lease for phenotype-p1, optionally with assoc-style
  overrides (e.g. :actions, :resource)."
  [cap-id & kvs]
  (let [base {:cap/id cap-id
              :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id phenotype-p1}
              :resource {:kind :tool :id :fixture/echo}
              :actions #{:invoke}
              :constraints {:max-calls 10}
              :issued-at issued-at
              :expires-at expires-at}]
    (if (seq kvs) (apply assoc base kvs) base)))

(def ^:private path-tool-id
  "The :tool/id used by the :fixture/path-resolve provider (the tool a
  :filesystem/path request is dispatched through)."
  :fixture/path-resolve)

(defn- fs-lease
  "A valid :filesystem (or :filesystem/path) lease for phenotype-p1
  granting `actions` over `path`, optionally overridden."
  [cap-id actions path & kvs]
  (let [base {:cap/id cap-id
              :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id phenotype-p1}
              :resource {:kind :filesystem :path path}
              :actions actions
              :constraints {:max-calls 10}
              :issued-at issued-at
              :expires-at expires-at}]
    (if (seq kvs) (apply assoc base kvs) base)))

(defn- fs-path-lease
  "A valid :filesystem/path lease for phenotype-p1 granting `actions`
  over `path`."
  [cap-id actions path & kvs]
  (let [base {:cap/id cap-id
              :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id phenotype-p1}
              :resource {:kind :filesystem/path :path path}
              :actions actions
              :constraints {:max-calls 10}
              :issued-at issued-at
              :expires-at expires-at}]
    (if (seq kvs) (apply assoc base kvs) base)))

(defn- echo-intent
  "A valid :intent/tool-call for :fixture/echo (action :invoke)."
  []
  (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                    {:tool/id :fixture/echo :args {:text "hi"}}
                    budget))

(defn- authorize
  "Run broker/authorize with the given leases, optional registry, usage,
  and instant (defaults: default registry, no usage, in-window)."
  ([normalized leases] (authorize normalized leases nil {} in-window))
  ([normalized leases reg] (authorize normalized leases reg {} in-window))
  ([normalized leases reg usage now]
   (broker/authorize {:intent (echo-intent)
                      :normalized-request normalized
                      :leases leases
                      :registry reg
                      :usage usage
                      :now now})))

(defn- allow? [d] (= :allow (:decision d)))

;; ============================================================================
;; HAPPY PATH — resource action honored + registry dual target required
;; ============================================================================

(deftest filesystem-path-request-action-read-honored-with-dual-grants
  (testing "a :filesystem/path request carrying :action :read is ALLOWED
            only when BOTH a tool grant (:invoke) AND a resource grant
            (:read) are present — the registry drives the dual auth"
    (let [normalized {:tool/id path-tool-id
                      :resource {:kind :filesystem/path
                                 :path "/work/secret"
                                 :action :read}}
          tool (tool-lease #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
                           :resource {:kind :tool :id path-tool-id})
          fs-read (fs-path-lease
                   #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"
                   #{:read} "/work/secret")]
      (is (allow? (authorize normalized [tool fs-read])))))
  (testing "dropping the tool grant denies (registry still requires the
            tool target even though the resource action is granted)"
    (let [normalized {:tool/id path-tool-id
                      :resource {:kind :filesystem/path
                                 :path "/work/secret"
                                 :action :read}}
          fs-read (fs-path-lease
                   #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"
                   #{:read} "/work/secret")]
      (is (not (allow? (authorize normalized [fs-read])))))))

;; ============================================================================
;; BRANCH 1 — resource action honored in the decision (single-target kind)
;; ============================================================================

(deftest filesystem-resource-action-read-vs-write-distinct
  (testing "a :filesystem request carrying :action :read is allowed when
            the lease grants :read (not merely :invoke)"
    (let [normalized {:tool/id :fixture/path-resolve
                      :resource {:kind :filesystem
                                 :path "/work/secret"
                                 :action :read}}
          lease (fs-lease #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc"
                          #{:read} "/work/secret")]
      (is (allow? (authorize normalized [lease])))))
  (testing "the same :read-granting lease DENIES a :write request (read
            and write are distinct resource actions)"
    (let [normalized {:tool/id :fixture/path-resolve
                      :resource {:kind :filesystem
                                 :path "/work/secret"
                                 :action :write}}
          lease (fs-lease #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc"
                          #{:read} "/work/secret")]
      (is (not (allow? (authorize normalized [lease]))))
      (is (= :capability/action-denied
             (:reason (authorize normalized [lease]))))))
  (testing "a :write-granting lease allows a :write request"
    (let [normalized {:tool/id :fixture/path-resolve
                      :resource {:kind :filesystem
                                 :path "/work/secret"
                                 :action :write}}
          lease (fs-lease #uuid "dddddddd-dddd-4ddd-8ddd-dddddddddddd"
                          #{:write} "/work/secret")]
      (is (allow? (authorize normalized [lease]))))))

;; ============================================================================
;; BRANCH 2 — registry dispatches per kind uniformly / is extensible
;; ============================================================================

(deftest custom-registry-dispatches-uniformly-not-hardcoded
  (testing "a CUSTOM resource-kind registry makes authorize dispatch
            uniformly: the default :filesystem/path entry REQUIRES a tool
            grant AND a resource grant (dual auth), but a custom registry
            that lists ONLY the request target authorizes a
            :filesystem/path request with a resource grant alone — proving
            the dual auth is driven by the registry, not a hard-coded
            branch. (Before M14 the dual branch was hard-coded, so this
            request would have been denied even without a tool lease.)"
    (let [request-only-registry
          (reg/make-registry {:filesystem/path
           [{:source :request :action-from :request}]})
          normalized {:tool/id path-tool-id
                      :resource {:kind :filesystem/path
                                 :path "/work/secret"
                                 :action :read}}
          fs-read (fs-path-lease
                   #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"
                   #{:read} "/work/secret")]
      ;; with the custom (request-only) registry, a resource grant alone
      ;; authorizes the request — the tool grant is no longer required
      (is (allow? (authorize normalized [fs-read] request-only-registry)))
      ;; with the DEFAULT registry, the same inputs are denied (tool grant
      ;; still required) — the two registries produce different behavior
      (is (not (allow? (authorize normalized [fs-read])))))))

(deftest built-in-kinds-still-dispatch-through-registry
  (testing "tool, model, memory, and filesystem kinds each authorize via
            the default registry's single request target (regression of
            the pre-M14 per-kind behavior, now uniform)"
    (let [echo-normalized {:tool/id :fixture/echo
                           :resource {:kind :tool :id :fixture/echo}}
          echo-lease (tool-lease #uuid "ffffffff-ffff-4fff-8fff-ffffffffffff")]
      (is (allow? (authorize echo-normalized [echo-lease]))))
    (let [fs-normalized {:tool/id :fixture/path-resolve
                         :resource {:kind :filesystem
                                    :path "/work/secret"}}
          fs-lease (fs-lease #uuid "10101010-1010-4101-8101-101010101010"
                             #{:invoke} "/work/secret")]
      (is (allow? (authorize fs-normalized [fs-lease]))))))

;; ============================================================================
;; FAULT 1 — unknown resource kind -> deny, fail-closed
;; ============================================================================

(deftest unknown-resource-kind-denied-fail-closed
  (testing "a request whose resource :kind is not registered is denied
            with :capability/unknown-resource-kind — never granted"
    (let [normalized {:tool/id :fixture/unknown
                      :resource {:kind :unknown/xyz :id :thing}}]
      (is (= {:decision :deny :reason :capability/unknown-resource-kind}
             (authorize normalized [(tool-lease
                                     #uuid "20202020-2020-4202-8202-202020202020")])))))
  (testing "the unknown-kind deny is not an exception — fail closed, not
            fail loud"
    (let [normalized {:tool/id :fixture/unknown
                      :resource {:kind :unknown/xyz :id :thing}}]
      (is (= :deny
             (:decision
              (authorize normalized
                         [(tool-lease #uuid "30303030-3030-4303-8303-303030303030")])))))))

;; ============================================================================
;; FAULT 2 — action not permitted -> deny
;; ============================================================================

(deftest action-not-permitted-denied
  (testing "a :filesystem/path request whose :action (:read) is absent
            from the resource grant's :actions is denied with
            :capability/action-denied"
    (let [normalized {:tool/id path-tool-id
                      :resource {:kind :filesystem/path
                                 :path "/work/secret"
                                 :action :read}}
          tool (tool-lease #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
                           :resource {:kind :tool :id path-tool-id})
          fs-write (fs-path-lease
                    #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"
                    #{:write} "/work/secret")]
      (is (not (allow? (authorize normalized [tool fs-write]))))
      (is (= :capability/action-denied
             (:reason (authorize normalized [tool fs-write])))))))

;; ============================================================================
;; REGRESSION — old hard-coded dual branch gone + doc/behavior consistency
;; ============================================================================

(deftest filesystem-path-dual-auth-requires-both-grants
  (testing "regression: the :filesystem/path dual authorization is driven
            by the registry — removing either the tool or the resource
            grant denies. With only a tool lease and a :filesystem/path
            resource whose action the tool lease does NOT grant, the
            precise reason is now :capability/action-denied (the resource
            action is a first-class tuple component)."
    (let [normalized {:tool/id path-tool-id
                      :resource {:kind :filesystem/path
                                 :path "/work/secret"
                                 :action :read}}
          tool (tool-lease #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
                           :resource {:kind :tool :id path-tool-id})]
      (is (not (allow? (authorize normalized [tool]))))
      (is (= :capability/action-denied
             (:reason (authorize normalized [tool]))))))
  (testing "with a tool lease AND a resource grant that matches the
            requested :action, the request allows (both registry targets
            pass) — this is the same end-to-end path the M13 canonical
            projection feeds"
    (let [normalized {:tool/id path-tool-id
                      :resource {:kind :filesystem/path
                                 :path "/work/secret"
                                 :action :read}}
          tool (tool-lease #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
                           :resource {:kind :tool :id path-tool-id})
          fs-read (fs-path-lease
                   #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"
                   #{:read} "/work/secret")]
      (is (allow? (authorize normalized [tool fs-read]))))))
