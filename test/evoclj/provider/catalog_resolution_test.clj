(ns evoclj.provider.catalog-resolution-test
  "S14 — tool catalog pin→provider resolution enforcement (后置).

  A tool catalog pins a set of tool ids that a consumer (the model-facing
  tool loop, the llm-mutator's tool catalog, etc.) will advertise and
  execute. Every pinned tool id must RESOLVE to a real registered
  provider — a tool-id → provider/handler binding in the kernel-owned
  provider registry. A pinned tool id with NO resolvable provider is a
  SILENT DANGLING TOOL REFERENCE and must fail closed with a typed error
  (never a bare nil, never an uncaught NPE downstream, never a partial
  map with a dangling id left in).

  These tests drive the production resolution function
  (evoclj.provider.registry/resolve-tool-catalog) against the real
  provider registry (production component), register real providers
  through the real register! path, and prove the resolved reference is
  usable end-to-end by dispatching a real tool-call through the real
  broker (evoclj.intent.dispatch/dispatch!) — INV-09: production
  components, no injected fns, no shape-only assertions.

  Error contract (typed, serializable EDN):
    :provider/catalog-invalid       — a malformed catalog (a non-collection,
                                       or an entry that does not carry a
                                       keyword tool-id / :tool / :tool/id).
    :provider/catalog-unresolved-tool — a pinned tool id that is ABSENT
                                       (never registered) or REMOVED
                                       (registered then unregistered) has
                                       no resolvable provider. Fail-closed.
  "
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]))

;; --- shared test values ----------------------------------------------------

(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private phenotype-id
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(defn- tool-lease
  "A valid CapabilityLease granting :invoke on :fixture/echo to
  phenotype-id."
  []
  {:cap/id (random-uuid)
   :subject {:phenotype/id phenotype-id}
   :resource {:kind :tool :id :fixture/echo}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at (java.util.Date.)
   :expires-at (java.util.Date. (+ (.getTime (java.util.Date.)) 60000))})

(defn- echo-intent
  "A validated :intent/tool-call for :fixture/echo carrying args."
  [args]
  (intent/tool-call session-id phenotype-id :node/tool 17
                    {:tool/id :fixture/echo :args args}
                    {:wall-ms 1000}))

;; --- helpers ---------------------------------------------------------------

(defn- error-of
  "The ExceptionInfo thrown by the thunk f, or nil when f succeeds."
  [f]
  (try (f)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- is-typed-error
  "Assert the thunk f throws an ExceptionInfo with the given
  :error/type."
  [f expected]
  (let [e (error-of f)]
    (is (some? e) "the call is rejected with an ExceptionInfo")
    (is (= expected (:error/type (ex-data e))))))

;; ============================================================================
;; happy — a pinned tool resolves to its registered provider
;; ============================================================================

(deftest pinned-tool-resolves-to-its-provider
  (testing "a catalog pinning one registered tool resolves to that tool's
            provider entry (a resolved reference, never a dangling id)"
    (let [reg (registry/create-registry)
          registered-id (registry/register! reg (fixture/echo-provider))
          _ (is (= :fixture/echo registered-id))
          resolved (registry/resolve-tool-catalog reg [{:tool :fixture/echo}])]
      (is (= #{:fixture/echo} (set (keys resolved))))
      (let [entry (get resolved :fixture/echo)]
        (is (map? entry))
        ;; the resolved reference carries the REAL provider object + descriptor
        (is (satisfies? proto/Provider (:provider entry)))
        (is (= :fixture/echo (get-in entry [:descriptor :tool/id])))
        (is (= :pure (get-in entry [:descriptor :effect])))))))

(deftest bare-keyword-tool-id-resolves
  (testing "a bare keyword tool-id is also a valid pinned catalog reference"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))
          resolved (registry/resolve-tool-catalog reg [:fixture/echo])]
      (is (= #{:fixture/echo} (set (keys resolved))))
      (is (satisfies? proto/Provider (get-in resolved [:fixture/echo :provider]))))))

(deftest assembler-form-tool-id-resolves
  (testing "the assembler form (a map carrying :tool/id) is a valid pinned
            catalog reference — the shape the RequestAssembler's :tool-map
            uses, so both wire and assembler forms resolve the same way"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))
          resolved (registry/resolve-tool-catalog reg [{:tool/id :fixture/echo
                                                        :name "echo_tool"}])]
      (is (= #{:fixture/echo} (set (keys resolved))))
      (is (satisfies? proto/Provider (get-in resolved [:fixture/echo :provider]))))))

;; ============================================================================
;; multiple pinned tools resolve consistently (deterministic, sorted)
;; ============================================================================

(deftest multiple-pinned-tools-resolve-consistently
  (testing "several pinned tools all resolve to their providers in a
            deterministic (sorted) resolved map"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))
          _ (registry/register! reg (fixture/path-resolve-provider))
          ;; The catalog's INPUT order deliberately DIFFERS from sorted
          ;; order (:fixture/path-resolve sorts AFTER :fixture/echo), so a
          ;; plain-map / input-order implementation would yield keys in
          ;; input order and be KILLED by the sorted assertion below.
          resolved (registry/resolve-tool-catalog
                    reg [{:name "resolve" :tool :fixture/path-resolve}
                         {:name "echo" :tool :fixture/echo}
                         :fixture/path-resolve])]
      (is (= [:fixture/echo :fixture/path-resolve] (vec (keys resolved)))
          "keys are the distinct pinned tool ids, deterministically sorted
           (independent of catalog input order)")
      (is (= 2 (count resolved)))
      (is (satisfies? proto/Provider (get-in resolved [:fixture/echo :provider])))
      (is (satisfies? proto/Provider
                       (get-in resolved [:fixture/path-resolve :provider])))
      ;; re-resolution is identical (determinism: same catalog + same registry)
      (is (= resolved (registry/resolve-tool-catalog
                       reg [{:name "resolve" :tool :fixture/path-resolve}
                            {:name "echo" :tool :fixture/echo}
                            :fixture/path-resolve]))))))

(deftest empty-catalog-resolves-to-empty-map
  (testing "a catalog pinning no tools is trivially resolved to {} (vacuous,
            not an error — nothing is dangling)"
    (let [reg (registry/create-registry)]
      (is (= {} (registry/resolve-tool-catalog reg [])))
      (is (= {} (registry/resolve-tool-catalog reg #{}))))))

;; ============================================================================
;; branch: an unresolvable pinned tool fails closed (typed)
;; ============================================================================

(deftest unresolved-pinned-tool-fails-closed
  (testing "a pinned tool id that is NEVER registered is rejected with a
            typed :provider/catalog-unresolved-tool error carrying the id"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))]
      (let [e (error-of #(registry/resolve-tool-catalog reg [:fixture/ghost]))
            d (err/error-data e)]
        (is (some? e))
        (is (= :provider/catalog-unresolved-tool (:error/type (ex-data e))))
        (is (= [:fixture/ghost] (get-in (ex-data e) [:tool/ids])))
        (is (= [{:tool/id :fixture/ghost :status :absent}]
               (get-in (ex-data e) [:unresolved])))
        ;; Global Constraint 22: the typed diagnostic is plain serializable EDN
        (is (= d (clojure.edn/read-string (pr-str d))))
        (is (= :provider/catalog-unresolved-tool (:error/type d)))))))

(deftest removed-pinned-tool-fails-closed
  (testing "a pinned tool id that WAS registered and then UNREGISTERED is
            rejected as :provider/catalog-unresolved-tool with :status :removed
            (a removed tool is a dangling reference, distinct from never-existed)"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))
          _ (registry/unregister! reg :fixture/echo)]
      (let [e (error-of #(registry/resolve-tool-catalog reg [:fixture/echo]))]
        (is (some? e))
        (is (= :provider/catalog-unresolved-tool (:error/type (ex-data e))))
        (is (= [{:tool/id :fixture/echo :status :removed}]
               (get-in (ex-data e) [:unresolved])))))))

(deftest unknown-tool-id-is-typed
  (testing "a genuinely unknown tool id is never silently dropped: typed
            fail-closed (same classification as the never-registered case)"
    (let [reg (registry/create-registry)]
      (is-typed-error #(registry/resolve-tool-catalog reg [:not/a-real-tool])
                      :provider/catalog-unresolved-tool))))

;; ============================================================================
;; fault: a malformed catalog entry is rejected (no partial resolution)
;; ============================================================================

(deftest malformed-catalog-entry-rejected
  (testing "a catalog entry that is neither a keyword nor a map carrying
            :tool / :tool/id is :provider/catalog-invalid (never silently skipped)"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))]
      (is-typed-error #(registry/resolve-tool-catalog reg [{:name "x" :action :y}])
                      :provider/catalog-invalid)
      (is-typed-error #(registry/resolve-tool-catalog reg ["not-a-tool"])
                      :provider/catalog-invalid)
      (is-typed-error #(registry/resolve-tool-catalog reg :not-a-collection)
                      :provider/catalog-invalid)
      (is-typed-error #(registry/resolve-tool-catalog reg [{:tool 42}])
                      :provider/catalog-invalid))))

;; ============================================================================
;; production path: the resolved reference is USABLE through the real broker
;; ============================================================================

(deftest resolved-reference-executes-through-broker
  (testing "an end-to-end production path: resolve the catalog to a real
            provider reference, then dispatch a real tool-call through the
            broker — the tool really executes"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))
          resolved (registry/resolve-tool-catalog reg [{:tool :fixture/echo}])
          ctx (dispatch/make-broker-context
               {:registry reg
                :leases [(tool-lease)]})
          result (dispatch/dispatch! ctx (echo-intent {:text "hello from S14"}))]
      (is (= :ok (:result/status result)))
      (is (= {:text "hello from S14"} (:value result)))
      (is (= :allow (get-in result [:authorization :decision])))
      ;; the resolved reference really is the provider the broker just ran
      (is (satisfies? proto/Provider (get-in resolved [:fixture/echo :provider]))))))

;; ============================================================================
;; concurrency — the registry is shared mutable state (an atom). Resolution
;; must never hand back a partial/dangling reference, and concurrent
;; resolutions over a stable registry must agree.
;; ============================================================================

(deftest concurrent-resolutions-are-consistent
  (testing "N concurrent resolutions over a stable registry agree exactly
            (deterministic), and each successful resolve is a fully-present map"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))
          _ (registry/register! reg (fixture/path-resolve-provider))
          catalog [{:tool :fixture/echo} {:tool :fixture/path-resolve}]
          expected (registry/resolve-tool-catalog reg catalog)
          ;; eagerly create ALL futures first so they genuinely run in
          ;; parallel against the shared registry atom
          futures (doall (repeatedly 8
                                     #(future
                                        (dotimes [_ 20]
                                          (registry/resolve-tool-catalog
                                           reg catalog))
                                        (registry/resolve-tool-catalog
                                         reg catalog))))
          results (mapv deref futures)]
      (is (every? #(= expected %) results)))
    (testing "an unresolvable pinned tool is consistently rejected across ALL
              concurrent resolutions (never silently dropped, never a dangling id)"
      (let [reg (registry/create-registry)
            _ (registry/register! reg (fixture/echo-provider))
            catalog [:fixture/echo :fixture/missing]
            futures (doall (repeatedly 6
                                       #(future
                                          (try
                                            (registry/resolve-tool-catalog
                                             reg catalog)
                                            :no-throw
                                            (catch clojure.lang.ExceptionInfo e
                                              (:error/type (ex-data e)))))))
            results (mapv deref futures)]
        (is (every? #(= :provider/catalog-unresolved-tool %) results))))))

;; ============================================================================
;; fail-closed contract: a successful result is NEVER partial (no dangling id)
;; ============================================================================

(deftest successful-resolution-never-partial
  (testing "a successful resolve-tool-catalog result contains ONLY tool-ids
            that were present, each mapped to a full registry entry — a
            dangling-id partial map is impossible"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))
          resolved (registry/resolve-tool-catalog
                    reg [{:tool :fixture/echo} {:tool :fixture/echo}])]
      (is (= #{:fixture/echo} (set (keys resolved)))
          "duplicate pins collapse to one key; only present tool-ids appear")
      (is (satisfies? proto/Provider (get-in resolved [:fixture/echo :provider]))))))
