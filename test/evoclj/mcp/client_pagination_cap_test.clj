(ns evoclj.mcp.client-pagination-cap-test
  "WO-M6 — list-all-tools pagination hard cap, typed :mcp/pagination-exceeded.

  Context (WO-T1 finding): the MCP Java SDK 2.0.0 auto-follows nextCursor
  *inside a single* listTools call, so one client/list-tools invocation can
  already return every server page aggregated into one vector. The production
  risk is therefore unbounded blocking/aggregation at the SDK-internal layer;
  the guard MUST live at the EvoCLJ layer — inside list-all-tools — and raise a
  typed :mcp/pagination-exceeded. This is fail-closed (INV-04), enforced
  before the unbounded result is returned, and is NOT delegated to the SDK.

  Production-path discipline (INV-09): every behavioral test drives a REAL
  fake-mcp-server subprocess (FAKE_MODE=many-pages) through the production
  client (open! -> :client -> list-all-tools). No discover-fn / injected stub
  replaces the listing path. The cap is exercised by passing the real
  :max-tools opt that production callers may supply; the configured default
  is exercised by rebinding *default-max-tools* (the same dynamic var a
  configuration layer would set).

  Required six path classes:
    A. happy path — under cap returns the full aggregated set
    B. new branch — over cap raises :mcp/pagination-exceeded
    C. new branch — exactly-at-boundary cap returns the full set (inclusive)
    D. >=2 fault cases — cap = 0 (invalid/zero ceiling) and cap below count
    E. concurrency — cap checked per independent client under the same server
    F. regression — the previously-unbounded aggregation is now bounded
    G. doc/behavior consistency — the typed signal serializes & round-trips"
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.kernel.error :as err]
            [evoclj.mcp.client :as mcp]
            [evoclj.mcp.support.fake-server :as fake])
  (:import [java.util.concurrent Executors]
           [java.util.concurrent.atomic AtomicInteger]))

;; Tool population used by the behavioral tests: many-pages with a large
;; tool count and small page size => the SDK aggregates many real wire pages
;; into one production result; the cap is on the AGGREGATE tool count.
(def ^:const tool-count 50)
(def ^:const page-size 5)

(defmacro ^:private with-many-pages-server
  "Start a many-pages fake server bound to `srv-bind`, run body, and always
   stop the server. Each consumer opens its OWN managed client (the MCP Java
   SDK McpSyncClient is not thread-safe, so sharing one across threads would
   confound the cap guard under test)."
  [[srv-bind] & body]
  `(fake/with-fake-server [~srv-bind {:mode :many-pages
                                      :tool-count ~tool-count
                                      :page-size ~page-size}]
     ~@body))

(defn- with-client
  "Open a REAL managed client against `srv`, invoke (f client), close, return."
  [srv f]
  (let [managed (mcp/open! (:config srv))]
    (try
      (f (:client managed))
      (finally
        (mcp/close! managed)))))

(defn- ex-type
  "Extract :error/type from a Throwable (or nil)."
  [t]
  (:error/type (ex-data t)))

(defn- reason-of
  "Extract the :error/reason from an error's serialized ex-data (it nests under
   :error/data after err/error-data)."
  [t]
  (get-in (err/error-data t) [:error/data :error/reason]))

;; ---------------------------------------------------------------------------
;; A. happy path — under cap returns the full aggregated set
;; ---------------------------------------------------------------------------

(deftest a-happy-path-returns-full-set-under-cap
  (testing "WO-M6 A: with a comfortable cap the full aggregated tool set is
            returned (no truncation, no error)"
    (with-many-pages-server [srv]
      (with-client srv
        (fn [c]
          (let [tools (mcp/list-all-tools c {:max-tools 1000})]
            (is (= tool-count (count tools))
                "all 50 aggregated tools returned under the cap")
            (is (= tool-count (count (distinct (map :mcp/name tools))))
                "no duplicates across the aggregated pages")
            (is (every? :mcp/name tools) "every descriptor carries a name")))))))

;; ---------------------------------------------------------------------------
;; B. new branch — over cap raises :mcp/pagination-exceeded
;; ---------------------------------------------------------------------------

(deftest b-over-cap-raises-typed-pagination-exceeded
  (testing "WO-M6 B: when the aggregate count exceeds max-tools, list-all-tools
            fails closed with the typed :mcp/pagination-exceeded signal"
    (with-many-pages-server [srv]
      (with-client srv
        (fn [c]
          (let [t (try (mcp/list-all-tools c {:max-tools 10})
                       (catch Throwable e e))]
            (is (instance? Throwable t)
                "over-cap must throw rather than return a partial/oversized set")
            (is (= :mcp/pagination-exceeded (ex-type t))
                "the typed error is exactly :mcp/pagination-exceeded")
            (is (= :tool-count-exceeded (reason-of t))
                "the reason discriminates the tool-count branch")
            (is (<= (get-in (ex-data t) [:observed]) tool-count)
                "observed count is the real aggregated size")))))))

;; ---------------------------------------------------------------------------
;; C. new branch — exactly-at-boundary cap is inclusive (returns full set)
;; ---------------------------------------------------------------------------

(deftest c-boundary-cap-exactly-at-count-returns-full-set
  (testing "WO-M6 C: a cap exactly equal to the aggregated count is inclusive —
            the full set is returned, not rejected"
    (with-many-pages-server [srv]
      (with-client srv
        (fn [c]
          (let [tools (mcp/list-all-tools c {:max-tools tool-count})]
            (is (= tool-count (count tools))
                "cap == count is allowed (boundary inclusive)")))))))

;; ---------------------------------------------------------------------------
;; D. fault cases — cap = 0 (invalid ceiling) and cap below count
;; ---------------------------------------------------------------------------

(deftest d1-cap-zero-is-invalid-and-fails-closed
  (testing "WO-M6 D (fault 1): a zero max-tools is not a positive integer, so
            it must fail closed with :mcp/pagination-exceeded up front — never
            silently accept an unbounded or partial list"
    (with-many-pages-server [srv]
      (with-client srv
        (fn [c]
          (let [t (try (mcp/list-all-tools c {:max-tools 0})
                       (catch Throwable e e))]
            (is (instance? Throwable t))
            (is (= :mcp/pagination-exceeded (ex-type t)))
            (is (= :invalid-max-tools (reason-of t))
                "the reason discriminates the invalid-config branch")))))))

(deftest d2-cap-below-count-fails-closed
  (testing "WO-M6 D (fault 2): a positive cap strictly below the aggregate
            count (boundary - 1) must still fail closed"
    (with-many-pages-server [srv]
      (with-client srv
        (fn [c]
          (let [t (try (mcp/list-all-tools c {:max-tools (dec tool-count)})
                       (catch Throwable e e))]
            (is (= :mcp/pagination-exceeded (ex-type t)))
            (is (= :tool-count-exceeded (reason-of t))))))))
  (testing "WO-M6 D (fault 2b): a negative cap is also rejected as invalid"
    (with-many-pages-server [srv]
      (with-client srv
        (fn [c]
          (let [t (try (mcp/list-all-tools c {:max-tools -5})
                       (catch Throwable e e))]
            (is (= :mcp/pagination-exceeded (ex-type t)))
            (is (= :invalid-max-tools (reason-of t)))))))))

;; ---------------------------------------------------------------------------
;; E. concurrency — each independent client's cap is checked (no shared state)
;; ---------------------------------------------------------------------------

(deftest e-concurrent-over-cap-all-fail-closed
  (testing "WO-M6 E: many concurrent listings, each on its OWN client against
            the same server and an over-tight cap, must each raise
            :mcp/pagination-exceeded; none returns an unbounded set. The cap
            check is pure/local, so no shared mutable state is touched."
    (with-many-pages-server [srv]
      (let [n 16
            pool (Executors/newFixedThreadPool n)
            hits (AtomicInteger.)
            fails (AtomicInteger.)
            tasks (repeatedly n
                              #(fn []
                                 (with-client srv
                                   (fn [c]
                                     (try
                                       (let [tools (mcp/list-all-tools
                                                     c {:max-tools 10})]
                                         ;; if it returned, it must be bounded
                                         (if (<= (count tools) 10)
                                           (.incrementAndGet hits)
                                           (.incrementAndGet fails)))
                                       (catch Throwable t
                                         (if (= :mcp/pagination-exceeded
                                                (ex-type t))
                                           (.incrementAndGet fails)
                                           (.incrementAndGet hits))))))))]
        (try
          (doseq [fut (.invokeAll pool tasks)]
            (.get fut))
          (is (= n (.get fails))
              "every concurrent listing failed closed (over-cap)")
          (is (= 0 (.get hits))
              "no concurrent listing returned an unbounded/oversized set")
          (finally
            (.shutdownNow pool)))))))

;; ---------------------------------------------------------------------------
;; F. regression — the previously-unbounded aggregation is now bounded
;; ---------------------------------------------------------------------------

(deftest f-regression-previously-unbounded-now-bounded
  (testing "WO-M6 F (regression, T1): before the EvoCLJ-layer cap, list-all-tools
            returned the entire aggregated set regardless of size — a 50-tool
            server's full aggregate was always returned. Now the configured cap
            bounds it: with a cap below the aggregate the function MUST NOT
            return the unbounded set, and with the default cap a modest server
            still works (no regression for legitimate use)."
    ;; (1) the unbounded path is now bounded: over-cap throws, does not return 50
    (with-many-pages-server [srv]
      (with-client srv
        (fn [c]
          (let [t (try (mcp/list-all-tools c {:max-tools 10})
                       (catch Throwable e e))]
            (is (= :mcp/pagination-exceeded (ex-type t))
                "the old 'always return everything' behavior is gone")))))
    ;; (2) legitimate use is unaffected: default cap (10000) still returns all 50
    (with-many-pages-server [srv]
      (with-client srv
        (fn [c]
          (is (= tool-count (count (mcp/list-all-tools c)))
              "default cap leaves a normal server fully usable (no regression)"))))))

(deftest f2-configured-default-cap-is-enforced
  (testing "WO-M6 F (config path): a configuration layer sets the cap by
            rebinding *default-max-tools*; the 1-arity call then honors it"
    (with-many-pages-server [srv]
      (with-client srv
        (fn [c]
          (binding [mcp/*default-max-tools* 10]
            (let [t (try (mcp/list-all-tools c)
                         (catch Throwable e e))]
              (is (= :mcp/pagination-exceeded (ex-type t))
                  "1-arity call respects the configured default cap")))
          ;; and returns normally when the default is raised again
          (binding [mcp/*default-max-tools* 1000]
            (is (= tool-count (count (mcp/list-all-tools c)))
                "raising the default cap restores full listing")))))))

;; ---------------------------------------------------------------------------
;; G. doc/behavior consistency — typed signal serializes & round-trips
;; ---------------------------------------------------------------------------

(deftest g-typed-error-serializes-and-round-trips
  (testing "WO-M6 G: the :mcp/pagination-exceeded signal is fully serializable
            and round-trips through err/error-data (GC-22 boundary contract),
            and carries the documented :error/type keyword"
    (with-many-pages-server [srv]
      (with-client srv
        (fn [c]
          (let [t (try (mcp/list-all-tools c {:max-tools 10})
                       (catch Throwable e e))
                data (err/error-data t)
                round (clojure.edn/read-string
                        (pr-str data))]
            (is (= :mcp/pagination-exceeded (:error/type data))
                "ex-data carries the documented typed keyword")
            (is (= :mcp/pagination-exceeded (:error/type round))
                "round-trips through pr-str / edn read-string (no opaque values)")
            (is (= :tool-count-exceeded (get-in round [:error/data :error/reason]))
                "reason survives serialization")))))))
