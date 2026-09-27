(ns evoclj.capability.budget-guard-test
  "Direct coverage for the two fail-closed guards in
  evoclj.intent.pipeline/budget-attempt.

  Those guards are unreachable through any production path today, and
  that is CORRECT rather than a defect: capability.mint only attaches
  :budget when its caller passes one, and no production minting site
  does (evolution_tools.clj:363, cli/session.clj:718/742,
  mount/filesystem.clj:164, subagent_capability.clj:101/112 all omit it).
  So `opted?` is false for every real lease.

  That makes the guards dead surface, and dead surface is exactly what
  rots. These tests call budget-attempt directly with a budgeted lease so
  both branches are exercised and pinned — they are pre-wired protection
  for a future host that DOES supply budgets, not code that can be
  reached today. Wiring a real ceiling is a product decision and is out
  of scope; what is in scope is that the protection is not merely
  present but correct."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.mint :as mint]
            ;; required so the private budget-attempt var exists to resolve
            [evoclj.intent.pipeline]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

(def ^:private session-id
  (java.util.UUID/randomUUID))

(def ^:private now (java.util.Date. 1700000000000))

(def ^:private expires
  (java.util.Date. (+ (.getTime ^java.util.Date now) 3600000)))

(defn- budgeted-lease
  "A real sealed lease carrying a finite :budget — the shape that makes
  budget-attempt's `opted?` true."
  []
  (mint/mint-lease!
   nil
   {:principal {:principal/type :session :session/id session-id}
    :resource {:kind :tool :id :fixture/echo}
    :actions #{:invoke}
    :constraints {}
    :issued-at now
    :expires-at expires
    :budget {:calls 1}}))

(defn- attempt
  "Invoke the private budget-attempt with `ctx`; returns the thrown
  :error/type, or ::no-throw when it returned normally."
  [ctx lease]
  (let [f (deref (resolve 'evoclj.intent.pipeline/budget-attempt))]
    (try
      (f ctx {:intent/id (java.util.UUID/randomUUID) :metadata {}}
         {:lease-id (:cap/id lease) :principal (:principal lease)}
         1)
      ::no-throw
      (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e))))))

(deftest budgeted-lease-without-a-store-fails-closed
  (let [lease (budgeted-lease)]
    (is (some? (:budget lease))
        "the lease really opted in — otherwise the guard could not fire")
    (is (= :capability/budget-authority-unavailable
           (attempt {:leases [lease] :budget-store nil} lease))
        "a budgeted lease with no durable budget store is refused, never
         silently allowed to run unbudgeted")))

(deftest budgeted-lease-without-an-allocation-fails-closed
  (let [db-path (str (java.nio.file.Files/createTempFile
                      "evoclj-budget-" ".db"
                      (make-array java.nio.file.attribute.FileAttribute 0)))]
    (migrate/migrate! db-path)
    (try
      (let [lease (budgeted-lease)
            db (sqlite/spec db-path)]
        (is (= :capability/budget-missing
               (attempt {:leases [lease] :budget-store db} lease))
            "a budgeted lease with a store but no allocation is refused"))

      (finally
        (java.nio.file.Files/deleteIfExists
         (java.nio.file.Paths/get db-path (into-array String [])))))))

(deftest an-unbudgeted-lease-is-untouched
  (let [lease (mint/mint-lease!
               nil
               {:principal {:principal/type :session :session/id session-id}
                :resource {:kind :tool :id :fixture/echo}
                :actions #{:invoke}
                :constraints {}
                :issued-at now
                :expires-at expires})]
    (is (nil? (:budget lease)) "no budget was requested")
    (is (= ::no-throw (attempt {:leases [lease] :budget-store nil} lease))
        "today's production shape: an unbudgeted lease reserves nothing and
         is not refused")))
