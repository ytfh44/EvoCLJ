(ns evoclj.eval.gates-test
  "Candidate evaluation gates G0–G3 (component).

  G0 re-parses/re-compiles the candidate Genome from scratch
  (evoclj.genome.load/load-genome + evoclj.compiler.core/compile-genome
  on the candidate's own files — never cached Mutator claims); G1
  revalidates the manifest schema and the ABI against the kernel's
  expected ABI; G2 applies the static policy surface (protected paths
  via evolution.mutation's rules, requested-capability subset,
  forbidden program surfaces via compiler.program's static scan,
  topology validity, and the absence of eval-root files); G3 runs the
  registered deterministic suites (evoclj.eval.static registry) in a
  fresh candidate workspace.

  Every gate returns the normative result map {:gate/id :status
  :hard? true :details-ref :duration-ms}; run-gates-until-hard-failure
  stops at the first non-pass gate. Details are persisted through the
  context's :store-details! fn (an artifact ref per Global Constraint
  21); tests capture them through the capturing context."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.genome.hash :as hash]
            [evoclj.eval.gates :as gates]
            [evoclj.eval.static :as static])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- fixtures and helpers --------------------------------------------------

(def ^:private kernel-abi
  "The kernel's expected ABI (v1)."
  {:kernel 1 :genome 1 :intent 1 :tool 1})

(def ^:private result-keys
  "The normative gate result key set (component)."
  #{:gate/id :status :hard? :details-ref :duration-ms})

(use-fixtures :each
  (fn [f]
    ;; the suite registry is kernel-side and shared; every test starts
    ;; with an empty registry so no suite leaks across tests
    (static/clear-suites!)
    (f)))

(defn- fixture-root
  "The bundle directory for a named fixture under test/fixtures/genomes."
  [name]
  (.toPath (io/file (io/resource (str "fixtures/genomes/" name)))))

(defn- copy-tree!
  "Copy every regular file under `src` into `dst`, preserving relative
  paths (a test-local clone of the dataset workspace copier)."
  [src dst]
  (let [src-path (Paths/get (str src) (make-array String 0))
        dst-path (Paths/get (str dst) (make-array String 0))]
    (with-open [stream (Files/walk src-path (make-array FileVisitOption 0))]
      (doseq [^java.nio.file.Path p (iterator-seq (.iterator stream))]
        (when (Files/isRegularFile p (make-array LinkOption 0))
          (let [rel (.relativize src-path p)
                target (.resolve dst-path rel)]
            (Files/createDirectories (.getParent target)
                                     (make-array FileAttribute 0))
            (Files/copy p target (make-array java.nio.file.CopyOption 0))))))))

(defn- temp-candidate!
  "Copy a named fixture bundle into a fresh temp directory and return
  its path string."
  ([name]
   (let [dir (Files/createTempDirectory "gates-fixture-"
                                        (make-array FileAttribute 0))
         dir-str (str dir)]
     (copy-tree! (fixture-root name) dir-str)
     dir-str))
  ([]
   (temp-candidate! "minimal-valid")))

(defn- write-file!
  "Write `content` as UTF-8 to `path`, creating parent directories."
  [path content]
  (let [p (Paths/get path (make-array String 0))
        parent (.getParent p)]
    (when parent
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))))

(defn- route-descriptor
  "The seed route program descriptor (component)."
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- programs-resolver
  "Registry resolver fn: every candidate declares the seed route
  program (component choice (a) — the registry is in-memory, not a
  bundle file)."
  []
  (fn [_loaded] [(route-descriptor)]))

(defn- provider-catalog
  "The on-disk provider catalog fixture."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- gate-context
  "A minimal valid gate context for a candidate bundle directory. The
  parent is the seed: requested capabilities #{:model/call}, kernel ABI
  v1, and the in-memory route program registry."
  ([root] (gate-context root {}))
  ([root overrides]
   (merge {:candidate/root root
           :provider/catalog (provider-catalog)
           :kernel/abi kernel-abi
           :parent/capabilities #{:model/call}
           :programs (programs-resolver)}
          overrides)))

(defn- ctx-with-capture
  "A gate context whose :store-details! persists details into an atom
  under their artifact ref. Returns [context store] where store maps
  ref -> details."
  [root]
  (let [store (atom {})]
    [(assoc (gate-context root)
            :store-details! (fn [details]
                              (let [ref (hash/text-digest (pr-str details))]
                                (swap! store assoc ref details)
                                ref)))
     store]))

(defn- details-for
  "The persisted details for a gate result, looked up in the store."
  [result store]
  (get @store (:details-ref result)))

;; --- G0: parse from scratch ------------------------------------------------

(deftest valid-candidate-passes-all-gates
  (let [root (temp-candidate!)
        outcome (gates/run-gates-until-hard-failure (gate-context root))]
    (testing "all four gates run and pass"
      (is (not (:stopped? outcome)))
      (is (= [:G0-parse :G1-schema-abi :G2-static-policy
              :G3-deterministic-suites]
             (mapv :gate/id (:results outcome))))
      (is (every? #(= :pass (:status %)) (:results outcome))))))

(deftest g0-fails-on-corrupt-candidate-directory
  (let [root (str (Files/createTempDirectory "gates-corrupt-"
                                             (make-array FileAttribute 0)))
        [ctx store] (ctx-with-capture root)
        result (gates/g0-parse ctx)]
    (testing "an unloadable candidate is a deterministic G0 failure"
      (is (= :G0-parse (:gate/id result)))
      (is (= :fail (:status result)))
      (is (true? (:hard? result)))
      (is (some? (:details-ref result)))
      (is (= :genome/manifest-missing
             (:error/type (details-for result store)))))))

(deftest g0-fails-on-invalid-manifest-edn
  (let [root (temp-candidate!)]
    ;; complete EDN that parses but violates the closed manifest schema
    (write-file! (str root "/manifest.edn") "{:genome/format 2}")
    (let [[ctx store] (ctx-with-capture root)
          result (gates/g0-parse ctx)]
      (is (= :fail (:status result)))
      (is (= :genome/schema-invalid
             (:error/type (details-for result store)))))))

;; --- G1: schema + ABI ------------------------------------------------------

(deftest g1-rejects-abi-mismatch
  (let [root (temp-candidate!)]
    (let [manifest (edn/read-string (slurp (str root "/manifest.edn")))]
      (write-file! (str root "/manifest.edn")
                   (pr-str (assoc-in manifest [:abi :kernel] 2))))
    (let [[ctx store] (ctx-with-capture root)
          result (gates/g1-schema-abi ctx)]
      (is (= :G1-schema-abi (:gate/id result)))
      (is (= :fail (:status result)))
      (let [details (details-for result store)]
        (is (= :abi-mismatch (:check details)))
        (is (= kernel-abi (:expected details)))
        (is (= 2 (get-in details [:candidate :kernel])))))))

(deftest g1-passes-for-valid-candidate
  (let [root (temp-candidate!)
        result (gates/g1-schema-abi (gate-context root))]
    (is (= :pass (:status result)))
    (is (nil? (:details-ref result)))))

;; --- G2: static policy -----------------------------------------------------

(deftest g2-rejects-requested-capability-expansion
  (let [root (temp-candidate!)]
    (let [manifest (edn/read-string (slurp (str root "/manifest.edn")))]
      (write-file! (str root "/manifest.edn")
                   (pr-str (update manifest :capabilities/requested
                                   conj :model/extra))))
    (let [[ctx store] (ctx-with-capture root)
          result (gates/g2-static-policy ctx)]
      (is (= :G2-static-policy (:gate/id result)))
      (is (= :fail (:status result)))
      (let [details (details-for result store)]
        (is (= :capability-expansion (:check (first details))))
        (is (= [:model/extra] (:added (first details))))))))

(deftest g2-rejects-protected-path-file
  (let [root (temp-candidate!)]
    ;; an eval-root file in the candidate genome: the candidate would
    ;; modify the evaluator that judges it (Global Constraint 12)
    (write-file! (str root "/eval/tamper.edn") "{:x 1}")
    (let [[ctx store] (ctx-with-capture root)
          result (gates/g2-static-policy ctx)]
      (is (= :fail (:status result)))
      (let [details (details-for result store)]
        (is (= :protected-path (:check (first details))))
        (is (= :eval-root (:reason (first details))))
        (is (= "eval/tamper.edn" (:path (first details))))))))

(deftest g2-rejects-forbidden-program-surface
  (let [root (temp-candidate!)]
    (write-file! (str root "/programs/route.clj")
                 "(ns agent.route)\n(defn run [x] (eval x))\n")
    (let [[ctx store] (ctx-with-capture root)
          result (gates/g2-static-policy ctx)]
      (is (= :fail (:status result)))
      (let [details (details-for result store)]
        (is (= :forbidden-program-surface (:check (first details))))
        (is (= :program/route (:program/id (first details))))
        (is (= :eval (get-in (first details) [:violation :reason])))))
    (testing "the same candidate also fails G0 (compile-time policy)"
      (let [[ctx _] (ctx-with-capture root)
            result (gates/g0-parse ctx)]
        (is (= :fail (:status result)))))))

(deftest g2-rejects-invalid-topology
  (let [root (temp-candidate!)]
    ;; an arbitrary raw cycle: compile-topology rejects it
    (write-file! (str root "/topology.edn")
                 (pr-str {:graph/id :graph/main
                          :entry :node/a
                          :nodes {:node/a {:node/type :emit :next :node/b}
                                  :node/b {:node/type :emit :next :node/a}}
                          :limits {:max-steps 16}}))
    (let [[ctx store] (ctx-with-capture root)
          result (gates/g2-static-policy ctx)]
      (is (= :fail (:status result)))
      (is (= :invalid-topology (:check (first (details-for result store))))))))

(deftest g2-passes-for-valid-candidate
  (let [root (temp-candidate!)
        result (gates/g2-static-policy (gate-context root))]
    (is (= :pass (:status result)))
    (is (nil? (:details-ref result)))))

;; --- short-circuit ---------------------------------------------------------

(deftest hard-g2-failure-short-circuits-later-gates
  (let [root (temp-candidate!)]
    (write-file! (str root "/eval/tamper.edn") "{:x 1}")
    (let [[ctx store] (ctx-with-capture root)
          outcome (gates/run-gates-until-hard-failure ctx)]
      (testing "later gates are not run after a hard G2 failure"
        (is (:stopped? outcome))
        (is (= [:G0-parse :G1-schema-abi :G2-static-policy]
               (mapv :gate/id (:results outcome))))
        (is (= :fail (:status (last (:results outcome)))))
        (is (= :eval-root (get-in (details-for (last (:results outcome)) store)
                                  [0 :reason])))))))

;; --- G3: deterministic suites ----------------------------------------------

(deftest g3-runs-registered-suites-in-a-fresh-workspace
  (let [root (temp-candidate!)]
    (static/register-suite!
     {:suite/id :unit/validity
      :suite/type :unit
      :check (fn [candidate]
               (when-not (map? (get-in candidate [:candidate/loaded :manifest]))
                 {:check :not-loaded}))})
    (static/register-suite!
     {:suite/id :unit/workspace-materialized
      :suite/type :unit
      :check (fn [candidate]
               (let [ws-root (get-in candidate [:workspace :workspace/root])
                     manifest (slurp (str ws-root "/manifest.edn"))]
                 (when-not (str/includes? manifest ":genome/format 1")
                   {:check :manifest-not-materialized})))})
    (let [result (gates/g3-deterministic-suites (gate-context root))]
      (is (= :G3-deterministic-suites (:gate/id result)))
      (is (= :pass (:status result)))
      (is (nil? (:details-ref result))))))

(deftest g3-reports-suite-failures
  (let [root (temp-candidate!)]
    (static/register-suite!
     {:suite/id :unit/always-fails
      :suite/type :unit
      :check (fn [_candidate] {:check :synthetic-failure})})
    (let [[ctx store] (ctx-with-capture root)
          result (gates/g3-deterministic-suites ctx)]
      (is (= :fail (:status result)))
      (let [details (details-for result store)]
        (is (= :unit/always-fails (:suite/id (first details))))
        (is (= :synthetic-failure (:check (first details))))))))

(deftest g3-records-suite-errors-without-failing-the-gate-type
  (let [root (temp-candidate!)]
    (static/register-suite!
     {:suite/id :unit/explodes
      :suite/type :property
      :check (fn [_candidate] (throw (ex-info "boom" {:synthetic true})))})
    (let [[ctx store] (ctx-with-capture root)
          result (gates/g3-deterministic-suites ctx)]
      (is (= :fail (:status result)))
      (let [details (details-for result store)]
        (is (= :unit/explodes (:suite/id (first details))))
        (is (= :error (:status (first details))))
        (is (= "boom" (:error/message (first details))))))))

;; --- registry contract -----------------------------------------------------

(deftest suite-registry-rejects-invalid-suites
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"suite"
                        (static/register-suite! {:suite/id :x})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"already registered"
                        (do (static/register-suite!
                             {:suite/id :unit/dup :suite/type :unit
                              :check (fn [_] nil)})
                            (static/register-suite!
                             {:suite/id :unit/dup :suite/type :unit
                              :check (fn [_] nil)})))))

;; --- gate result schema ----------------------------------------------------

(deftest gate-result-schema-is-normative
  (let [root (temp-candidate!)
        results (mapv (fn [gate-fn] (gate-fn (gate-context root)))
                      [gates/g0-parse
                       gates/g1-schema-abi
                       gates/g2-static-policy
                       gates/g3-deterministic-suites])]
    (doseq [r results]
      (testing (pr-str (:gate/id r))
        (is (= result-keys (set (keys r))) (pr-str r))
        (is (contains? #{:pass :fail :error} (:status r)) (pr-str r))
        (is (true? (:hard? r)) (pr-str r))
        (is (or (nil? (:details-ref r))
                (re-matches #"^sha256:[0-9a-f]{64}$" (:details-ref r)))
            (pr-str r))
        (is (and (integer? (:duration-ms r)) (not (neg? (:duration-ms r))))
            (pr-str r))))))
