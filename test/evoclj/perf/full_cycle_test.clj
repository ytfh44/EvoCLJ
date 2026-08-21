(ns evoclj.perf.full-cycle-test
  "component — the full-cycle timing harness, exercised headlessly on the
  fixture-provider path (no model endpoint, no network needed).

  The harness lives in scripts/full-cycle.clj (namespace
  evoclj.perf.full-cycle-harness): it provisions a fresh state dir,
  records an Evolution set, then runs evolve -> eval -> promote through
  the SAME public subsystem APIs the `cycle` CLI command walks
  (evolution.core/propose-candidates!, eval.core/evaluate-candidate!,
  promotion.promote/promote!), timing every phase and collecting F2
  metric records (evoclj.metrics.core) — including the component eval
  envelope passed into evaluate-candidate!.

  scripts/ is NOT on the test classpath (deps.edn :paths is src +
  resources), so the test loads the script with load-file relative to
  the repo root (the documented working dir of the suite) and drives
  its public run-harness entry point.

  What is asserted:
    - the report is a plain EDN map (round-trips through
      clojure.edn/read-string — \"results written as structured EDN\");
    - every phase carries a positive wall-clock timing (:wall-ms);
    - the cycle really ran: 1 candidate evolved, 1 eligible evaluation,
      1 promotion;
    - the F2 metric records are present and each satisfies the closed
      MetricSchema (evoclj.metrics.core/validate-record!);
    - the provider section is HONEST: :provider/mode :fixture with
      :model/endpoint? false and an explanatory note (no real model
      endpoint is reachable on this host — no API keys / offline
      catalog — so the numbers are fixture-mode, never fabricated as
      real-model timings).

  RUN: clojure -M:test -n evoclj.perf.full-cycle-test"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.metrics.core :as f2])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; ----------------------------------------------------------------------------
;; loading the harness script (scripts/ is not on the classpath)
;; ----------------------------------------------------------------------------

(defn- run-harness
  "Load scripts/full-cycle.clj (with the script runner disabled) and
  run its public run-harness entry point with default options (fresh
  temp state dir, fixture path)."
  []
  (System/setProperty "evoclj.harness.loaded-by-test" "true")
  (load-file "scripts/full-cycle.clj")
  (let [run (requiring-resolve 'evoclj.perf.full-cycle-harness/run-harness)]
    (run {})))

(defn- temp-dir [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree! [path]
  (with-open [stream (Files/walk (Paths/get path (make-array String 0))
                                 (make-array FileVisitOption 0))]
    (doseq [p (reverse (iterator-seq (.iterator stream)))]
      (Files/deleteIfExists p))))

;; ----------------------------------------------------------------------------
;; the harness report contract
;; ----------------------------------------------------------------------------

(deftest full-cycle-harness-runs-headless-and-times-every-phase
  (let [report (run-harness)]
    (testing "the report is structured EDN"
      (is (map? report))
      (is (= :full-cycle (:harness/name report)))
      (is (= report (edn/read-string (pr-str report)))
          "the report round-trips through clojure.edn/read-string")
      (is (string? (:state-dir report))))
    (testing "output EDN contains per-phase wall timings"
      (let [phases (:phases report)]
        (is (map? phases))
        (is (pos-int? (get-in phases [:evolve :wall-ms])))
        (is (pos-int? (get-in phases [:eval :wall-ms])))
        (is (pos-int? (get-in phases [:promote :wall-ms])))
        (is (pos-int? (:cycle/wall-ms report)))))
    (testing "the harness actually ran evolve -> eval -> promote"
      (is (= 1 (get-in report [:phases :evolve :candidates])))
      (is (= 1 (get-in report [:phases :eval :evaluated])))
      (is (= 1 (get-in report [:phases :eval :eligible])))
      (is (= 1 (get-in report [:phases :promote :promoted])))
      (is (= "generation-1" (get-in report [:seed :generation/id])))
      (is (some? (get-in report [:seed :genome/id]))))
    (testing "F2 metric records are collected and schema-valid"
      (let [records (:f2/metrics report)]
        (is (seq records) "the harness collected F2 metric records")
        (doseq [r records]
          (is (= r (f2/validate-record! r))
              "every F2 record satisfies the closed MetricSchema"))
        (is (some #(= :cycle/evolve-ms (:metric/name %)) records))
        (is (some #(= :cycle/eval-ms (:metric/name %)) records))
        (is (some #(= :cycle/promote-ms (:metric/name %)) records))
        (is (some (fn [r]
                    (and (= :cycle/total-ms (:metric/name r))
                         (= :runtime (:metric/scope r))))
                  records))
        (is (some (fn [r]
                    (and (= :eval/total-ms (:metric/name r))
                         (= :candidate (:metric/scope r))))
                  records)
            "the component eval envelope recorded :eval/total-ms for the candidate")
        (is (some (fn [r]
                    (and (= :eval/total-ms (:metric/name r))
                         (pos? (:metric/value r))))
                  records)
            "the eval envelope carried a positive total wall time")))
    (testing "the provider mode is reported honestly (fixture fallback)"
      (is (= :fixture (get-in report [:provider :mode])))
      (is (false? (get-in report [:provider :model/endpoint?]))
          "no real model endpoint is reachable on this host")
      (is (string? (get-in report [:provider :model/note])))
      (is (seq (get-in report [:provider :model/note]))
          "the no-endpoint note is explicit, never an empty string"))))

;; ----------------------------------------------------------------------------
;; the written EDN output is the same structured report
;; ----------------------------------------------------------------------------

(deftest full-cycle-harness-writes-structured-edn-to-a-file
  (let [dir (temp-dir "evoclj-fullcycle-out-")
        out (str dir "/report.edn")]
    (try
      (System/setProperty "evoclj.harness.loaded-by-test" "true")
      (load-file "scripts/full-cycle.clj")
      (let [write (requiring-resolve 'evoclj.perf.full-cycle-harness/write-report!)
            report (run-harness)]
        (write report out)
        (testing "the file exists and parses back to the identical report"
          (is (Files/isRegularFile (Paths/get out (make-array String 0))
                                   (make-array LinkOption 0)))
          (let [read-back (edn/read-string (slurp out))]
            (is (= report read-back))
            (is (pos-int? (get-in read-back [:phases :eval :wall-ms]))))))
      (finally
        (delete-tree! dir)))))
