#!/usr/bin/env clojure
;; ---------------------------------------------------------------------------
;; verify-forms.clj — V1 refinement/composition verification
;; ---------------------------------------------------------------------------
;; Usage:
;;     clojure -M scripts/verify-forms.clj [--fix-hint] [DOCS_DIR]
;;
;; Scans docs/formal/*.md for the 32 Wolfram predicates [W-01..W-32] and
;; verifies the V1 refinement invariants (Principal single field, Grant meet,
;; Work×Session product, Hydration, Event prev/causal-links split).
;;
;; Also checks docs/invariants.md consistency (no heritage dual-anchor /
;; cause same-session, has Principal/Grant/Work/Hydration).
;;
;; Output: per-check PASS/FAIL listing. Exit codes:
;;     0  all 32 forms present and all refinement checks pass (0 invalid)
;;     1  at least one check fails (missing W, refinement mismatch, invariants inconsistent)
;;     2  usage or environment error
;;
;; Lexical rules (checked by evoclj.support.verify-forms-test):
;;   - W tokens are `[W-##]` with two digits, 01..32 inclusive
;;   - Tokens are counted distinct across all formal docs (perm+subagent+async+README)
;;   - Refinement checks are keyword-anchored (case-insensitive) so prose changes
;;     that keep the keywords still pass; missing keywords fail loud.
;;   - invariants.md checks: must not contain heritage "dual-anchor" (case-insensitive),
;;     must not contain "cause.*same-session" as a normative invariant; must contain
;;     "Principal" , "Grant" , "Work" , "Hydration" / "hydrate".
;;
(ns scripts.verify-forms
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set])
  (:import (java.nio.file Files LinkOption Paths)))

;; --- lexical ---------------------------------------------------------------

(def ^:private w-token-re
  #"\[W-(\d{2})\]")

(def ^:private expected-w-range
  (set (map #(format "%02d" %) (range 1 33)))) ; 01..32

(defn- read-file [path]
  (try (slurp path) (catch Exception _ nil)))

(defn- md-files-under [dir]
  (let [d (io/file dir)]
    (when (.exists d)
      (->> (file-seq d)
           (filter #(and (.isFile %) (str/ends-with? (.getName %) ".md")))
           (map #(.getPath %))
           vec))))

(defn- extract-w-tokens [text]
  (when text
    (->> (re-seq w-token-re text)
         (map second)
         set)))

(defn scan-formal
  "Scan DOCS_DIR/formal for W tokens. Returns {:w-set set :by-file {path set} :texts {path text}}."
  [docs-dir]
  (let [formal-dir (str docs-dir "/formal")
        files (md-files-under formal-dir)]
    (if (empty? files)
      {:w-set #{} :by-file {} :texts {} :files []}
      (let [by-file (into {} (map (fn [p] [p (or (extract-w-tokens (read-file p)) #{})]) files))
            w-set (apply set/union (vals by-file))
            texts (into {} (map (fn [p] [p (read-file p)]) files))]
        {:w-set w-set :by-file by-file :texts texts :files files}))))

;; --- refinement checks (keyword-anchored) --------------------------------

(defn- contains-ci? [^String s ^String kw]
  (when (and s kw)
    (str/includes? (str/lower-case s) (str/lower-case kw))))

(defn- check-refinement
  "Check docs/formal texts contain refinement keywords. Returns seq of {:check :pass? :reason}."
  [formal-texts]
  (let [all-text (str/join "\n" (vals formal-texts))
        checks
        [{:id "PrincipalSingleFieldQ" :kw "Principal" :desc "Principal single field replaces dual-anchor" :need "Principal"}
         {:id "ResourceKindOpenQ" :kw "ResourceKindDescriptor" :desc "open registry replaces closed kind set"}
         {:id "GrantLatticeQ" :kw "Grant" :desc "Grant = ResourceScope × ActionSet lattice"}
         {:id "GrantMeetQ" :kw "meet" :desc "Grant meet greatest lower bound"}
         {:id "WorkProductQ" :kw "Work" :desc "Work 7 states (queued|running|waiting)"}
         {:id "WorkProductCollapseQ" :kw "48" :desc "Session×Command 48 collapses to Work 7"}
         {:id "HydrationQ" :kw "hydrate" :desc "hydrate(pin) → ExecutionHandle"}
         {:id "EventPrevQ" :kw "prev/event-id" :desc "prev linear same-session predecessor"}
         {:id "EventCausalQ" :kw "causal-links" :desc "causal-links cross-session graph"}
         {:id "EventChainQ" :kw "sha256" :desc "sha256 hash chain"}]]
    (map (fn [{:keys [id kw desc]}]
           {:check id :desc desc :pass? (boolean (contains-ci? all-text kw)) :kw kw})
         checks)))

(defn- check-invariants
  "Check docs/invariants.md consistency. Returns seq of checks."
  [docs-dir]
  (let [path (str docs-dir "/invariants.md")
        text (read-file path)]
    (if (nil? text)
      [{:check "invariantsExistsQ" :desc "invariants.md exists" :pass? false}]
      (let [has-dual-anchor (contains-ci? text "dual-anchor")
            has-cause-same (boolean (re-find #"(?i)cause.*same-session" text))
            has-principal (contains-ci? text "Principal")
            has-grant (contains-ci? text "Grant")
            has-work (contains-ci? text "Work")
            has-hydration (or (contains-ci? text "Hydration") (contains-ci? text "hydrate"))
            has-constraint (contains-ci? text "Constraint")] ; optional
        [{:check "invariantsNoDualAnchorQ" :desc "no heritage dual-anchor remains" :pass? (not has-dual-anchor)}
         {:check "invariantsNoCauseSameSessionQ" :desc "no heritage cause same-session remains" :pass? (not has-cause-same)}
         {:check "invariantsHasPrincipalQ" :desc "has Principal invariant" :pass? has-principal}
         {:check "invariantsHasGrantQ" :desc "has Grant invariant" :pass? has-grant}
         {:check "invariantsHasWorkQ" :desc "has Work invariant" :pass? has-work}
         {:check "invariantsHasHydrationQ" :desc "has Hydration invariant" :pass? has-hydration}]))))

(defn scan-docs
  "Full scan. Returns {:w-set :missing :present :refinement-checks :invariants-checks :valid?}."
  [docs-dir]
  (let [{:keys [w-set by-file texts]} (scan-formal docs-dir)
        missing (set/difference expected-w-range w-set)
        present (set/intersection expected-w-range w-set)
        refinement (check-refinement texts)
        invariants (check-invariants docs-dir)
        all-ref-pass? (every? :pass? refinement)
        all-inv-pass? (every? :pass? invariants)
        valid? (and (empty? missing) all-ref-pass? all-inv-pass?)]
    {:w-set w-set
     :by-file by-file
     :missing missing
     :present present
     :refinement-checks refinement
     :invariants-checks invariants
     :valid? valid?
     :unique-count (count w-set)}))

;; --- reporting -------------------------------------------------------------

(defn report-lines
  "Pure report for tests. Returns {:exit-code :present :missing :unique-count :refinement :invariants :invalid-count}."
  [docs-dir]
  (let [{:keys [missing present w-set refinement-checks invariants-checks valid?]} (scan-docs docs-dir)
        invalid-count (+ (count missing)
                         (count (filter (complement :pass?) refinement-checks))
                         (count (filter (complement :pass?) invariants-checks)))]
    {:exit-code (if valid? 0 1)
     :present present
     :missing missing
     :unique-count (count w-set)
     :refinement refinement-checks
     :invariants invariants-checks
     :invalid-count invalid-count
     :total-expected (count expected-w-range)}))

(defn- print-report
  [docs-dir]
  (let [{:keys [missing present w-set refinement-checks invariants-checks valid?]} (scan-docs docs-dir)
        {:keys [by-file]} (scan-formal docs-dir)
        sorted-present (sort present)
        sorted-missing (sort missing)]
    (println (str "Scanning " docs-dir "/formal"))
    (doseq [[path ws] (sort by-file)]
      (let [fname (.getName (io/file path))]
        (println (str "  " fname ": " (count ws) " W tokens -> " (str/join ", " (sort ws))))))
    (println (str "Total distinct W tokens: " (count w-set) "/32"))
    (println (str "Present: " (str/join ", " sorted-present)))
    (when (seq sorted-missing)
      (println (str "MISSING: " (str/join ", " (map #(str "[W-" % "]") sorted-missing)))))
    (println "Refinement checks:")
    (doseq [{:keys [check desc pass?]} refinement-checks]
      (println (str "  " (if pass? "PASS" "FAIL") " " check " — " desc)))
    (println "Invariants checks:")
    (doseq [{:keys [check desc pass?]} invariants-checks]
      (println (str "  " (if pass? "PASS" "FAIL") " " check " — " desc)))
    (println (str "Result: " (if valid? "PASS (0 invalid)" (str "FAIL (" (count sorted-missing) " missing W + "
                                                                (count (filter (complement :pass?) refinement-checks)) " refinement fail + "
                                                                (count (filter (complement :pass?) invariants-checks)) " invariants fail)"))
                  " — exit " (if valid? 0 1)))))

;; --- CLI -------------------------------------------------------------------

(def usage
  "Usage: clojure -M scripts/verify-forms.clj [--fix-hint] [DOCS-DIR]\nScans docs/formal for [W-01..W-32] (32 Wolfram predicates) and V1 refinement invariants.\nExit codes: 0 all 32 present and all refinement/invariants pass; 1 missing/fail; 2 usage error.")

(defn parse-args [args]
  (loop [args args fix-hint false positional []]
    (if (empty? args)
      {:fix-hint fix-hint :docs-dir (or (first positional) "docs")}
      (let [a (first args) r (rest args)]
        (cond
          (= a "--fix-hint") (recur r true positional)
          (= a "--help") {:error usage}
          (str/starts-with? a "-") {:error (str "unknown flag: " a "\n" usage)}
          :else (recur r fix-hint (conj positional a)))))))

(defn -main [& args]
  (let [{:keys [error docs-dir fix-hint]} (parse-args args)]
    (cond
      error (do (println error) (System/exit 2))
      :else
      (let [{:keys [valid? missing present]} (scan-docs docs-dir)
            invalid-count (+ (count missing)
                             (count (filter (complement :pass?) (:refinement-checks (scan-docs docs-dir))))
                             (count (filter (complement :pass?) (:invariants-checks (scan-docs docs-dir)))))]
        (print-report docs-dir)
        (when (and fix-hint (seq missing))
          (println "\nFix hint: ensure docs/formal/*.md each list its [W-XX] table. Missing:")
          (doseq [w (sort missing)]
            (println (str "  [W-" w "] — add row to appropriate model file (perm:01-18, async:19-27, subagent:28-32)"))))
        (System/exit (if valid? 0 1))))))

(when (and (bound? #'*command-line-args*)
           (not (System/getProperty "evoclj.verify-forms.loaded-by-test")))
  (apply -main (or (seq *command-line-args*) [])))
