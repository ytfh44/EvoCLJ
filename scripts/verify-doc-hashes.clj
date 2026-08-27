#!/usr/bin/env clojure
;; ---------------------------------------------------------------------------
;; verify-doc-hashes.clj — WO-T5 documentation commit-hash discipline script
;; ---------------------------------------------------------------------------
;; Usage / README (contract; pinned by evoclj.support.doc-hashes-test):
;;
;;     clojure -M scripts/verify-doc-hashes.clj [--fix-hint] [DOCS-DIR]
;;
;; Recursively scans every markdown file under DOCS-DIR (default "docs") for
;; 7..40 character hexadecimal strings appearing at word boundaries — git
;; abbreviated SHAs and full SHAs — and verifies that every unique candidate
;; resolves to a real commit in the enclosing repository:
;;
;;     git rev-parse --verify <sha>^{commit}
;;
;; Output: per-file PASS/FAIL listing; failing references are reported as
;; FILE:LINE HEX. Exit codes:
;;     0  every referenced hash resolves
;;     1  at least one invalid reference (all occurrences listed)
;;     2  usage or environment error (bad flag, missing DOCS-DIR)
;;
;; --fix-hint additionally prints the raw source line beneath each failing
;; reference so manual repair (work-group D1) is a single lookup away.
;;
;; Exclusion rules (deliberate heuristics, unit-tested in
;; evoclj.support.doc-hashes-test):
;;   E1  word-boundary pure-hex tokens of length 7..40 only. Because a match
;;       must end at a word boundary too, longer hex runs (bare 64-hex
;;       sha256 digests) can never be split into pseudo-refs, and hex glued
;;       into larger tokens (0xdeadbee, _abc1234, cafe1234x) never matches;
;;   E2  `sha256:<hex>` content-addressing digests are stripped from each
;;       line before scanning;
;;   E3  (narrowed in R2 — was: any marker word exempted its WHOLE line,
;;       letting a real stale reference hide behind one stray marker word;
;;       reviewer-demonstrated backdoor, now closed) exemption is
;;       PER-CANDIDATE: a hex is exempt iff (i) it HUGS an example marker
;;       (example/sample/placeholder/dummy/fake/示例/样例/占位, at most 2
;;       non-word characters between them, marker before or after the hex)
;;       or (ii) it is itself placeholder-SHAPED: every character identical
;;       (0000000, ddddddd…) or a canonical dummy constant (deadbee/
;;       deadbeef/cafebabe/…, EXACT match — derivatives like deadbeef123
;;       stay reported). Deliberately narrow: sequential-digit refs like
;;       0123456 stay REPORTED (plausible real abbreviations; fails loud).
;;       A marker word
;;       elsewhere on the line shields NOTHING — such references are reported
;;       normally. Every E3 exemption is surfaced in :skipped-inventory and
;;       printed as a WARN line: skipping is never silent. Unchanged: fenced
;;       blocks whose OPENING HEADER carries a marker skip the whole block;
;;   E4  hex segments of canonical UUIDs (8-4-4-4-12, e.g. goal-ids) are not
;;       commit references; the rule is hex-view-anchored ((?<![0-9a-f])…
;;       (?![0-9a-f])), so UUID shapes glued to neighbouring hex characters
;;       are NOT exempted and their segments stay visible candidates.
;;
;; Known-limitations ledger (documented, accepted):
;;   L1  bare 8-hex words in prose that lexically look like SHAs (e.g. agent
;;       instance ids) pass E1-E4 and rely on git verification; if they do
;;       not resolve they are reported and a human decides. Missing
;;       `git describe`-style g<sha> suffixes is likewise out of scope by
;;       design ("纯 hex" word rule);
;;   L2  unresolvable-but-not-a-ref weak categories are adjudicated MANUALLY
;;       at the D1 handoff (e.g. the two agent instance ids recorded in
;;       docs/codebase/REPAIR-PLAN.md); there is deliberately no --ignore
;;       mechanism (YAGNI);
;;   L3  E4 exempts only CANONICAL UUID shapes; non-canonical identifier
;;       shapes (missing hyphens, wrong widths, glued hex) are NOT exempted
;;       and are reported (fail loud);
;;   L4  R2 elimination note: the former line-level E3 blanket is gone (see
;;       E3). Residual direction: a placeholder declaration separated from
;;       its hex by MORE than 2 non-word characters is intentionally treated
;;       as a real citation — the rule fails loud toward reporting, never
;;       silent toward skipping.
(ns scripts.verify-doc-hashes
  "WO-T5 — documentation commit-hash discipline (standalone script).
See the README comment block at the top of this file for usage, output and
exclusion-rule documentation; the test namespace pins that contract."
  (:require [clojure.string :as str])
  (:import (java.io File)
           (java.nio.file Files LinkOption Paths)))

;; --- lexical rules ---------------------------------------------------------

(def ^:private hex-token-re
  "E1: pure-hex token, 7..40 chars, bounded by non-word characters."
  #"(?i)\b([0-9a-f]{7,40})\b")

(def ^:private sha256-prefix-re
  "E2: content-addressed digest, label + optional spaces + hex run."
  #"(?i)sha256\s*:\s*[0-9a-f]+")

(def ^:private uuid-re
  "E4: canonical UUID (8-4-4-4-12 hex segments), hex-view-anchored on BOTH
  sides (F3): a UUID shape glued to neighbouring hex characters is not a
  canonical identifier and must not be silently stripped."
  #"(?i)(?<![0-9a-f])[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}(?![0-9a-f])")

(def ^:private example-mark-core
  "E3 marker alternation (without pattern flags) shared by the block-header
  check and the adjacency rule below."
  "\\bexamples?\\b|\\bsamples?\\b|\\bplaceholders?\\b|\\bdummies\\b|\\bdummy\\b|\\bfakes?\\b|示例|样例|占位")

(def ^:private example-mark-pattern
  "E3: markers declaring example/placeholder status (block headers, etc.)."
  (re-pattern (str "(?i)(" example-mark-core ")")))

(def ^:private marker-adjacent-pattern
  "E3-narrow (R2): a hex token HUGGING a marker — at most 2 non-word
  characters between them, in either order — reads as a declared
  placeholder. A marker word anywhere else on the line exempts nothing."
  (re-pattern (str "(?i)(?:" example-mark-core ")[\\W_]{0,2}\\b([0-9a-f]{7,40})\\b"
                   "|\\b([0-9a-f]{7,40})\\b[\\W_]{0,2}(?:" example-mark-core ")")))

(def ^:private canonical-dummy-hexes
  "Placeholder-SHAPED allowlist: famous joke constants. EXACT match only —
  derivatives like deadbeef123 stay reported (they are plausible refs)."
  #{"deadbee" "deadbeef" "cafebabe" "cafed00d" "feedface" "d15ea5e"
    "badc0de" "abadcafe" "0ddba11" "defaced"})

(defn- placeholder-form?
  "占位形态 (R2): every character identical, or a canonical dummy constant
  (EXACT match — derivatives like deadbeef123 stay reported). Deliberately
  NARROWER than \"any repetitive look\": sequential-digit refs such as
  0123456/456789a are plausible real abbreviations and stay verified
  candidates (fails loud toward reporting — see ledger L4)."
  [^String hex]
  (boolean (or (contains? canonical-dummy-hexes hex)
               (apply = hex))))

(defn- adjacent-marker-hexes
  "Set of lowercase hex tokens on the scrubbed line that hug an E3 marker."
  [^String s]
  (into #{}
        (comp (mapcat #(remove nil? [(nth % 1 nil) (nth % 2 nil)]))
              (map str/lower-case))
        (re-seq marker-adjacent-pattern s)))

(def ^:private fence-re
  "Markdown fenced-code-block delimiter line (``` or ~~~)."
  #"^\s*(`{3,}|~{3,})")

(defn- example-line?
  [^String s]
  (some? (re-find example-mark-pattern s)))

(defn- scrub-line
  "Remove E2 digests and E4 UUIDs so their hex cannot become candidates."
  [^String line]
  (-> line
      (str/replace sha256-prefix-re " ")
      (str/replace uuid-re " ")))

(defn extract-ref-report
  "Pure core: markdown `text` ->
    {:refs    [{:line <1-based long> :hex <lowercase string>} …]   document order
     :skipped [{:line :hex :reason (:marker-adjacent | :placeholder-form)} …]}
  where :skipped are the E3 exemptions, surfaced so that skipping is NEVER
  silent (R2). Implements rules E1-E4 (see file-header README block). Fenced
  code blocks are tracked only for rule E3 marker inheritance from the
  opening header; unmarked code lines are ordinary candidates (real refs
  often live there)."
  [text]
  (let [lines (str/split-lines (str text))]
    (loop [idx 0, in-block? false, block-header "", refs [], skipped []]
      (if (= idx (count lines))
        {:refs refs :skipped skipped}
        (let [line (nth lines idx)]
          (if (re-find fence-re line)
            ;; fence toggle: entering records the header, leaving clears it
            (recur (inc idx) (not in-block?) (if-not in-block? line "")
                   refs skipped)
            (if (and in-block? (example-line? block-header))
              ;; E3 block inheritance: the whole marked block is out of scope
              (recur (inc idx) in-block? block-header refs skipped)
              (let [ln       (long (inc idx))
                    scrubbed (scrub-line line)
                    adjacent (adjacent-marker-hexes scrubbed)
                    [refs' skipped']
                    (reduce (fn [[r s] [_ tok]]
                              (let [t (str/lower-case tok)]
                                (cond
                                  (contains? adjacent t)
                                  [r (conj s {:line ln :hex t
                                              :reason :marker-adjacent})]
                                  (placeholder-form? t)
                                  [r (conj s {:line ln :hex t
                                              :reason :placeholder-form})]
                                  :else [(conj r {:line ln :hex t}) s])))
                            [[] []]
                            (re-seq hex-token-re scrubbed))]
                (recur (inc idx) in-block? block-header
                       (into refs refs') (into skipped skipped'))))))))))

(defn extract-ref-candidates
  "Backward-compatible view of `extract-ref-report`: just the verified
  candidates ({:line :hex}, document order)."
  [text]
  (:refs (extract-ref-report text)))


;; --- verification ----------------------------------------------------------

(defn sha-resolves?
  "True iff `git rev-parse --verify --quiet <sha>^{commit}` exits 0 when run
  inside `repo-dir`, i.e. the candidate names an existing commit object.

  Timeout policy (F1/R2, honest version): we WAIT up to 20 s for git to
  reach a terminal state BEFORE touching its output — the previous
  slurp-first version blocked until process exit, which made the 20 s
  timeout structurally unreachable and the old \"never hang\" claim false.
  On timeout the process is destroyed and the candidate counts as
  non-resolving (false), with a diagnostic line on stderr. Output is
  drained only AFTER the process has finished; rev-parse emits at most a
  41-byte line, far below any pipe buffer, so reading after waitFor cannot
  deadlock. Determinism policy: never pass silently, never hang."
  [^String repo-dir sha]
  (let [argv (into-array String ["git" "rev-parse" "--verify" "--quiet"
                                 (str sha "^{commit}")])
        pb (doto (java.lang.ProcessBuilder. argv)
             (.directory (java.io.File. repo-dir))
             (.redirectErrorStream true))
        proc (.start pb)
        finished (.waitFor proc 20 java.util.concurrent.TimeUnit/SECONDS)]
    (if-not finished
      (do (.destroyForcibly proc)
          (binding [*out* *err*]
            (println (str "WARN  git timed out after 20000 ms verifying " sha
                          " in " repo-dir
                          " — process destroyed; counted as non-resolving")))
          (try (.close (.getInputStream proc)) (catch Exception _))
          false)
      (let [_ (slurp (.getInputStream proc))]
        (zero? (.exitValue proc))))))

(defn- md-files-under
  "Vector of absolute-path Strings, one per *.md file under `docs-dir`
  (recursive), in lexicographic order for deterministic reporting."
  [^String docs-dir]
  (let [root (.toAbsolutePath (Paths/get docs-dir (make-array String 0)))]
    (when-not (Files/isDirectory root (make-array LinkOption 0))
      (throw (ex-info "docs directory not found" {:docs-dir docs-dir})))
    (->> (file-seq (.toFile root))
         (filter (fn [^java.io.File f] (.isFile f)))
         (filter (fn [^java.io.File f]
                   (.endsWith (str/lower-case (.getName f)) ".md")))
         (map (fn [^java.io.File f] (str (.toAbsolutePath (.toPath f)))))
         sort
         vec)))

(defn scan-docs
  "Core orchestration (pure except the injected/default git verification).

  opts:
    :docs-dir   (default \"docs\")   markdown root scanned recursively
    :repo-dir   (default \".\")       repository SHAs are resolved against
    :fix-hint?  (default false)      attach raw source :line-text to every ref
    :verify-fn  optional (fn [sha] -> boolean), overrides the git check

  returns {:exit-code 0|1
           :files     [{:path <string> :status :pass|:fail :refs [...]}]
                       path-sorted; each ref {:line :hex :valid [:line-text]}
           :invalid-occurrences [{:path :line :hex [:line-text]}]
           :skipped-inventory [{:path :line :hex :reason}] E3 exemptions,
                       WARN-visible (R2): never silent, never a failure
           :unique-count N}           distinct candidate strings verified once
  "
  [{:keys [docs-dir repo-dir fix-hint? verify-fn]
    :or {docs-dir "docs" repo-dir "." fix-hint? false}}]
  (let [verify (or verify-fn (fn [^String sha] (sha-resolves? repo-dir sha)))
        parsed (for [path (md-files-under docs-dir)]
                 (let [text (slurp path)
                       lines (vec (str/split-lines text))
                       report (extract-ref-report text)]
                   {:path path :lines lines
                    :refs (:refs report)
                    :skipped (:skipped report)}))
        hexes (distinct (map :hex (mapcat :refs parsed)))
        verdicts (zipmap hexes (map verify hexes))
        files-out (for [{:keys [path lines refs]} parsed]
                    (let [refs' (mapv (fn [{:keys [line hex] :as r}]
                                        (cond-> (assoc r :valid (get verdicts hex))
                                          fix-hint? (assoc :line-text
                                                           (or (nth lines (dec line) nil) ""))))
                                      refs)]
                      {:path path
                       :status (if (some #(not (:valid %)) refs') :fail :pass)
                       :refs refs'}))
        invalid (vec (for [{:keys [path refs]} files-out
                           {:keys [line hex line-text valid]} refs
                           :when (not valid)]
                       (cond-> {:path path :line line :hex hex}
                         fix-hint? (assoc :line-text line-text))))
        skipped-inventory (vec (for [{:keys [path skipped]} parsed
                                     s skipped]
                                 (assoc s :path path)))]
    {:exit-code (if (seq invalid) 1 0)
     :files (vec files-out)
     :invalid-occurrences invalid
     :skipped-inventory skipped-inventory
     :unique-count (count hexes)}))

;; --- CLI surface (thin; all logic lives above) ------------------------------

(def usage
  "verify-doc-hashes.clj (WO-T5) - documentation commit-hash discipline.

Usage:
    clojure -M scripts/verify-doc-hashes.clj [--fix-hint] [DOCS-DIR]

Recursively scans DOCS-DIR (default \"docs\") for 7..40-char hexadecimal
strings at word boundaries (git abbreviated and full SHAs), excluding
sha256:<hex> content digests and canonical UUID hex segments. Example-marker
exemptions apply PER CANDIDATE only (R2): a hex is exempt iff it hugs an
example marker (example/sample/placeholder/dummy/fake/示例/样例/占位, <=2
non-word characters, either order) or is placeholder-shaped (all-equal
characters, or a canonical dummy constant like deadbeef, EXACT match); a
marker word elsewhere
on a line shields nothing, and EVERY exemption prints a WARN line and lands
in :skipped-inventory - skipping is never silent. Every distinct candidate
is verified against the enclosing repository with:
    git rev-parse --verify <sha>^{commit}

Output: per-file PASS/FAIL listing; failing references as FILE:LINE HEX;
WARN lines for each E3-skipped candidate; a summary line counts both.
--fix-hint additionally prints the raw source line under each failure.

Exit codes: 0 all referenced hashes resolve; 1 at least one invalid reference; 2 usage or environment error.")

(defn parse-args
  "CLI args -> {:fix-hint? b :docs-dir s} on success, or {:exit-code 2
  :reason s} on a usage error (unknown flag, more than one DOCS-DIR)."
  [args]
  (let [[flags positional] (split-with #(str/starts-with? % "--") args)
        unknown (remove #(= "--fix-hint" %) flags)]
    (cond
      (seq unknown)
      {:exit-code 2 :reason (str "unknown flag(s): " (str/join " " unknown))}

      (< 1 (count positional))
      {:exit-code 2 :reason "expected at most one DOCS-DIR argument"}

      :else {:fix-hint? (boolean (some #(= "--fix-hint" %) flags))
             :docs-dir (or (first positional) "docs")})))

(defn report-lines
  "scan-docs result -> vector of human-readable output lines: one PASS/FAIL
  line per file, failing references as \"  FILE:LINE HEX\" (each optionally
  followed by its \"hint>\" raw source line), one \"WARN\" line per E3-skipped
  candidate (R2: exemptions are command-surface visible, never silent), and a
  summary line naming the exit status and both counts."
  [{:keys [files invalid-occurrences skipped-inventory exit-code unique-count]}]
  (vec (concat
        (for [{:keys [path status refs]} files]
          (if (= :pass status)
            (str "PASS " path " (" (count refs) " ref"
                 (when-not (= 1 (count refs)) "s") ")")
            (str "FAIL " path)))
        (mapcat (fn [{:keys [path line hex line-text]}]
                  (concat [(str "  " path ":" line " " hex)]
                          (when line-text [(str "      hint> " line-text)])))
                invalid-occurrences)
        (map (fn [{:keys [path line hex reason]}]
               (str "WARN  skipped-by-E3 (" (name reason) ") "
                    path ":" line " " hex))
             skipped-inventory)
        [(str "Summary: " (count files) " file(s), " unique-count
              " unique ref(s), " (count invalid-occurrences)
              " invalid occurrence(s), "
              (count skipped-inventory) " skipped-by-E3 - exit " exit-code)])))

(defn -main
  "CLI entry point: parse args, scan, print report, exit 0/1/2."
  [& args]
  (let [parsed (parse-args args)]
    (if (:exit-code parsed)
      (do (binding [*out* *err*]
            (println "usage error:" (:reason parsed))
            (println usage))
          (System/exit 2))
      (let [res (try
                  (scan-docs {:docs-dir (:docs-dir parsed)
                              :repo-dir "."
                              :fix-hint? (:fix-hint? parsed)})
                  (catch Exception e
                    (binding [*out* *err*]
                      (println "environment error:" (.getMessage e))
                      (println usage))
                    ::env-error))]
        (if (= ::env-error res)
          (System/exit 2)
          (do (doseq [line (report-lines res)]
                (println line))
              (System/exit (:exit-code res))))))))

;; --- the script entry point ---------------------------------------------------
;;
;; `clojure -M scripts/verify-doc-hashes.clj [--fix-hint] [DOCS-DIR]` loads
;; this file as a script and runs the top-level form below. The test suite
;; load-files the SAME file in-process; to keep that load a pure definition
;; (no scan, no System/exit), tests set the system property
;; evoclj.doc-hashes.loaded-by-test before load-file and this runner is
;; gated on its absence (same convention as scripts/full-cycle.clj).

(when (and (bound? #'*command-line-args*)
           (not (System/getProperty "evoclj.doc-hashes.loaded-by-test")))
  (apply -main (or (seq *command-line-args*) [])))
