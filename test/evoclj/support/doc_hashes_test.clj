(ns evoclj.support.doc-hashes-test
  "WO-T5 — documentation commit-hash discipline script (logic-side tests).

  The script lives at scripts/verify-doc-hashes.clj, outside the classpath,
  and is loaded explicitly (load-file) after locating the project root by
  walking up from the cwd to deps.edn. All assertions are driven by inline
  markdown text or temporary-directory fixtures — none depend on the real
  docs/ contents. The one real-docs touchpoint (WO path 5 regression) only
  asserts structural sanity, never validity, because fixing existing docs
  belongs to D1.

  Required coverage (WO 必测路径):
   1. happy    — a temp md citing a REAL HEAD hash passes (exit 0);
   2. branch   — a 7-char abbreviation and the full 40-char SHA both resolve;
   3. fault    — a fabricated hash exits 1 and the report points at file:line;
   4. boundary — sha256:<64hex> content-addressed digests (and bare 64-hex
                 runs, english hex-letter words like \"decade\", UUID hex
                 segments) are NOT flagged as commit references; E3 example-
                 marker exemptions apply PER CANDIDATE only (marker-hugging
                 or placeholder-shaped hex) and are always WARN-visible —
                 every exclusion rule is explicit and tested;
   5. regression — a structured run against the current real docs/ completes
                 and reports per-ref file:line data (inventory goes to the
                 work-group report; docs themselves untouched);
   6. contract — the script's README/usage comment matches observable
                 behavior (--fix-hint flag, exit codes 0/1/2).

  Exclusion-rule ledger (mirrors the script docstring, kept in lockstep;
  E3 narrowed + made visible in R2):
    E1 word-boundary pure-hex tokens of length 7..40 only — a longer hex run
       (e.g. a bare sha256 digest) can therefore never match partially;
    E2 sha256:<hex> content-addressing digests are stripped before scanning;
    E3 PER-CANDIDATE exemption only (R2): a hex is exempt iff it hugs an
       example marker (example/sample/placeholder/dummy/fake/示例/样例/占位,
       <=2 non-word characters between them, marker before or after the hex)
       OR it is itself placeholder-shaped: every character identical, or a
       canonical dummy constant such as deadbeef (EXACT match — derivatives
       like deadbeef123 stay reported). Deliberately narrow: sequential-digit
       refs like 0123456 stay REPORTED (plausible real abbreviations; the
       rule fails loud toward reporting). A marker word ELSEWHERE on a line
       shields nothing; every E3 exemption surfaces in :skipped-inventory and
       prints as a WARN line. Block-level inheritance (a fenced block whose
       OPENING HEADER carries a marker skips the whole block) is unchanged;
    E4 hex segments of canonical UUIDs (8-4-4-4-12) are not commit refs; the
       rule is hex-view-anchored, so UUID shapes glued to neighbouring hex
       characters are NOT exempted (their segments stay visible candidates);
    L  known limitations mirror the script header: agent-instance-id-style
       8-hex words remain reported for human adjudication (deliberately no
       --ignore mechanism, YAGNI); unresolvable-but-not-a-ref weak categories
       are adjudicated manually at D1 handoff."
  (:require [clojure.test :refer [deftest is testing]])
  (:import (java.nio.file Files LinkOption Path Paths)))

(defn- find-project-root
  "^String — nearest ancestor directory of the cwd containing deps.edn."
  ^String []
  (let [cwd (Paths/get (System/getProperty "user.dir") (make-array String 0))]
    (loop [^Path dir cwd]
      (cond
        (nil? dir)
        (throw (ex-info "project root with deps.edn not found above cwd"
                        {:cwd (str cwd)}))

        (Files/exists (.resolve dir "deps.edn")
                      (make-array LinkOption 0))
        (str dir)

        :else (recur (.getParent dir))))))

(def ^:private ^String project-root (find-project-root))

(def ^:private ^String script-path
  (str project-root (System/getProperty "file.separator") "scripts"
       (System/getProperty "file.separator") "verify-doc-hashes.clj"))

(when-not (Files/exists (Paths/get script-path (make-array String 0))
                        (make-array LinkOption 0))
  (throw (ex-info "WO-T5 script not found (expected scripts/verify-doc-hashes.clj)"
                  {:script script-path :project-root project-root})))

;; keep the in-process load a pure definition: gate off the script's own CLI
;; entry point (same convention as scripts/full-cycle.clj's runner property)
(System/setProperty "evoclj.doc-hashes.loaded-by-test" "true")
(load-file script-path)
(require 'scripts.verify-doc-hashes)
(alias 'vdh 'scripts.verify-doc-hashes)
(System/clearProperty "evoclj.doc-hashes.loaded-by-test")

(require '[clojure.string :as str]
         '[clojure.java.shell :refer [sh]])
(import '(java.nio.file Files Paths LinkOption)
        '(java.nio.file.attribute FileAttribute))

;; ---------------------------------------------------------------------------
;; Fixtures — self-contained temporary repositories/docs (no real docs/ reads)
;; ---------------------------------------------------------------------------

(def ^:private fs-sep (System/getProperty "file.separator"))

(defn- tmp-dir!
  "^String fresh temp directory."
  ^String [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- write-file!
  "^String absolute path — writes UTF-8 `content` to dir/rel creating parents."
  [^String dir rel ^String content]
  (let [p (.resolve (Paths/get dir (make-array String 0)) rel)]
    (Files/createDirectories (.getParent p) (make-array FileAttribute 0))
    (Files/write p (.getBytes ^String content "UTF-8")
                 (make-array java.nio.file.OpenOption 0))
    (str p)))

(defn- git*
  "Run a fixture git command in `dir`; throws on non-zero exit."
  [^String dir & args]
  (let [res (apply sh "git" (concat args [:dir dir]))]
    (when-not (zero? (:exit res))
      (throw (ex-info "fixture git command failed"
                      {:args (vec args) :dir dir :exit (:exit res) :err (:err res)})))
    res))

(defn- make-temp-repo!
  "One-commit throwaway repository. Returns
  {:root dir :docs dir/docs :head full-40-sha :short 7-char-sha}."
  []
  (let [root (tmp-dir! "wo-t5-repo-")]
    (git* root "init" "-q")
    (git* root "config" "user.email" "wo-t5@example.invalid")
    (git* root "config" "user.name" "WO-T5 Fixture")
    (write-file! root "docs/seed.md" "seed document\n")
    (git* root "add" ".")
    (git* root "commit" "-q" "-m" "init")
    (let [head  (str/trim (:out (git* root "rev-parse" "HEAD")))
          short (str/trim (:out (git* root "rev-parse" "--short=7" "HEAD")))]
      {:root root :docs (str root fs-sep "docs") :head head :short short})))

;; ---------------------------------------------------------------------------
;; Verification orchestration over a real temp git repository
;; ---------------------------------------------------------------------------

(deftest scan-docs-happy-real-head-full-and-short-both-resolve
  (testing "WO path 1 (happy): a temp md citing the REAL HEAD hash passes with exit 0"
    (let [{:keys [root docs head]} (make-temp-repo!)
          _ (write-file! docs "happy.md" (str "# refs\ncited: " head "\n"))
          res (vdh/scan-docs {:docs-dir docs :repo-dir root})
          happy (some #(when (.endsWith ^String (:path %) "happy.md") %)
                      (:files res))]
      (is (= 0 (:exit-code res)))
      ;; fixture also contains the init-commit seed doc — every file passes
      (is (every? #(= :pass (:status %)) (:files res)))
      (is (= :pass (:status happy)))
      (is (= [{:line 2 :hex head :valid true}] (:refs happy))
          "the real HEAD hash is extracted from line 2 and resolves")
      (is (empty? (:invalid-occurrences res)))))
  (testing "WO path 2 (branch): 7-char abbreviation resolves alongside the full SHA"
    (let [{:keys [root docs head short]} (make-temp-repo!)
          _ (write-file! docs "abbrev.md"
                         (str "full " head "\nabbrev " short "\n"))
          res (vdh/scan-docs {:docs-dir docs :repo-dir root})
          refs (mapcat :refs (:files res))]
      (is (= 0 (:exit-code res)))
      (is (= #{head short} (set (map :hex refs))))
      (is (= 2 (:unique-count res)) "distinct candidate strings verified once each")
      (is (every? :valid refs)))))

(deftest scan-docs-fault-fabricated-hash-fails-at-file-line
  (testing "WO path 3 (fault): fabricated hash -> exit 1, report names file AND line"
    (let [{:keys [root docs head]} (make-temp-repo!)
          _ (write-file! docs "mixed.md"
                         (str "line1 good " head "\n"
                              "line2 bogus deadbeef123\n"
                              "line3 again deadbeef123\n"))
          _ (write-file! docs "clean.md" (str "ok " head "\n"))
          res (vdh/scan-docs {:docs-dir docs :repo-dir root})]
      (is (= 1 (:exit-code res)))
      (is (= [{:path (str root fs-sep "docs" fs-sep "mixed.md")
               :line 2 :hex "deadbeef123"}
              {:path (str root fs-sep "docs" fs-sep "mixed.md")
               :line 3 :hex "deadbeef123"}]
             (:invalid-occurrences res))
          "every failing OCCURRENCE listed as file:line, in document order")
      (is (= 2 (:unique-count res)) "deduped verification: {head, deadbeef123}")
      (let [mixed (some #(when (.endsWith ^String (:path %) "mixed.md") %) (:files res))
            clean (some #(when (.endsWith ^String (:path %) "clean.md") %) (:files res))]
        (is (= :fail (:status mixed)))
        (is (= :pass (:status clean)))))))

(deftest regression-real-docs-run-is-structurally-sound
  (testing "WO path 5 (regression): the CURRENT real docs/ scans end-to-end;
            structural sanity only — existing broken hashes are D1's job, the
            inventory below goes to the work-group report verbatim"
    (let [res (vdh/scan-docs {:docs-dir "docs" :repo-dir "." :fix-hint? true})]
      (is (map? res))
      (is (pos? (count (:files res))) "the repo does have markdown docs")
      (is (every? #(and (contains? % :path) (contains? % :status)
                        (vector? (:refs %)))
                  (:files res)))
      (is (every? #(and (contains? % :line) (contains? % :hex)
                        (boolean? (contains? % :valid)) (string? (:line-text %)))
                  (mapcat :refs (:files res)))
          "fix-hint mode carries the raw source line for every ref")
      (is (contains? #{0 1} (:exit-code res))
          "real docs may legitimately contain stale hashes pre-D1")
      (println "[WO-T5 path-5] real docs/:"
               (count (:files res)) "files,"
               (:unique-count res) "unique refs, exit" (:exit-code res))
      (doseq [o (:invalid-occurrences res)]
        (println "  INVALID" (:path o) ":" (:line o) (:hex o)))
      (doseq [s (:skipped-inventory res)]
        (println "  SKIPPED-E3(WARN)" (:path s) ":" (:line s) (:hex s)
                 (name (:reason s)))))))

;; ---------------------------------------------------------------------------
;; fix-hint, verifier injection, and the usage contract
;; ---------------------------------------------------------------------------

(deftest fix-hint-attaches-raw-source-lines
  (testing "--fix-hint: every reported occurrence carries the raw source line;
            without the flag no :line-text is attached"
    (let [{:keys [root docs head]} (make-temp-repo!)
          line2 "line2 bogus deadbeef123"
          _ (write-file! docs "hinted.md"
                         (str "line1 good " head "\n" line2 "\n"))
          with-hint (vdh/scan-docs {:docs-dir docs :repo-dir root :fix-hint? true})
          no-hint   (vdh/scan-docs {:docs-dir docs :repo-dir root})]
      (is (= [{:path (str root fs-sep "docs" fs-sep "hinted.md")
               :line 2 :hex "deadbeef123" :line-text line2}]
             (:invalid-occurrences with-hint)))
      (is (every? #(not (contains? % :line-text))
                  (:invalid-occurrences no-hint)))
      (is (= 1 (:exit-code with-hint))))))

(deftest scan-docs-accepts-injected-verifier
  (testing ":verify-fn overrides the git check (logic testable without git);
            verdicts are memoized per distinct hex (WO: 去重后批量校验)"
    (let [root (tmp-dir! "wo-t5-nogit-")
          docs-file (write-file! root "docs.md"
                                 (str "keep 1a2b3c4\ndrop 9f8e7d6\nagain 9f8e7d6\n"))
          calls (atom [])
          res (vdh/scan-docs
               {:docs-dir root :repo-dir root
                :verify-fn (fn [sha]
                             (swap! calls conj sha)
                             (= "1a2b3c4" sha))})]
      (is (= 1 (:exit-code res)))
      (is (= ["1a2b3c4" "9f8e7d6"] @calls)
          "each distinct candidate string verified exactly once")
      (is (= [{:path docs-file :line 2 :hex "9f8e7d6"}
              {:path docs-file :line 3 :hex "9f8e7d6"}]
             (:invalid-occurrences res))))))

(deftest contract-usage-and-argument-handling
  (testing "WO path 6 (contract): the README/usage text matches observable behavior"
    (is (str/includes? vdh/usage "clojure -M scripts/verify-doc-hashes.clj")
        "documented invocation is the one that actually works")
    (is (str/includes? vdh/usage "--fix-hint") "flag documented")
    (is (str/includes? vdh/usage "DOCS-DIR") "positional docs dir documented")
    (is (re-find #"Exit codes:\s*0[^\n]*1[^\n]*2" vdh/usage)
        "exit codes documented as an ordered 0/1/2 sequence (F4: structural
         assertion, not three independent substring probes)"))
  (testing "argument parsing: --fix-hint flag and optional positional DOCS-DIR"
    (is (= {:fix-hint? false :docs-dir "docs"} (vdh/parse-args [])))
    (is (= {:fix-hint? true :docs-dir "docs"} (vdh/parse-args ["--fix-hint"])))
    (is (= {:fix-hint? false :docs-dir "other"} (vdh/parse-args ["other"])))
    (is (= {:fix-hint? true :docs-dir "other"} (vdh/parse-args ["--fix-hint" "other"]))))
  (testing "unknown flags and extra arguments are usage errors (exit 2), never crashes"
    (is (= 2 (:exit-code (vdh/parse-args ["--bogus"]))))
    (is (= 2 (:exit-code (vdh/parse-args ["--bogus" "docs"]))))
    (is (= 2 (:exit-code (vdh/parse-args ["a" "b"])))))
  (testing "report rendering: per-file PASS/FAIL plus FILE:LINE HEX failure list"
    (let [{:keys [root docs head]} (make-temp-repo!)
          _ (write-file! docs "r.md" (str "good " head "\nbogus deadbeef123\n"))
          res (vdh/scan-docs {:docs-dir docs :repo-dir root})
          out (vdh/report-lines res)
          text (str/join "\n" out)]
      (is (some #(.startsWith ^String % "PASS") out) "passing files listed")
      (is (some #(.startsWith ^String % "FAIL") out) "failing files listed")
      (is (re-find #"r\.md:2 deadbeef123" text)
          "failure entries are FILE:LINE HEX")
      (is (re-find #"(?i)exit" text) "summary names the exit status"))))



;; ---------------------------------------------------------------------------
;; Extraction core (pure): word boundaries, length window, exclusion rules
;; ---------------------------------------------------------------------------

(deftest extract-finds-abbrev-and-full-sha-at-word-boundaries
  (testing "WO path 2 (branch): 7-char abbreviation AND full 40-char SHA extracted"
    (is (= [{:line 1 :hex "abc1234"}]
           (vdh/extract-ref-candidates "see commit abc1234 for context")))
    (let [full "0123456789abcdef0123456789abcdef01234567"]
      (is (= [{:line 1 :hex full}]
             (vdh/extract-ref-candidates (str "rebase onto " full " done"))))))
  (testing "uppercase hex accepted and normalized to lowercase (git refs are case-insensitive)"
    (is (= [{:line 1 :hex "abc1234"}]
           (vdh/extract-ref-candidates "commit ABC1234 landed"))))
  (testing "occurrences carry 1-based line numbers in document order"
    (is (= [{:line 2 :hex "9f2c1ab"}]
           (vdh/extract-ref-candidates "intro line\nthen 9f2c1ab lands\nepilogue"))
        "R2: fixtures use random-format hex — deadbee itself is (correctly)
         placeholder-shaped now and covered by the E3 exemption tests")
    (is (= [{:line 1 :hex "1a2b3c4"} {:line 2 :hex "9f8e7d6"}]
           (vdh/extract-ref-candidates "one 1a2b3c4\ntwo 9f8e7d6"))
        "R2: all-same-digit aaaaaaa/bbbbbbb would be placeholder-exempt,
         so these pins use random-format hex instead")))

(deftest extract-rejects-non-hash-lexical-contexts
  (testing "english hex-letter words below the 7-char floor never match ('decade', 'faced')"
    (is (= [] (vdh/extract-ref-candidates
               "a decade of commits faced with feedback"))))
  (testing "hex embedded in larger alphanumeric tokens is not at a word boundary"
    (is (= [] (vdh/extract-ref-candidates
               "ids _abc1234, 0xdeadbee, vff3d641 and cafe1234x stay put"))))
  (testing "E1: bare 64-hex sha256 digest is outside the 7..40 window and is never split into pseudo-refs"
    (let [d64 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"]
      (is (= [] (vdh/extract-ref-candidates (str "digest " d64 " end"))))))
  (testing "E1: 45-hex blob digests likewise never match"
    (is (= [] (vdh/extract-ref-candidates
               (str "blob 1234567890abcdef1234567890abcdef1234567890abc here"))))))

(deftest extract-excludes-content-addressed-digests
  (testing "E2/WO path 4: sha256:<64hex> content addressing is not a commit reference"
    (let [d64 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"]
      (is (= [] (vdh/extract-ref-candidates
                 (str "tree digest = sha256:" d64 " (canonical empty tree)"))))))
  (testing "E2: short sha256:-prefixed forms are excluded too"
    (is (= [] (vdh/extract-ref-candidates "sha256:deadbee1 short form"))))
  (testing "E2: a genuine ref on the SAME line survives the digest stripping"
    (is (= [{:line 1 :hex "ff3d641"}]
           (vdh/extract-ref-candidates
            "payload sha256:00112233445566 committed as ff3d641")))))

(deftest extract-excludes-canonical-uuid-hex-segments
  (testing "E4: the 8-4-4-4-12 segments of a UUID are not commit references"
    (is (= [] (vdh/extract-ref-candidates
               "Goal: goal-b52cbdb6-7e7f-44dc-a9b6-7e88a0619dc0 · 分支: fix/mcp-skills-closure")))))

(deftest extract-skips-example-marked-placeholders
  (testing "E3a (narrowed R2): marker-HUGGING hex yields no candidates"
    (doseq [marked ["hash abc1234 (example only)"
                    "占位 abc1234 非真实引用"
                    "placeholder: deadbee1"
                    "example: cafebabe"
                    "abc1234 样例"]]
      (is (= [] (vdh/extract-ref-candidates marked)) (pr-str marked))))
  (testing "E3-narrow R2: placeholder-SHAPED hex is exempt even with NO marker word"
    (doseq [ph ["0000000" "ddddddd" "cafebabe"]]
      (is (= [] (vdh/extract-ref-candidates (str "demo " ph " prose"))) ph)))
  (testing "E3b: inside a fenced block whose OPENING HEADER carries a marker, nothing is scanned"
    (is (= [] (vdh/extract-ref-candidates
               "```text ; 示例输出（非真实）\nroot abc1234\n```"))))
  (testing "E3c: a marked line inside an otherwise plain block excludes just that line"
    (is (= [{:line 3 :hex "9f2c1ab"}]
           (vdh/extract-ref-candidates
            "```clojure\n;; example: abc1234\nreal ref 9f2c1ab here\n```"))))
  (testing "plain fenced code without markers is still scanned"
    (is (= [{:line 2 :hex "ff3d641"}]
           (vdh/extract-ref-candidates "```bash\ngit show ff3d641\n```")))))

(deftest e3-narrowed-marker-word-cannot-hide-real-reference
  (testing "E3/R2 red->green attack replay: a real-format stale reference
            sharing a line with an example marker word MUST be reported —
            the old line-level blanket exemption silently hid it"
    (is (= [{:line 1 :hex "deadbeef999"}]
           (vdh/extract-ref-candidates
            "the stale pointer deadbeef999 must be repaired (sample prose)"))
        "marker word far from the hex no longer exempts it")
    (is (= [{:line 1 :hex "3f9a2c1"}]
           (vdh/extract-ref-candidates
            "example walkthrough below; broken ref 3f9a2c1 needs fix"))
        "'example' earlier on the line does not shield a later ref")
    (is (= [{:line 1 :hex "3f9a2c1"}]
           (vdh/extract-ref-candidates
            "占位说明见上；真实失效引用 3f9a2c1 需修复"))
        "Chinese-marker distance likewise fails loud"))
  (testing "a word between marker and hex breaks adjacency (>2 non-word chars)"
    (is (= [{:line 1 :hex "deadbee12"}]
           (vdh/extract-ref-candidates "SAMPLE OUTPUT deadbee12"))))
  (testing "end-to-end: scan-docs lists the co-linear reference as INVALID"
    (let [root (tmp-dir! "wo-t5-e3atk-")
          f (write-file! root "attack.md"
                         "the stale pointer deadbeef999 must be repaired (sample prose)\n")
          res (vdh/scan-docs {:docs-dir root :repo-dir "."
                              :verify-fn (constantly false)})]
      (is (= 1 (:exit-code res)))
      (is (= [{:path f :line 1 :hex "deadbeef999"}]
             (:invalid-occurrences res))
          "the adversarial co-linear ref surfaces (silently skipped pre-R2)")
      (is (empty? (:skipped-inventory res))
          "nothing on this line qualified for an exemption"))))

(deftest e3-exemptions-are-placeholder-or-adjacent-only
  (testing "placeholder-form unit grid: all-same runs and canonical dummy
            constants only (exact match)"
    (doseq [ph ["0000000" "aaaaaaaa" "fffffffffff" "ddddddd"
                "deadbee" "deadbeef" "cafebabe"]]
      (is (= [] (vdh/extract-ref-candidates (str "demo " ph " prose"))) ph)))
  (testing "realistic or plausible refs are NEVER placeholder-shaped —
            sequential-digit abbreviations deliberately stay REPORTED
            (they could be real; fails loud, ledger L4)"
    (doseq [real ["1a2b3c4" "deadbeef123" "9f8e7d6" "ff3d641" "a1b2c3d"
                  "0123456" "456789a" "fedcba9" "01234567"
                  "0123456789abcdef0123456789abcdef01234567"]]
      (is (= [{:line 1 :hex real}]
             (vdh/extract-ref-candidates (str "demo " real " prose")))
          (str real " must stay a verified candidate"))))
  (testing "adjacency window: <=2 non-word gap chars exempt, wider is not"
    (is (= [] (:refs (vdh/extract-ref-report "example: abc1234"))))
    (is (= [] (:refs (vdh/extract-ref-report "示例：abc1234"))))
    (is (= [] (:refs (vdh/extract-ref-report "占位abc1234"))))
    (is (= [{:line 1 :hex "abc1234"}]
           (:refs (vdh/extract-ref-report "example -- abc1234")))
        "gap of 3 non-word chars => reported, not exempt")))

(deftest e3-skips-are-warn-visible-in-report-not-silent
  (testing "pipeline: exemptions land in :skipped-inventory and print WARN;
            they never affect :exit-code and never hide genuine refs"
    (let [{:keys [root docs head]} (make-temp-repo!)
          _ (write-file! docs "warn.md"
                         (str "demo 0000000 untouched\n"
                              "example: deadbee1\n"
                              "cited " head " for real\n"))
          res (vdh/scan-docs {:docs-dir docs :repo-dir root})
          out (vdh/report-lines res)
          text (str/join "\n" out)]
      (is (= 0 (:exit-code res)) "exemptions are not failures")
      (is (= [{:line 1 :hex "0000000" :reason :placeholder-form}
              {:line 2 :hex "deadbee1" :reason :marker-adjacent}]
             (map #(select-keys % [:line :hex :reason])
                  (:skipped-inventory res)))
          "both exemption reasons recorded in document order")
      (is (some #(re-find #"^WARN\b.*0000000" %) out)
          "WARN line names the all-zero placeholder hex")
      (is (some #(re-find #"^WARN\b.*deadbee1" %) out)
          "WARN line names the marker-adjacent hex")
      (is (re-find #"2 skipped-by-E3" text) "summary counts the skips")
      (is (= [{:line 3 :hex head :valid true}]
             (map #(select-keys % [:line :hex :valid])
                  (mapcat :refs (:files res))))
          "only the genuine ref remains a verified candidate"))))

(deftest e4-uuid-exemption-is-hex-view-anchored
  (testing "F3: canonical standalone UUID still fully stripped (E4 unchanged)"
    (is (= [] (vdh/extract-ref-candidates
               "goal-b52cbdb6-7e7f-44dc-a9b6-7e88a0619dc0 · done"))))
  (testing "UUID shape glued to neighbouring hex characters is NOT canonical
            E4: the anchored scrub refuses mid-token strips, segments stay
            visible candidates (fail loud — ledger L3)"
    (is (= [{:line 1 :hex "ab52cbdb6"} {:line 1 :hex "7e88a0619dc0"}]
           (vdh/extract-ref-candidates
            "ab52cbdb6-7e7f-44dc-a9b6-7e88a0619dc0 chained"))
        "lookbehind: leading hex char blocks the UUID strip (pre-R2: silent [])")
    (is (= [{:line 1 :hex "b52cbdb6"} {:line 1 :hex "7e88a0619dc0ff"}]
           (vdh/extract-ref-candidates
            "b52cbdb6-7e7f-44dc-a9b6-7e88a0619dc0ff chained"))
        "lookahead: trailing hex char blocks the strip too")))
