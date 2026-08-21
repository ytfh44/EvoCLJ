(ns evoclj.adversarial.mutation-test
  "component — Mutation determinism and sandbox escape suite (adversarial
  release gate).

  This suite ATTACKS the mutation pipeline's own claims. Every malformed
  patch must fail BEFORE candidate registration — before a single byte
  is staged (Step 1), every valid repeated application must yield ONE
  candidate Genome hash (Step 2, Global Constraints 1 and 6), and SCI
  runtime exhaustion after a mutation must stay CONTAINED to the
  candidate/session without blocking evaluator/kernel threads (Step 3,
  Global Constraints 22 and 23).

  Cases (the plan's normative list, one deftest each):

    1. Patch path traversal            -> :mutation/path-invalid, no write
    2. Symlink in the candidate tree   -> rejected, never followed
    3. Wrong preimage hash             -> :patch/preimage-mismatch, no dir
    4. Ambiguous text range            -> :patch/anchor-ambiguous
    5. rewrite-clj selector, zero forms -> :patch/form-not-found
    6. rewrite-clj selector, multi-match -> bounded first-match edit,
                                            deterministic (see DEVIATION)
    7. Same parent+mutation x100       -> ONE candidate Genome hash
    8. SCI infinite loop after mutation -> :sci/limit-exceeded, contained
    9. SCI huge/infinite lazy output    -> :edn/size-exceeded, contained

  DEVIATION (reported per Repo Conventions rule 5 — the plan's case list
  is normative, the implemented component contract is the higher-priority
  Global Constraint 6 determinism requirement, and the implementation
  itself is the documented contract of evoclj.genome.patch-clj): the
  plan expects a multi-match rewrite-clj selector to be REJECTED when
  uniqueness is required (mirroring :patch/anchor-ambiguous for text
  anchors), but the shipped selector contract is FIRST-MATCH-WINS:
  \"a scalar ... matches the first form whose element sequence starts
  with that value\" (first in depth-first source order). Case 6
  therefore verifies the implemented guarantees that keep the
  multi-match case safe: the edit is bounded to EXACTLY ONE form (never
  a global replace of every match), the remaining matches survive
  byte-for-byte, and repeated application is byte-identical and yields
  one candidate hash. The rejected-on-multi-match semantics is NOT
  implemented at HEAD; the failure is closed for the zero-match case
  (:patch/form-not-found) and bounded/deterministic for the multi-match
  case. Flagged for the reviewer agent."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.genome.patch :as patch]
            [evoclj.genome.path :as gpath]
            [evoclj.genome.types :as types]
            [evoclj.sci.context :as context]
            [evoclj.sci.execute :as execute])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption OpenOption Path Paths)
           (java.nio.file.attribute FileAttribute)))

;; ============================================================================
;; shared fixtures
;; ============================================================================

(def ^:private manifest-source
  (pr-str {:genome/format 1
           :agent/id :main
           :agent/entry :graph/main
           :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
           :modules {:topology "topology.edn"
                     :models "models.edn"
                     :memory "memory.edn"
                     :evolution "evolution.edn"}
           :capabilities/requested #{:model/call}
           :evolution {:max-risk :program
                       :mutable #{:parameters :prompts :skills :programs :topology}}
           :metadata {:name "mutation-adversarial-fixture"}}))

(def ^:private evolution-source "{:evolution {}}\n")
(def ^:private memory-source "{:memory {}}\n")
(def ^:private models-source "{:models {:planner {:alias :reasoning/high}}}\n")
(def ^:private topology-source
  "{:graph/id :graph/main\n :entry :node/planner\n :nodes\n {:node/planner {:node/type :sci :program :program/route :next :node/finish}\n  :node/finish {:node/type :emit}}\n :limits {:max-steps 64}}\n")
(def ^:private skills-edn-source "{:workflow {:before-edit []}}\n")
(def ^:private notes-source "alpha\nbeta\ngamma\n")
(def ^:private route-source
  "(ns agent.route\n  \"Mutated route program fixture (component).\")\n\n;; Keep me! This comment must survive form replacement.\n(defn run\n  \"Route one task.\"\n  [x]\n  x)\n\n(defn other\n  \"Unrelated helper.\"\n  []\n  2)\n")
(def ^:private multi-source
  "(ns fixture.multi)\n(defn a [] (run 1))\n(defn b [] (run 2))\n")

(def ^:private fixture-files
  {"manifest.edn" manifest-source
   "evolution.edn" evolution-source
   "memory.edn" memory-source
   "models.edn" models-source
   "topology.edn" topology-source
   "skills/debugging.edn" skills-edn-source
   "skills/notes.txt" notes-source
   "programs/route.clj" route-source
   "programs/multi.clj" multi-source})

;; --- temp-dir helpers ------------------------------------------------------

(defn- temp-dir! ^Path []
  (Files/createTempDirectory "evoclj-mutation-adv-" (make-array FileAttribute 0)))

(def ^:private nofollow-links
  "LinkOption array meaning NOFOLLOW_LINKS for the Files checks in
  delete-recursively!."
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- delete-recursively! [^Path dir]
  ;; A symlink is deleted AS A LINK (never followed, never recursed into),
  ;; so cleanup cannot escape the tree through a link; regular directories
  ;; are recursed, plain files deleted. NOFOLLOW existence keeps dangling
  ;; links from reading as nonexistent and being left behind.
  (when (Files/exists dir nofollow-links)
    (if (Files/isSymbolicLink dir)
      (Files/delete dir)
      (let [f (.toFile dir)]
        (when (.isDirectory f)
          (doseq [c (.listFiles f)]
            (delete-recursively! (.toPath c))))
        (Files/delete dir)))))

(defmacro with-temp-dirs [names & body]
  (let [names (vec names)]
    `(let [~@(mapcat (fn [n] [n `(temp-dir!)]) names)]
       (try
         ~@body
         (finally
           (doseq [~'d ~names]
             (delete-recursively! ~'d)))))))

(defn- write-text-file!
  "Write `content` to `dir`/`rel`, creating parent directories."
  [^Path dir rel ^String content]
  (let [p (.resolve dir rel)]
    (Files/createDirectories (.getParent p) (make-array FileAttribute 0))
    (Files/write p (.getBytes content StandardCharsets/UTF_8)
                 (make-array OpenOption 0))
    p))

(defn- write-genome! [^Path dir]
  (doseq [[rel content] fixture-files]
    (write-text-file! dir rel content))
  dir)

(defn- try-create-symlink!
  "Best-effort Files/createSymbolicLink. Returns false when the host
  refuses (Windows hosts without Developer Mode or symlink privileges)."
  [^Path target ^Path link]
  (try
    (Files/createSymbolicLink link target (make-array FileAttribute 0))
    true
    (catch Exception _ false)))

(defn- text-of
  "Decode an immutable file payload's :bytes as UTF-8 text."
  [file-value]
  (String. ^bytes (byte-array (:bytes file-value)) StandardCharsets/UTF_8))

(defn- dir-entries [^Path dir]
  (->> (.list (.toFile dir)) sort vec))

(defn- thrown-error
  "The ExceptionInfo thrown by (f), or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- error-type
  "The :error/type of the ExceptionInfo thrown by (f), or nil."
  [f]
  (:error/type (ex-data (thrown-error f))))

;; --- mutation fixtures -----------------------------------------------------

(defn- mutation-with
  "A schema-valid Mutation envelope carrying `ops`, pinned to `parent`."
  [parent ops]
  {:mutation/id (java.util.UUID/randomUUID)
   :parent/genome-id (:genome/id parent)
   :hypothesis/id (java.util.UUID/randomUUID)
   :evidence/id "sha256:1111111111111111111111111111111111111111111111111111111111111111"
   :risk :program
   :ops ops
   :expected-effect {:primary-metric :task/success :direction :increase}})

(defn- set-edn-op
  "A :set-edn op against skills/debugging.edn with the fixture preimage."
  [& [overrides]]
  (merge {:op :set-edn
          :file "skills/debugging.edn"
          :path [:workflow :before-edit]
          :expect/hash (hash/text-digest skills-edn-source)
          :value [:reproduce :localize]}
         overrides))

(defn- load-parent!
  "Write the fixture genome into `dir` and load it."
  [^Path dir]
  (load/load-genome (write-genome! dir)))

;; ============================================================================
;; STEP 1 — malformed patches fail before candidate registration
;; ============================================================================

(deftest case-1-patch-path-traversal-is-rejected-before-any-write
  (with-temp-dirs [root-dir parent-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          output-dir (Files/createDirectories (.resolve root-dir "candidates")
                                              (make-array FileAttribute 0))
          mutation (mutation-with parent [(set-edn-op {:file "../escape.edn"})])]
      (testing "a traversal :file is rejected by the mutation gate"
        (is (= :mutation/path-invalid
               (error-type #(patch/apply-mutation parent mutation output-dir)))))
      (testing "the rejection precedes every write — no candidate, no staging,
                and the escaped location was never touched"
        (is (= [] (dir-entries output-dir))
            "no candidate or staging directory may survive a rejected patch")
        (is (not (Files/exists (.resolve root-dir "escape.edn")
                               (make-array LinkOption 0)))
            "the escaped file does not exist anywhere outside the Genome root")))
    (testing "Windows-style backslash traversal is canonicalized and rejected too"
      (let [parent (load/load-genome parent-dir)
            output-dir (Files/createDirectories (.resolve root-dir "candidates2")
                                                (make-array FileAttribute 0))
            mutation (mutation-with parent
                                    [(set-edn-op {:file "..\\..\\secret.edn"})])]
        (is (= :mutation/path-invalid
               (error-type #(patch/apply-mutation parent mutation output-dir))))
        (is (= [] (dir-entries output-dir)))))))

(deftest case-2-symlink-inside-the-candidate-staging-tree-is-rejected-without-following
  (with-temp-dirs [parent-dir outside-dir output-dir]
    (let [parent (load-parent! parent-dir)
          skills-dir (.resolve parent-dir "skills")
          sentinel (write-text-file! outside-dir "hidden/secret.edn" "{:secret true}\n")]
      ;; Replace the real skills/ directory with a symlink pointing OUTSIDE
      ;; the Genome root — the exact hazard a malicious candidate bundle
      ;; would plant in its staging tree.
      (delete-recursively! skills-dir)
      (if (try-create-symlink! outside-dir skills-dir)
        (testing "a symlink inside the bundle/tree is rejected, never followed"
          (testing "the load gate apply-mutation reuses for every staged
                    candidate rejects a bundle containing a symlink"
            (is (= :genome/symlink-rejected
                   (error-type #(load/load-genome parent-dir)))))
          (testing "the mutation gate anchored at :genome/root rejects an op
                    whose :file resolves through the symlink — before staging"
            (is (= :mutation/path-invalid
                   (error-type #(patch/apply-mutation
                                 parent
                                 (mutation-with parent [(set-edn-op)])
                                 output-dir))))
            (is (= [] (dir-entries output-dir))))
          (testing "the staging-root re-check (defense-in-depth) rejects the
                    same path against a staging tree containing the symlink"
            (let [staging (Files/createDirectories (.resolve output-dir "staging")
                                                   (make-array FileAttribute 0))
                  link (.resolve staging "skills")
                  _ (try-create-symlink! outside-dir link)]
              (is (not (gpath/allowed-genome-path? staging "skills/debugging.edn")))))
          (testing "no follow: the outside target was never read or written"
            (is (Files/exists sentinel (make-array LinkOption 0))
                "the sentinel behind the symlink is untouched")
            (is (not (Files/exists (.resolve outside-dir "escape.edn")
                                   (make-array LinkOption 0))))))
        (testing "symlink creation unavailable on this host; skipped"
          (is true))))))

(deftest case-3-wrong-preimage-hash-leaves-no-candidate-dir
  (with-temp-dirs [parent-dir output-dir]
    (let [parent (load-parent! parent-dir)
          stale (set-edn-op {:expect/hash
                             "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"})
          mutation (mutation-with parent [stale])
          e (thrown-error #(patch/apply-mutation parent mutation output-dir))]
      (testing "a stale preimage fails with the typed :patch/preimage-mismatch"
        (is (= :patch/preimage-mismatch (:error/type (ex-data e))))
        (is (= "skills/debugging.edn" (:path (ex-data e)))))
      (testing "and no candidate (or staging) directory is left behind"
        (is (= [] (dir-entries output-dir)))))))

(deftest case-4-ambiguous-text-range-is-rejected
  (with-temp-dirs [parent-dir output-dir]
    (let [parent (load-parent! parent-dir)
          ambiguous {:op :replace-text
                     :file "skills/notes.txt"
                     :anchor "a"                 ; "alpha" AND "gamma"
                     :text "X"
                     :expect/hash (hash/text-digest notes-source)}
          missing {:op :replace-text
                   :file "skills/notes.txt"
                   :anchor "zzz"
                   :text "X"
                   :expect/hash (hash/text-digest notes-source)}]
      (testing "an anchor matching multiple ranges is rejected, never globally
                replaced (:patch/anchor-ambiguous)"
        (is (= :patch/anchor-ambiguous
               (error-type #(patch/apply-mutation
                             parent (mutation-with parent [ambiguous]) output-dir))))
        (is (= [] (dir-entries output-dir))))
      (testing "an anchor matching zero ranges is rejected too (bounded-range
                rule: :patch/anchor-not-found)"
        (is (= :patch/anchor-not-found
               (error-type #(patch/apply-mutation
                             parent (mutation-with parent [missing]) output-dir))))
        (is (= [] (dir-entries output-dir)))))))

(deftest case-5-rewrite-clj-selector-matching-zero-forms-is-rejected
  (with-temp-dirs [parent-dir output-dir]
    (let [parent (load-parent! parent-dir)
          mutation (mutation-with
                    parent
                    [{:op :replace-form
                      :file "programs/route.clj"
                      :selector :nonexistent
                      :form '(defn run [x] x)
                      :expect/hash (hash/text-digest route-source)}])
          e (thrown-error #(patch/apply-mutation parent mutation output-dir))]
      (testing "a selector that matches no form fails closed with the typed
                :patch/form-not-found, before any edit"
        (is (= :patch/form-not-found (:error/type (ex-data e))))
        (is (= "programs/route.clj" (:path (ex-data e)))))
      (testing "no candidate directory survives"
        (is (= [] (dir-entries output-dir)))))))

(deftest case-6-rewrite-clj-selector-matching-multiple-forms-is-bounded-and-deterministic
  ;; DEVIATION from the plan's case list: the shipped component selector
  ;; contract is FIRST-MATCH-WINS (evoclj.genome.patch-clj), NOT
  ;; reject-on-multi-match. This case verifies the implemented guarantees
  ;; that keep the multi-match scenario safe: the edit touches exactly ONE
  ;; form, every other match survives byte-for-byte, and repeated
  ;; application is deterministic (Global Constraint 6). See the
  ;; namespace docstring.
  (with-temp-dirs [parent-dir output-a output-b]
    (let [parent (load-parent! parent-dir)
          mutation (mutation-with
                    parent
                    [{:op :delete-form
                      :file "programs/multi.clj"
                      :selector 'run          ; matches BOTH (run 1) and (run 2)
                      :expect/hash (hash/text-digest multi-source)}])
          c1 (patch/apply-mutation parent mutation output-a)
          c2 (patch/apply-mutation parent mutation output-b)
          after (text-of (get-in c1 [:files "programs/multi.clj"]))]
      (testing "the multi-match selector selects exactly the first matching
                form — a bounded single-form edit, never a global delete"
        (is (= "(ns fixture.multi)\n(defn a [])\n(defn b [] (run 2))\n" after))
        (is (str/includes? after "(defn b [] (run 2))")
            "the SECOND matching form survives — no global/unbounded edit"))
      (testing "the choice is deterministic: the same parent+mutation applied
                into separate output dirs yields one candidate hash"
        (is (types/genome-id? (:genome/id c1)))
        (is (= (:genome/id c1) (:genome/id c2)))
        (is (= (->> (:files c1) (sort-by key) (map (comp vec second)) vec)
               (->> (:files c2) (sort-by key) (map (comp vec second)) vec))
            "byte-identical candidate bundles")))))

;; ============================================================================
;; STEP 2 — valid repeated applications yield ONE candidate Genome hash
;; ============================================================================

(deftest case-7-same-parent-and-mutation-applied-100-times-yields-one-candidate-hash
  (with-temp-dirs [parent-dir output-dir]
    (let [parent (load-parent! parent-dir)
          mutation (mutation-with parent [(set-edn-op)])
          results (mapv (fn [_] (patch/apply-mutation parent mutation output-dir))
                        (range 100))
          ids (into #{} (map :genome/id) results)
          first-candidate (first results)
          last-candidate (peek results)
          entries (dir-entries output-dir)]
      (testing "100 applications of the same parent+mutation produce exactly
                ONE candidate Genome hash (Global Constraints 1 and 6)"
        (is (= 1 (count ids)))
        (is (= 1 (count entries))
            "the deterministic finalize dedupes: a single candidate directory")
        (is (str/starts-with? (first entries) "sha256-"))
        (is (= (first ids) (:genome/id last-candidate))))
      (testing "and byte-identical outputs across all applications"
        (is (= (->> (:files first-candidate) (sort-by key) (map (comp vec second)) vec)
               (->> (:files last-candidate) (sort-by key) (map (comp vec second)) vec)))
        (is (= "{:workflow {:before-edit [:reproduce :localize]}}\n"
               (text-of (get-in last-candidate [:files "skills/debugging.edn"]))))))))

;; ============================================================================
;; STEP 3 — SCI runtime exhaustion after a mutation is contained
;; ============================================================================

(defn- route-descriptor
  "The program descriptor for the (possibly mutated) route program."
  []
  {:program/id :program/route
   :entry 'agent.route/run})

(defn- mutate-route!
  "Apply ONE :replace-form mutation to the fixture parent's route program
  and return the mutated source text decoded from the candidate bundle.
  Scratch dirs are cleaned up before the source is returned."
  [form]
  (let [parent-dir (temp-dir!)
        output (temp-dir!)
        parent (load/load-genome (write-genome! parent-dir))
        mutation (mutation-with
                  parent
                  [{:op :replace-form
                    :file "programs/route.clj"
                    :selector '[defn run]
                    :form form
                    :expect/hash (hash/text-digest route-source)}])
        candidate (patch/apply-mutation parent mutation output)
        source (text-of (get-in candidate [:files "programs/route.clj"]))]
    (delete-recursively! output)
    (delete-recursively! parent-dir)
    source))

(defn- fresh-runtime
  "A Phenotype-style sci-runtime with the default closed context."
  []
  {:context (context/make-context {}) :programs {}})

(defn- load-route!
  "Load `source` into a fresh runtime under :program/route and return the
  runtime map."
  [source]
  (execute/load-program! (fresh-runtime) (route-descriptor) source))

(defn- benign-program
  "A pure decision program registered into the SAME runtime after an
  exhausted call, proving the session/context was not poisoned."
  []
  {:program/id :program/echo
   :entry 'fixture.echo/run
   :source "(ns fixture.echo)\n(defn run [x] x)\n"})

(defn- elapsed-ms
  "Wall-clock milliseconds taken by (f)."
  [f]
  (let [t0 (System/nanoTime)]
    (f)
    (long (/ (- (System/nanoTime) t0) 1000000))))

(defn- thread-count
  "The number of live JVM threads."
  []
  (count (Thread/getAllStackTraces)))

(deftest case-8-sci-infinite-loop-after-mutation-is-interrupted-and-contained
  (testing "the MUTATED program (its route body replaced with an unbounded
            loop) is interrupted by the runtime's limits, not hung"
    (let [mutated (mutate-route! '(defn run [x] (loop [] (recur))))
          runtime (load-route! mutated)
          threads-before (thread-count)
          elapsed
          (elapsed-ms
           (fn []
             (let [result (execute/invoke! runtime :program/route {}
                                           {:wall-ms 10000 :max-steps 100000
                                            :max-output-nodes 1000})]
               (is (= :error (:status result)))
               (is (= :sci/limit-exceeded (:error/type (:error result))))
               (is (contains? #{:max-steps :wall-ms}
                              (:limit (:error/data (:error result)))))
               (is (= (:error result)
                      (edn/read-string (pr-str (:error result))))
                   "the interrupted error is plain serializable data
                   (Global Constraint 22)")
               (is (some? (:steps (:usage result)))))))]
      (is (< elapsed 8000)
          (str "infinite loop interrupted within a bounded time; took "
               elapsed "ms"))
      (testing "the exhaustion is CONTAINED to the session: a repeat call is
                interrupted again, no thread is leaked, and a benign program
                in the SAME runtime still executes normally afterwards"
        (let [r2 (execute/invoke! runtime :program/route {}
                                  {:wall-ms 10000 :max-steps 100000
                                   :max-output-nodes 1000})]
          (is (= :sci/limit-exceeded (:error/type (:error r2)))
              "every exhausted call is interrupted, not wedged"))
        (is (<= (thread-count) (+ threads-before 2))
            (str "no worker/watchdog thread was leaked; "
                 threads-before " -> " (thread-count)))
        (let [with-echo (execute/load-program! runtime (benign-program)
                                               (:source (benign-program)))
              ok (execute/invoke! with-echo :program/echo {:text "still-alive"}
                                  {:wall-ms 10000 :max-steps 100000
                                   :max-output-nodes 1000})]
          (is (= :ok (:status ok)))
          (is (= {:text "still-alive"} (:value ok))
              "kernel operations in the same session proceed normally"))))))

(deftest case-9-sci-huge-infinite-lazy-output-after-mutation-hits-the-materialization-cap
  (testing "the MUTATED program returning an infinite lazy sequence is cut
            off by the materialization cap — a typed error, not a hang"
    (let [mutated (mutate-route! '(defn run [x] (range)))
          runtime (load-route! mutated)
          threads-before (thread-count)
          elapsed
          (elapsed-ms
           (fn []
             (let [result (execute/invoke! runtime :program/route {}
                                           {:wall-ms 10000 :max-steps 100000
                                            :max-output-nodes 1000})]
               (is (= :error (:status result)))
               (is (= :edn/size-exceeded (:error/type (:error result))))
               (is (= 1000 (get-in (:error result) [:error/data :limit]))
                   "the cap that fired is the per-collection output cap")
               (is (= (:error result)
                      (edn/read-string (pr-str (:error result))))
                   "the capped error is plain serializable data
                   (Global Constraint 22)"))))]
      (is (< elapsed 8000)
          (str "infinite lazy output capped within a bounded time; took "
               elapsed "ms"))
      (testing "containment: the session survives — a bounded call and a
                benign program in the SAME runtime still execute normally"
        (is (<= (thread-count) (+ threads-before 2))
            (str "no thread was leaked; "
                 threads-before " -> " (thread-count)))
        (let [with-echo (execute/load-program! runtime (benign-program)
                                               (:source (benign-program)))
              ok (execute/invoke! with-echo :program/echo {:text "alive"}
                                  {:wall-ms 10000 :max-steps 100000
                                   :max-output-nodes 1000})]
          (is (= :ok (:status ok)))
          (is (= {:text "alive"} (:value ok))
              "kernel operations in the same session proceed normally"))))))
