(ns evoclj.evolution.mutation-test
  "Task 7.3 tests for the Mutation IR and patch preconditions.

  The Mutation is the evolution subsystem's declarative successor
  language: an immutable, closed-map IR that names its parent Genome,
  the evidence and hypothesis it answers, its risk class, a NON-EMPTY
  bounded op vector, and the expected effect it claims. This task is
  DATA VALIDATION ONLY — application comes in Task 7.4. The three
  normative scenarios, in the task's numbered order:

  - Step 1: schema tests for ALL THIRTEEN op variants
    (:set-edn :delete-edn :insert-text :replace-text :delete-text
    :replace-form :insert-form :delete-form :add-node :remove-node
    :add-edge :remove-edge :update-node) and the shared Mutation
    envelope preconditions (closed maps, canonical ids, non-empty
    :ops, the :expected-effect contract).
  - Step 2: every destructive/replace operation REQUIRES an
    :expect/hash — the expected sha256 preimage digest of the target
    file — so a stale patch can never silently apply to a different
    parent. Pure-add operations (:insert-text :insert-form :add-node
    :add-edge) carry it only optionally, and even the optional value
    must be a canonical digest when present.
  - Step 3: preconditions reject operations targeting kernel files
    (manifest.edn, kernel/), evaluation roots (eval/, evaluator/,
    evaluation/), the evolution-policy module, capability-root data
    (capability/, capabilities/), paths escaping the Genome root, and
    asset classes the parent manifest does not declare mutable — each
    with a stable typed :error/type. Path checks reuse
    evoclj.genome.path/allowed-genome-path?, so traversal, absolute,
    drive-letter, and symlink-escape paths fail closed.

  FIXTURE DESIGN: mutations and ops are hand-built so every variant's
  shape is controlled precisely. The class gate needs the parent
  manifest's declared :evolution :mutable set — the tests use the seed
  manifest's declaration (:parameters :prompts :skills :programs) plus
  a permissive variant (:topology) for the topology ops, so a
  mutation that would be legal under a mutable-topology policy is
  still schema-valid end to end."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.evolution.mutation-schema :as ms])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path Paths LinkOption)
           (java.nio.file.attribute FileAttribute)))

;; --- shared fixture identity ------------------------------------------------

(def ^:private hex64
  "64 hex chars for the canonical content-addressed ids."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private genome-id (str "sha256:" hex64))
(def ^:private evidence-id (str "sha256:" (apply str (repeat 64 "e"))))
(def ^:private file-hash (str "sha256:" (apply str (repeat 64 "f"))))

(defn- uuid
  "A fixed, readable UUID for fixture ids."
  [n]
  (java.util.UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

;; --- the parent manifest (seed declaration) ----------------------------------

(def ^:private seed-manifest
  "The seed Genome's manifest contract, reduced to the keys mutation
  preconditions read: the declared module paths (for the protected
  evolution-policy module) and the declared :evolution :mutable set."
  {:genome/format 1
   :agent/id :seed
   :modules {:topology "topology.edn"
             :models "models.edn"
             :memory "memory.edn"
             :evolution "evolution.edn"}
   :evolution {:max-risk :behavioral
               :mutable #{:parameters :prompts :skills :programs}}})

(def ^:private permissive-manifest
  "The seed manifest plus :topology as a declared mutable class, so
  the topology ops (:add-node etc.) pass the full validation gate."
  (update-in seed-manifest [:evolution :mutable] conj :topology))

;; --- op fixtures: one valid sample per variant ------------------------------

(defn- set-edn-op []
  {:op :set-edn
   :file "skills/debugging.edn"
   :path [:workflow :before-edit]
   :expect/hash file-hash
   :value [:reproduce :localize]})

(defn- delete-edn-op []
  {:op :delete-edn
   :file "skills/debugging.edn"
   :path [:workflow :before-edit]
   :expect/hash file-hash})

(defn- insert-text-op []
  {:op :insert-text
   :file "skills/debugging.edn"
   :position :after
   :anchor "before-edit"
   :text "(reproduce)"})

(defn- replace-text-op []
  {:op :replace-text
   :file "skills/debugging.edn"
   :anchor "before-edit"
   :expect/hash file-hash
   :text "localize"})

(defn- delete-text-op []
  {:op :delete-text
   :file "skills/debugging.edn"
   :anchor "before-edit"
   :expect/hash file-hash})

(defn- replace-form-op []
  {:op :replace-form
   :file "programs/route.clj"
   :selector :program/route
   :expect/hash file-hash
   :form (quote (defn route [state] state))})

(defn- insert-form-op []
  {:op :insert-form
   :file "programs/route.clj"
   :selector :program/route
   :position :after
   :form (quote (defn helper [state] state))})

(defn- delete-form-op []
  {:op :delete-form
   :file "programs/route.clj"
   :selector :program/route
   :expect/hash file-hash})

(defn- add-node-op []
  {:op :add-node
   :file "topology.edn"
   :node {:node/id :node/extra :node/type :emit}})

(defn- remove-node-op []
  {:op :remove-node
   :file "topology.edn"
   :node/id :node/extra
   :expect/hash file-hash})

(defn- add-edge-op []
  {:op :add-edge
   :file "topology.edn"
   :edge {:from :node/a :to :node/b}})

(defn- remove-edge-op []
  {:op :remove-edge
   :file "topology.edn"
   :edge {:from :node/a :to :node/b}
   :expect/hash file-hash})

(defn- update-node-op []
  {:op :update-node
   :file "topology.edn"
   :node/id :node/router
   :update/keys [:next]
   :value {:next :node/emit}
   :expect/hash file-hash})

(def ^:private all-op-variants
  "Every op variant with its canonical label, for the Step 1 sweep."
  [[:set-edn (set-edn-op)]
   [:delete-edn (delete-edn-op)]
   [:insert-text (insert-text-op)]
   [:replace-text (replace-text-op)]
   [:delete-text (delete-text-op)]
   [:replace-form (replace-form-op)]
   [:insert-form (insert-form-op)]
   [:delete-form (delete-form-op)]
   [:add-node (add-node-op)]
   [:remove-node (remove-node-op)]
   [:add-edge (add-edge-op)]
   [:remove-edge (remove-edge-op)]
   [:update-node (update-node-op)]])

(def ^:private destructive-ops
  "Ops that overwrite or delete existing content: applying them to a
  different parent than expected is a silent data hazard, so they MUST
  carry :expect/hash (Step 2)."
  [[:set-edn (set-edn-op)]
   [:delete-edn (delete-edn-op)]
   [:replace-text (replace-text-op)]
   [:delete-text (delete-text-op)]
   [:replace-form (replace-form-op)]
   [:delete-form (delete-form-op)]
   [:remove-node (remove-node-op)]
   [:remove-edge (remove-edge-op)]
   [:update-node (update-node-op)]])

(def ^:private additive-ops
  "Pure-addition ops: no preimage digest is required, and the optional
  :expect/hash must still be canonical when present."
  [[:insert-text (insert-text-op)]
   [:insert-form (insert-form-op)]
   [:add-node (add-node-op)]
   [:add-edge (add-edge-op)]])

;; --- mutation envelope fixture ----------------------------------------------

(defn- op
  "The canonical :set-edn op; an optional override map wins."
  [& [overrides]]
  (merge (set-edn-op) overrides))

(defn- mutation*
  "A schema-valid mutation carrying one :set-edn op; an optional
  override map wins (including :ops)."
  [& [overrides]]
  (merge {:mutation/id (uuid 1)
          :parent/genome-id genome-id
          :hypothesis/id (uuid 2)
          :evidence/id evidence-id
          :risk :behavioral
          :ops [(op)]
          :expected-effect {:primary-metric :task/success
                            :direction :increase}}
         overrides))

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by `f`, or nil
  when nothing is thrown."
  [f]
  (:error/type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e)))))

;; ============================================================================
;; Step 1 — schema tests for all op variants and the shared envelope
;; ============================================================================

(deftest step-1-the-normative-mutation-shape-validates
  (let [m {:mutation/id (uuid 1)
           :parent/genome-id genome-id
           :hypothesis/id (uuid 2)
           :evidence/id evidence-id
           :risk :behavioral
           :ops [{:op :set-edn
                  :file "skills/debugging.edn"
                  :path [:workflow :before-edit]
                  :expect/hash file-hash
                  :value [:reproduce :localize]}]
           :expected-effect {:primary-metric :task/success
                             :direction :increase}}]
    (testing "the normative Mutation shape validates at the schema level"
      (is (= m (ms/validate-mutation m))))
    (testing "and passes the full precondition pipeline against the parent manifest"
      (is (= m (mutation/validate-mutation m seed-manifest))))))

(deftest step-1-all-thirteen-op-variants-validate
  (doseq [[label v] all-op-variants]
    (testing (str (name label) " validates at the op level")
      (is (= v (ms/validate-op v))))
    (testing (str (name label) " validates inside a mutation against a
              policy that declares its asset class mutable")
      (is (= (mutation* {:ops [v]})
             (mutation/validate-mutation (mutation* {:ops [v]}) permissive-manifest))))))

(deftest step-1-mutation-envelope-is-a-closed-contract
  (testing "a valid mutation validates unchanged"
    (is (= (mutation*) (ms/validate-mutation (mutation*))))
    (is (= (mutation*) (mutation/validate-mutation (mutation*) seed-manifest))))
  (testing "unknown top-level keys are rejected (closed map)"
    (is (= :mutation/invalid
           (thrown-error-type #(ms/validate-mutation (assoc (mutation*) :bogus 1))))))
  (testing "missing or malformed identity fields are rejected"
    (is (= :mutation/invalid
           (thrown-error-type #(ms/validate-mutation (dissoc (mutation*) :mutation/id)))))
    (is (= :mutation/invalid
           (thrown-error-type #(ms/validate-mutation (assoc (mutation*) :parent/genome-id "G42")))))
    (is (= :mutation/invalid
           (thrown-error-type #(ms/validate-mutation (assoc (mutation*) :hypothesis/id "not-a-uuid")))))
    (is (= :mutation/invalid
           (thrown-error-type #(ms/validate-mutation (assoc (mutation*) :evidence/id "sha256:zzz"))))))
  (testing "the risk class is bounded by the normative risk enum"
    (is (= :mutation/invalid
           (thrown-error-type #(ms/validate-mutation (assoc (mutation*) :risk :explosive))))))
  (testing "the expected-effect contract is closed and typed"
    (is (= :mutation/invalid
           (thrown-error-type #(ms/validate-mutation (dissoc (mutation*) :expected-effect)))))
    (is (= :mutation/invalid
           (thrown-error-type #(ms/validate-mutation
                                (assoc-in (mutation*) [:expected-effect :direction] :sideways)))))
    (is (= :mutation/invalid
           (thrown-error-type #(ms/validate-mutation
                                (assoc-in (mutation*) [:expected-effect :primary-metric] 42)))))
    (is (= :mutation/invalid
           (thrown-error-type #(ms/validate-mutation
                                (assoc (mutation*) :expected-effect {:metric :task/success}))))))
  (testing "a mutation with no ops is meaningless and rejected"
    (is (= :mutation/invalid
           (thrown-error-type #(ms/validate-mutation (assoc (mutation*) :ops []))))))
  (testing "explanations are plain serializable data (Global Constraint 22)"
    (let [e (try (ms/validate-mutation (assoc (mutation*) :risk :explosive))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= (ex-data e) (edn/read-string (pr-str (ex-data e))))))))

(deftest step-1-op-maps-are-closed
  (testing "an unknown :op keyword is rejected"
    (is (= :mutation/op-invalid
           (thrown-error-type #(ms/validate-op (assoc (op) :op :explode-file))))))
  (testing "unknown op keys are rejected (closed op maps)"
    (is (= :mutation/op-invalid
           (thrown-error-type #(ms/validate-op (assoc (set-edn-op) :extra 1))))))
  (testing ":file must be present and a string"
    (is (= :mutation/op-invalid
           (thrown-error-type #(ms/validate-op (dissoc (op) :file)))))
    (is (= :mutation/op-invalid
           (thrown-error-type #(ms/validate-op (assoc (op) :file 42)))))))

;; ============================================================================
;; Step 2 — destructive/replace ops REQUIRE :expect/hash
;; ============================================================================

(deftest step-2-destructive-and-replace-ops-require-expect-hash
  (doseq [[label v] destructive-ops]
    (testing (str (name label) " requires :expect/hash — a stale patch must
              never silently apply to a different parent")
      (is (= :mutation/op-invalid
             (thrown-error-type #(ms/validate-op (dissoc v :expect/hash))))))
    (testing (str (name label) " accepts a canonical sha256 :expect/hash")
      (is (= v (ms/validate-op v))))
    (testing (str (name label) " rejects a malformed :expect/hash")
      (is (= :mutation/op-invalid
             (thrown-error-type #(ms/validate-op (assoc v :expect/hash "sha256:not-hex"))))))))

(deftest step-2-additive-ops-need-no-preimage-hash
  (doseq [[label v] additive-ops]
    (testing (str (name label) " is valid WITHOUT :expect/hash (pure addition)")
      (is (= (dissoc v :expect/hash) (ms/validate-op (dissoc v :expect/hash)))))
    (testing (str (name label) " accepts an optional canonical :expect/hash")
      (is (= v (ms/validate-op v))))
    (testing (str (name label) " rejects a malformed optional :expect/hash")
      (is (= :mutation/op-invalid
             (thrown-error-type #(ms/validate-op (assoc v :expect/hash "nope"))))))))

(deftest step-2-destructive-op-without-preimage-hash-fails-end-to-end
  (testing "the missing preimage digest is caught through the full mutation
            pipeline, not only at the op-level validator"
    (is (= :mutation/op-invalid
           (thrown-error-type
            #(mutation/validate-mutation
              (mutation* {:ops [(dissoc (delete-text-op) :expect/hash)]})
              seed-manifest))))))

;; ============================================================================
;; Step 3 — protected paths, root escapes, and undeclared mutable classes
;; ============================================================================

(deftest step-3-kernel-files-and-eval-and-capability-roots-are-protected
  (let [assert-protected
        (fn [file reason]
          (testing (str file " -> " (name reason))
            (let [e (try (mutation/validate-mutation
                          (mutation* {:ops [(assoc (op) :file file)]})
                          seed-manifest)
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo e) (pr-str file))
              (is (= :mutation/protected-path (:error/type (ex-data e))) (pr-str file))
              (is (= reason (:reason (ex-data e))) (pr-str file)))))]
    (assert-protected "manifest.edn" :kernel-file)
    (assert-protected "kernel/trust.edn" :kernel-root)
    (assert-protected "eval/gates.edn" :eval-root)
    (assert-protected "evaluator/cases.edn" :eval-root)
    (assert-protected "evaluation/selection.edn" :eval-root)
    (assert-protected "capability/lease.edn" :capability-root)
    (assert-protected "capabilities/requested.edn" :capability-root)
    (assert-protected "evolution.edn" :evolution-root)))

(deftest step-3-paths-escaping-the-genome-root-are-rejected
  (doseq [file ["../secret.edn"
                "a/../../b"
                "/etc/passwd"
                "C:\\evil.edn"
                "..\\..\\secret.edn"
                "a//b"
                ""]]
    (testing (pr-str file)
      (is (= :mutation/path-invalid
             (thrown-error-type
              #(mutation/validate-mutation
                (mutation* {:ops [(assoc (op) :file file)]})
                seed-manifest)))))))

(deftest step-3-undeclared-mutable-classes-are-rejected
  (let [assert-undeclared
        (fn [file cls]
          (testing (str file " -> " (name cls))
            (let [e (try (mutation/validate-mutation
                          (mutation* {:ops [(assoc (op) :file file)]})
                          seed-manifest)
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo e) (pr-str file))
              (is (= :mutation/undeclared-mutable-class
                     (:error/type (ex-data e))) (pr-str file))
              (is (= cls (:class (ex-data e))) (pr-str file))
              (is (= #{:parameters :prompts :skills :programs}
                     (set (:declared (ex-data e)))) (pr-str file)))))]
    (assert-undeclared "topology.edn" :topology)
    (assert-undeclared "models.edn" :models)
    (assert-undeclared "memory.edn" :memory)
    (assert-undeclared "memory/notes.edn" :memory)
    (assert-undeclared "notes/foo.edn" :notes)
    (assert-undeclared "skills-extra/foo.edn" :skills-extra)))

(deftest step-3-declared-mutable-classes-are-accepted
  (doseq [file ["skills/debugging.edn"
                "skills/sub/rule.edn"
                "prompts/main.txt"
                "parameters/temperature.edn"
                "programs/route.clj"]]
    (testing (pr-str file)
      (is (= (mutation* {:ops [(assoc (op) :file file)]})
             (mutation/validate-mutation
              (mutation* {:ops [(assoc (op) :file file)]})
              seed-manifest))))))

(deftest step-3-without-a-manifest-path-safety-still-applies
  (testing "traversal and protected paths are rejected even with no manifest"
    (is (= :mutation/path-invalid
           (thrown-error-type
            #(mutation/validate-mutation (mutation* {:ops [(assoc (op) :file "../x.edn")]})))))
    (is (= :mutation/protected-path
           (thrown-error-type
            #(mutation/validate-mutation (mutation* {:ops [(assoc (op) :file "manifest.edn")]}))))))
  (testing "the mutable-class gate needs the parent manifest; without it the
            mutation is still schema- and path-valid"
    (is (= (mutation* {:ops [(assoc (op) :file "topology.edn")]})
           (mutation/validate-mutation (mutation* {:ops [(assoc (op) :file "topology.edn")]}))))))

(deftest step-3-the-loaded-genome-context-form-is-supported
  (let [context {:genome/id genome-id :manifest seed-manifest :files {}}]
    (testing "a mutation validated against the loaded parent Genome map"
      (is (= (mutation*) (mutation/validate-mutation (mutation*) context))))
    (testing "undeclared classes are rejected through the genome context"
      (is (= :mutation/undeclared-mutable-class
             (thrown-error-type
              #(mutation/validate-mutation
                (mutation* {:ops [(assoc (op) :file "topology.edn")]})
                context)))))))

;; --- symlink escape integration (through allowed-genome-path?) ---------------

(defn- temp-dir!
  ^Path []
  (Files/createTempDirectory "evoclj-mutation-test" (make-array FileAttribute 0)))

(defn- write-text-file!
  "Write `content` to `dir`/`rel` (a Path), creating parent directories."
  [^Path dir rel ^String content]
  (let [p (.resolve dir rel)]
    (Files/createDirectories (.getParent p) (make-array FileAttribute 0))
    (Files/write p (.getBytes content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))
    p))

(defn- try-create-symlink!
  "Best-effort Files/createSymbolicLink. Returns false when the host
  refuses (Windows hosts without Developer Mode or symlink privileges)."
  [^Path target ^Path link]
  (try
    (Files/createSymbolicLink link target (make-array FileAttribute 0))
    true
    (catch Exception _ false)))

(defn- delete-recursively! [^Path dir]
  (when (Files/exists dir (make-array LinkOption 0))
    (let [f (.toFile dir)]
      (when (.isDirectory f)
        (doseq [c (.listFiles f)]
          (delete-recursively! (.toPath c))))
      (Files/deleteIfExists dir))))

(deftest step-3-symlink-escapes-are-rejected-through-allowed-genome-path
  (let [dir (temp-dir!)
        real (write-text-file! dir "real.edn" "{:ok true}\n")
        link (.resolve dir "link.edn")]
    (try
      (if (try-create-symlink! real link)
        (testing "a :file resolving through a symlink inside the Genome root
                  is rejected via allowed-genome-path?"
          (is (= :mutation/path-invalid
                 (thrown-error-type
                  #(mutation/validate-mutation
                    (mutation* {:ops [(assoc (op) :file "link.edn")]})
                    {:manifest seed-manifest :genome/root dir})))))
        (testing "symlink creation unavailable on this host; skipped"
          (is true)))
      (finally
        (delete-recursively! dir)))))
