(ns evoclj.genome.patch-test
  "Tests for applying a declarative mutation to an immutable parent Genome
  (component, evoclj.genome.patch/apply-mutation).

  apply-mutation stages a safe copy of the parent bundle (bytes from the
  already-loaded in-memory Genome, never following symlinks), applies the
  finite declarative op language in order, verifies each destructive op's
  :expect/hash preimage against the CURRENT staged content, reloads and
  re-hashes the staged bundle, and atomically finalizes a candidate
  directory named by the new Genome ID. Tests cover: EDN nested set,
  stale-preimage rejection without a candidate dir, bounded text
  replacement (never a global replace), rewrite-clj form replacement that
  preserves comments, deterministic double application into separate temp
  dirs, symlink/escape rejection, and topology graph ops validated through
  evoclj.compiler.topology."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.helpers :as h :refer [with-temp-dirs]]
            [evoclj.compiler.topology :as topology]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.genome.patch :as patch]
            [evoclj.genome.patch-edn :as patch-edn]
            [evoclj.genome.types :as types])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption OpenOption Path)
           (java.nio.file.attribute FileAttribute)))

;; --- fixtures -------------------------------------------------------------

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
           :evolution {:max-risk :topology
                       :mutable #{:parameters :prompts :skills :programs :topology}}
           :metadata {:name "patch-fixture"}}))

(def ^:private evolution-source "{:evolution {}}\n")
(def ^:private memory-source "{:memory {}}\n")
(def ^:private models-source "{:models {:planner {:alias :reasoning/high}}}\n")
(def ^:private topology-source
  "{:graph/id :graph/main\n :entry :node/planner\n :nodes\n {:node/planner {:node/type :llm :model :planner :next :node/finish}\n  :node/finish {:node/type :emit}}\n :limits {:max-steps 64}}\n")
(def ^:private skills-edn-source "{:workflow {:before-edit []}}\n")
(def ^:private notes-source "alpha\nbeta\ngamma\n")
(def ^:private route-source
  ";; Keep me! This comment must survive form replacement.\n(defn run\n  \"Route one task.\"\n  [x]\n  x)\n\n(defn other\n  \"Unrelated helper.\"\n  []\n  2)\n")

(def ^:private fixture-files
  {"manifest.edn" manifest-source
   "evolution.edn" evolution-source
   "memory.edn" memory-source
   "models.edn" models-source
   "topology.edn" topology-source
   "skills/debugging.edn" skills-edn-source
   "skills/notes.txt" notes-source
   "programs/route.clj" route-source})

;; --- temp-dir helpers — collapsed to evoclj.helpers -------------------------

(def ^:private temp-dir! h/temp-dir!)
(def ^:private delete-recursively! h/delete-recursively!)
(def ^:private try-create-symlink! h/try-create-symlink!)
(def ^:private text-of h/text-of)
(def ^:private dir-entries h/dir-entries)
(def ^:private thrown-error h/thrown-error)

(defn- write-genome! [^Path dir]
  (doseq [[rel content] fixture-files]
    (h/write-text! dir rel content))
  dir)

;; --- mutation fixtures ----------------------------------------------------

(defn- mutation-with
  "A schema-valid Mutation envelope carrying `ops`, pinned to `parent` — returns sealed ValidatedMutation."
  [parent ops]
  (mutation/validate-mutation
    {:mutation/id (java.util.UUID/randomUUID)
     :parent/genome-id (:genome/id parent)
     :hypothesis/id (java.util.UUID/randomUUID)
     :evidence/id "sha256:1111111111111111111111111111111111111111111111111111111111111111"
     :risk :behavioral
     :ops ops
     :expected-effect {:primary-metric :task/success :direction :increase}}
    parent))

;; --- step 1: EDN nested set ------------------------------------------------

(deftest step-1-edn-nested-set-produces-canonical-content
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          op {:op :set-edn
              :file "skills/debugging.edn"
              :path [:workflow :before-edit]
              :expect/hash (hash/text-digest skills-edn-source)
              :value [:reproduce :localize]}
          candidate (patch/apply-mutation parent (mutation-with parent [op]) output-dir)
          file-value (get-in candidate [:files "skills/debugging.edn"])]
      (is (types/genome-id? (:genome/id candidate)))
      (is (not= (:genome/id parent) (:genome/id candidate)))
      (is (= "{:workflow {:before-edit [:reproduce :localize]}}\n"
             (text-of file-value)))
      (is (= (:manifest parent) (:manifest candidate)))
      (is (= (set (keys (:files parent))) (set (keys (:files candidate))))))))

(deftest step-1-delete-edn-removes-nested-value
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          op {:op :delete-edn
              :file "skills/debugging.edn"
              :path [:workflow :before-edit]
              :expect/hash (hash/text-digest skills-edn-source)}
          candidate (patch/apply-mutation parent (mutation-with parent [op]) output-dir)]
      (is (= "{:workflow {}}\n"
             (text-of (get-in candidate [:files "skills/debugging.edn"])))))))

;; --- step 2: stale preimage ------------------------------------------------

(deftest step-2-stale-preimage-fails-without-a-candidate-dir
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          op {:op :set-edn
              :file "skills/debugging.edn"
              :path [:workflow :before-edit]
              :expect/hash "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
              :value [:reproduce]}
          e (thrown-error #(patch/apply-mutation parent (mutation-with parent [op]) output-dir))]
      (is (= :patch/preimage-mismatch (:error/type (ex-data e))))
      (is (= [] (dir-entries output-dir))
          "a stale patch must not leave any candidate (or staging) directory"))))

;; --- step 3: bounded text replacement --------------------------------------

(deftest step-3-bounded-text-replacement
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          candidate (patch/apply-mutation
                     parent
                     (mutation-with
                      parent
                      [{:op :replace-text
                        :file "skills/notes.txt"
                        :anchor "beta"
                        :text "BETA"
                        :expect/hash (hash/text-digest notes-source)}])
                     output-dir)]
      (is (= "alpha\nBETA\ngamma\n"
             (text-of (get-in candidate [:files "skills/notes.txt"])))))))

(deftest step-3-line-anchor-replacement
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          candidate (patch/apply-mutation
                     parent
                     (mutation-with
                      parent
                      [{:op :replace-text
                        :file "skills/notes.txt"
                        :anchor 2
                        :text "BETA"
                        :expect/hash (hash/text-digest notes-source)}])
                     output-dir)]
      (is (= "alpha\nBETA\ngamma\n"
             (text-of (get-in candidate [:files "skills/notes.txt"])))))))

(deftest step-3-ambiguous-anchor-is-rejected-not-globally-replaced
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          op {:op :replace-text
              :file "skills/notes.txt"
              :anchor "a"   ; occurs in both "alpha" and "gamma"
              :text "X"
              :expect/hash (hash/text-digest notes-source)}
          e (thrown-error #(patch/apply-mutation parent (mutation-with parent [op]) output-dir))]
      (is (= :patch/anchor-ambiguous (:error/type (ex-data e))))
      (is (= [] (dir-entries output-dir))))))

(deftest step-3-missing-anchor-is-rejected
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          op {:op :replace-text
              :file "skills/notes.txt"
              :anchor "zzz"
              :text "X"
              :expect/hash (hash/text-digest notes-source)}
          e (thrown-error #(patch/apply-mutation parent (mutation-with parent [op]) output-dir))]
      (is (= :patch/anchor-not-found (:error/type (ex-data e)))))))

(deftest step-3-delete-and-insert-text
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          candidate (patch/apply-mutation
                     parent
                     (mutation-with
                      parent
                      [{:op :delete-text
                        :file "skills/notes.txt"
                        :anchor 2
                        :expect/hash (hash/text-digest notes-source)}
                       {:op :insert-text
                        :file "skills/notes.txt"
                        :position :after
                        :anchor "alpha"
                        :text "!"}])
                     output-dir)]
      (is (= "alpha!\ngamma\n"
             (text-of (get-in candidate [:files "skills/notes.txt"])))))))

;; --- step 4: rewrite-clj form replacement preserves comments ---------------

(deftest step-4-form-replacement-preserves-comment
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          op {:op :replace-form
              :file "programs/route.clj"
              :selector '[defn run]
              :form '(defn run [x] (assoc x :patched true))
              :expect/hash (hash/text-digest route-source)}
          candidate (patch/apply-mutation parent (mutation-with parent [op]) output-dir)
          content (text-of (get-in candidate [:files "programs/route.clj"]))]
      (is (str/includes? content "Keep me! This comment must survive"))
      (is (str/includes? content "patched"))
      (is (not (str/includes? content "Route one task."))))))

(deftest step-4-insert-and-delete-forms-preserve-comment
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          inserted (patch/apply-mutation
                    parent
                    (mutation-with
                     parent
                     [{:op :insert-form
                       :file "programs/route.clj"
                       :selector '[defn run]
                       :position :after
                       :form '(defn helper [] 42)}])
                    output-dir)
          inserted-content (text-of (get-in inserted [:files "programs/route.clj"]))]
      (is (str/includes? inserted-content "Keep me! This comment must survive"))
      (is (str/includes? inserted-content "(defn helper [] 42)"))
      (let [deleted (patch/apply-mutation
                     inserted
                     (mutation-with
                      inserted
                      [{:op :delete-form
                        :file "programs/route.clj"
                        :selector '[defn run]
                        :expect/hash (hash/text-digest inserted-content)}])
                     output-dir)
            deleted-content (text-of (get-in deleted [:files "programs/route.clj"]))]
        (is (str/includes? deleted-content "Keep me! This comment must survive"))
        (is (not (str/includes? deleted-content "defn run")))
        (is (str/includes? deleted-content "(defn helper [] 42)"))))))

;; --- step 5: deterministic double application ------------------------------

(deftest step-5-deterministic-double-application
  (with-temp-dirs [p1 p2 o1 o2]
    (write-genome! p1)
    (write-genome! p2)
    (let [parent1 (load/load-genome p1)
          parent2 (load/load-genome p2)
          ops [{:op :set-edn
                :file "skills/debugging.edn"
                :path [:workflow :before-edit]
                :expect/hash (hash/text-digest skills-edn-source)
                :value [:reproduce :localize]}
               {:op :replace-text
                :file "skills/notes.txt"
                :anchor "beta"
                :text "BETA"
                :expect/hash (hash/text-digest notes-source)}
               {:op :replace-form
                :file "programs/route.clj"
                :selector '[defn run]
                :form '(defn run [x] (assoc x :patched true))
                :expect/hash (hash/text-digest route-source)}]
          c1 (patch/apply-mutation parent1 (mutation-with parent1 ops) o1)
          c2 (patch/apply-mutation parent2 (mutation-with parent2 ops) o2)
          same-files? (= (->> (:files c1) (sort-by key) (map (comp vec second)) vec)
                         (->> (:files c2) (sort-by key) (map (comp vec second)) vec))]
      (is (= (:genome/id c1) (:genome/id c2)))
      (is same-files?)
      (is (= "alpha\nBETA\ngamma\n"
             (text-of (get-in c1 [:files "skills/notes.txt"])))))))

;; --- step 6: symlink and escape rejection ----------------------------------

(deftest step-6-traversal-and-symlink-escape-are-rejected
  (with-temp-dirs [parent-dir output-dir outside-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          traversal-op {:op :set-edn
                        :file "../escape.edn"
                        :path [:a]
                        :expect/hash "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                        :value 1}
          e1 (thrown-error #(patch/apply-mutation parent (mutation-with parent [traversal-op]) output-dir))]
      (is (= :mutation/path-invalid (:error/type (ex-data e1))))
      (is (= [] (dir-entries output-dir)))
      (when (try-create-symlink! outside-dir (.resolve parent-dir "sub"))
        (let [link-op {:op :set-edn
                       :file "sub/evil.edn"
                       :path [:a]
                       :expect/hash "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                       :value 1}
              e2 (thrown-error #(patch/apply-mutation parent (mutation-with parent [link-op]) output-dir))]
          (is (= :mutation/path-invalid (:error/type (ex-data e2))))
          (is (= [] (dir-entries output-dir))))))))

(deftest step-6-successful-apply-leaves-only-the-candidate-dir
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          op {:op :set-edn
              :file "skills/debugging.edn"
              :path [:workflow :before-edit]
              :expect/hash (hash/text-digest skills-edn-source)
              :value [:reproduce]}
          candidate (patch/apply-mutation parent (mutation-with parent [op]) output-dir)
          entries (dir-entries output-dir)]
      (is (= 1 (count entries)))
      (is (str/starts-with? (first entries) "sha256-"))
      (is (Files/isDirectory (.resolve output-dir (first entries))
                             (make-array LinkOption 0))))))

;; --- step 7: topology graph ops validated by the topology compiler ---------

(deftest step-7-topology-graph-ops-validate-via-compiler
  (with-temp-dirs [parent-dir output-dir]
    (write-genome! parent-dir)
    (let [parent (load/load-genome parent-dir)
          add-node-op {:op :add-node
                       :file "topology.edn"
                       :node {:node/id :node/audit :node/type :emit}}
          add-edge-op {:op :add-edge
                       :file "topology.edn"
                       :edge {:from :node/planner :to :node/audit}}
          after-adds (patch-edn/apply-op
                      (patch-edn/apply-op topology-source add-node-op)
                      add-edge-op)
          update-op {:op :update-node
                     :file "topology.edn"
                     :node/id :node/audit
                     :update/keys [:note]
                     :value {:note :wip}
                     :expect/hash (hash/text-digest after-adds)}
          after-update (patch-edn/apply-op after-adds update-op)
          remove-op {:op :remove-node
                     :file "topology.edn"
                     :node/id :node/finish
                     :expect/hash (hash/text-digest after-update)}
          candidate (patch/apply-mutation
                     parent
                     (mutation-with parent [add-node-op add-edge-op update-op remove-op])
                     output-dir)
          value (edn/read-string (text-of (get-in candidate [:files "topology.edn"])))
          compiled (topology/compile-topology value)]
      (is (contains? (:nodes compiled) :node/audit))
      (is (not (contains? (:nodes compiled) :node/finish)))
      (is (= :node/audit (get-in compiled [:nodes :node/planner :next])))
      (is (= :wip (get-in compiled [:nodes :node/audit :note]))))))