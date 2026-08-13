(ns evoclj.evolution.budget-test
  "Task 7.5 tests for the mutation budget and risk-class gate.

  Every mutation op belongs to exactly one risk class (R0 :parameter,
  R1 :behavioral, R2 :program, R3 :topology); the mutation's declared
  :risk must COVER all of its ops' classes, and per-class resource
  costs are aggregated against the normative v0 profile:

      {:parameter  {:max-ops 8}
       :behavioral {:max-files 2 :max-added-bytes 8192
                    :max-deleted-bytes 8192}
       :program    {:max-files 2 :max-top-level-forms 3}
       :topology   {:max-new-nodes 2 :max-removed-nodes 1
                    :max-edge-changes 4}}

  The four scenarios, in the task's numbered order:

  - Step 1: cost per op class — the EDN ops (:set-edn :delete-edn)
    follow their target file's asset class (parameters/* → :parameter;
    skills/, prompts/ → :behavioral; programs/* → :program; topology
    → :topology), the text ops (:insert-text :replace-text
    :delete-text) → :behavioral, the form ops (:replace-form
    :insert-form :delete-form) → :program, and the graph ops
    (:add-node :remove-node :add-edge :remove-edge :update-node) →
    :topology. Each op also costs the resource units its class counts.
  - Step 2: aggregate limits across multiple ops AND files — the
    per-class totals must fit the v0 profile; any violation fails
    closed with :evolution/budget-exceeded.
  - Step 3: a mutation declaring R1 (:behavioral) but containing an R3
    graph op is rejected as UNDER-DECLARED risk
    (:evolution/under-declared-risk) — the declared :risk must cover
    all of its ops' classes.
  - Step 4: R4 (:meta) is explicitly rejected in v0 with
    :evolution/risk-not-enabled.

  FIXTURE DESIGN: ops are hand-built to the Task 7.3 op shapes so a
  costed mutation could also pass mutation-schema validation. The
  :set-edn / :delete-edn fixtures take a :file argument so the same
  shape can be pointed at parameters/, skills/, prompts/, programs/,
  or topology to pin the file-based class mapping."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.budget :as budget]))

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

;; --- op fixtures: one valid sample per variant ------------------------------

(defn- set-edn-op
  "A :set-edn op; defaults to a text/rule asset (skills/), so its class
  is :behavioral unless a parameters/programs/topology file is given."
  ([] (set-edn-op "skills/debugging.edn"))
  ([file] {:op :set-edn
           :file file
           :path [:temperature]
           :expect/hash file-hash
           :value 0.7}))

(defn- delete-edn-op
  ([] (delete-edn-op "skills/debugging.edn"))
  ([file] {:op :delete-edn
           :file file
           :path [:temperature]
           :expect/hash file-hash}))

(defn- insert-text-op
  "A pure-add text op; :text is \"(reproduce)\" — 11 ASCII bytes."
  ([] (insert-text-op "skills/debugging.edn"))
  ([file] {:op :insert-text
           :file file
           :position :after
           :anchor "before-edit"
           :text "(reproduce)"}))

(defn- replace-text-op
  "A replace text op: adds \"localize\" (8 bytes), deletes the string
  anchor \"before-edit\" (11 bytes)."
  ([] (replace-text-op "skills/debugging.edn"))
  ([file] {:op :replace-text
           :file file
           :anchor "before-edit"
           :expect/hash file-hash
           :text "localize"}))

(defn- delete-text-op
  "A delete text op: deletes the string anchor \"before-edit\"
  (11 bytes)."
  ([] (delete-text-op "skills/debugging.edn"))
  ([file] {:op :delete-text
           :file file
           :anchor "before-edit"
           :expect/hash file-hash}))

(defn- replace-form-op
  ([] (replace-form-op "programs/route.clj"))
  ([file] {:op :replace-form
           :file file
           :selector :program/route
           :expect/hash file-hash
           :form (quote (defn route [state] state))}))

(defn- insert-form-op
  ([] (insert-form-op "programs/route.clj"))
  ([file] {:op :insert-form
           :file file
           :selector :program/route
           :position :after
           :form (quote (defn helper [state] state))}))

(defn- delete-form-op
  ([] (delete-form-op "programs/route.clj"))
  ([file] {:op :delete-form
           :file file
           :selector :program/route
           :expect/hash file-hash}))

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
  "Every op variant with its canonical label and EXPECTED risk class.
  The EDN fixtures default to a skills/ target, hence :behavioral."
  [[:set-edn (set-edn-op) :behavioral]
   [:delete-edn (delete-edn-op) :behavioral]
   [:insert-text (insert-text-op) :behavioral]
   [:replace-text (replace-text-op) :behavioral]
   [:delete-text (delete-text-op) :behavioral]
   [:replace-form (replace-form-op) :program]
   [:insert-form (insert-form-op) :program]
   [:delete-form (delete-form-op) :program]
   [:add-node (add-node-op) :topology]
   [:remove-node (remove-node-op) :topology]
   [:add-edge (add-edge-op) :topology]
   [:remove-edge (remove-edge-op) :topology]
   [:update-node (update-node-op) :topology]])

;; --- mutation envelope fixture ----------------------------------------------

(defn- mutation*
  "A mutation envelope whose :risk defaults to :behavioral; an optional
  override map wins (including :ops and :risk)."
  [& [overrides]]
  (merge {:mutation/id (uuid 1)
          :parent/genome-id genome-id
          :hypothesis/id (uuid 2)
          :evidence/id evidence-id
          :risk :behavioral
          :ops [(set-edn-op)]
          :expected-effect {:primary-metric :task/success
                            :direction :increase}}
         overrides))

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by `f`, or nil."
  [f]
  (:error/type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e)))))

(defn- thrown-error-data
  "The ex-data of the typed ExceptionInfo thrown by `f`, or nil."
  [f]
  (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e))))

;; ============================================================================
;; Step 1 — cost per op class
;; ============================================================================

(deftest step-1-every-op-variant-maps-to-exactly-one-risk-class
  (doseq [[label v expected] all-op-variants]
    (testing (str (name label) " → " (name expected))
      (is (= expected (budget/op-risk-class v))))))

(deftest step-1-edn-ops-take-their-class-from-the-target-file
  (testing "parameters/* → :parameter"
    (is (= :parameter
           (budget/op-risk-class (set-edn-op "parameters/temperature.edn"))))
    (is (= :parameter
           (budget/op-risk-class (delete-edn-op "parameters/response-format.edn")))))
  (testing "skills/ and prompts/ (text/rules) → :behavioral"
    (is (= :behavioral
           (budget/op-risk-class (set-edn-op "skills/debugging.edn"))))
    (is (= :behavioral
           (budget/op-risk-class (set-edn-op "prompts/main.txt")))))
  (testing "programs/* → :program"
    (is (= :program
           (budget/op-risk-class (set-edn-op "programs/route.edn")))))
  (testing "topology → :topology"
    (is (= :topology
           (budget/op-risk-class (set-edn-op "topology.edn")))))
  (testing "an unlisted asset class defaults to :behavioral (rules)"
    (is (= :behavioral
           (budget/op-risk-class (set-edn-op "models/notes.edn"))))))

(deftest step-1-op-cost-accounts-each-class-resource
  (testing "a parameter op costs one :ops unit"
    (is (= {:parameter {:ops 1}}
           (budget/op-cost (set-edn-op "parameters/temperature.edn")))))
  (testing "a program op costs one top-level form on its file"
    (is (= {:program {:files #{"programs/route.clj"} :top-level-forms 1}}
           (budget/op-cost (replace-form-op)))))
  (testing "text ops cost their UTF-8 byte deltas"
    (is (= {:behavioral {:files #{"skills/debugging.edn"} :added-bytes 11}}
           (budget/op-cost (insert-text-op))))
    (is (= {:behavioral {:files #{"skills/debugging.edn"}
                         :added-bytes 8 :deleted-bytes 11}}
           (budget/op-cost (replace-text-op))))
    (is (= {:behavioral {:files #{"skills/debugging.edn"} :deleted-bytes 11}}
           (budget/op-cost (delete-text-op)))))
  (testing "byte accounting is UTF-8, not char count (\"温度\" is 6 bytes)"
    (is (= {:behavioral {:files #{"skills/debugging.edn"} :added-bytes 6}}
           (budget/op-cost (assoc (insert-text-op) :text "温度")))))
  (testing "a line-offset anchor has no computable preimage size; it costs 0"
    (is (= {:behavioral {:files #{"skills/debugging.edn"} :deleted-bytes 0}}
           (budget/op-cost (assoc (delete-text-op) :anchor 42)))))
  (testing "graph ops cost their topology units"
    (is (= {:topology {:new-nodes 1}} (budget/op-cost (add-node-op))))
    (is (= {:topology {:removed-nodes 1}} (budget/op-cost (remove-node-op))))
    (is (= {:topology {:edge-changes 1}} (budget/op-cost (add-edge-op))))
    (is (= {:topology {:edge-changes 1}} (budget/op-cost (remove-edge-op))))
    (is (= {:topology {:edge-changes 1}} (budget/op-cost (update-node-op))))))

(deftest step-1-an-op-outside-the-op-language-fails-closed
  (testing "an unclassifiable op cannot be budgeted"
    (is (= :evolution/unknown-op-class
           (thrown-error-type #(budget/op-risk-class {:op :explode-file}))))
    (is (= :evolution/unknown-op-class
           (thrown-error-type
            #(budget/check-budget (mutation* {:ops [{:op :bogus}]})))))))

;; ============================================================================
;; Step 2 — aggregate limits across multiple ops/files
;; ============================================================================

(deftest step-2-mutation-cost-aggregates-across-multiple-ops-and-files
  (let [m (mutation*
           {:risk :topology
            :ops [(set-edn-op "parameters/temperature.edn")
                  (set-edn-op "parameters/response-format.edn")
                  (insert-text-op "skills/debugging.edn")
                  (replace-text-op "prompts/main.txt")
                  (delete-text-op "skills/debugging.edn")
                  (replace-form-op)
                  (insert-form-op)
                  (add-node-op)
                  (add-edge-op)
                  (remove-edge-op)
                  (update-node-op)]})
        cost (budget/mutation-cost m)]
    (testing "costs aggregate by class across ops and files"
      (is (= {:ops 2} (:parameter cost)))
      (is (= #{"skills/debugging.edn" "prompts/main.txt"}
             (:files (:behavioral cost))))
      (is (= 19 (:added-bytes (:behavioral cost))))    ; 11 + 8
      (is (= 22 (:deleted-bytes (:behavioral cost))))  ; 11 + 11
      (is (= #{"programs/route.clj"} (:files (:program cost))))
      (is (= 2 (:top-level-forms (:program cost))))
      (is (= 1 (:new-nodes (:topology cost))))
      (is (= 0 (:removed-nodes (:topology cost))))
      (is (= 3 (:edge-changes (:topology cost))))))
  (testing "and the same mutation fits the v0 profile, returned unchanged"
    (let [m (mutation*
             {:risk :topology
              :ops [(set-edn-op "parameters/temperature.edn")
                    (set-edn-op "parameters/response-format.edn")
                    (insert-text-op "skills/debugging.edn")
                    (replace-text-op "prompts/main.txt")
                    (delete-text-op "skills/debugging.edn")
                    (replace-form-op)
                    (insert-form-op)
                    (add-node-op)
                    (add-edge-op)
                    (remove-edge-op)
                    (update-node-op)]})]
      (is (= m (budget/check-budget m))))))

(deftest step-2-parameter-ops-are-bounded-by-max-ops
  (testing "8 parameter ops fit :max-ops 8"
    (let [m (mutation* {:risk :parameter
                        :ops (vec (repeat 8 (set-edn-op "parameters/temperature.edn")))})]
      (is (= m (budget/check-budget m)))))
  (testing "9 parameter ops exceed :max-ops 8"
    (let [m (mutation* {:risk :parameter
                        :ops (vec (repeat 9 (set-edn-op "parameters/temperature.edn")))})]
      (is (= :evolution/budget-exceeded (thrown-error-type #(budget/check-budget m))))
      (is (= {:class :parameter :limit :max-ops :actual 9 :max 8}
             (first (:failures (thrown-error-data #(budget/check-budget m)))))))))

(deftest step-2-behavioral-files-and-byte-deltas-are-bounded
  (testing "2 behavioral files fit :max-files 2"
    (let [m (mutation* {:risk :behavioral
                        :ops [(insert-text-op "skills/a.edn")
                              (insert-text-op "prompts/b.txt")]})]
      (is (= m (budget/check-budget m)))))
  (testing "3 behavioral files exceed :max-files 2"
    (let [m (mutation* {:risk :behavioral
                        :ops [(insert-text-op "skills/a.edn")
                              (insert-text-op "skills/b.edn")
                              (insert-text-op "prompts/c.txt")]})]
      (is (= :evolution/budget-exceeded (thrown-error-type #(budget/check-budget m))))
      (is (some #(= {:class :behavioral :limit :max-files :actual 3 :max 2} %)
                (:failures (thrown-error-data #(budget/check-budget m)))))))
  (testing "a 9000-byte insert exceeds :max-added-bytes 8192"
    (let [m (mutation* {:risk :behavioral
                        :ops [(assoc (insert-text-op)
                                     :text (apply str (repeat 9000 "a")))]})]
      (is (= :evolution/budget-exceeded (thrown-error-type #(budget/check-budget m))))
      (is (some #(= {:class :behavioral :limit :max-added-bytes
                      :actual 9000 :max 8192} %)
                (:failures (thrown-error-data #(budget/check-budget m)))))))
  (testing "a 9000-byte string-anchor delete exceeds :max-deleted-bytes 8192"
    (let [m (mutation* {:risk :behavioral
                        :ops [(assoc (delete-text-op)
                                     :anchor (apply str (repeat 9000 "b")))]})]
      (is (= :evolution/budget-exceeded (thrown-error-type #(budget/check-budget m))))
      (is (some #(= {:class :behavioral :limit :max-deleted-bytes
                      :actual 9000 :max 8192} %)
                (:failures (thrown-error-data #(budget/check-budget m))))))))

(deftest step-2-program-forms-and-files-are-bounded
  (testing "3 top-level forms across 2 files fit the v0 profile"
    (let [m (mutation* {:risk :program
                        :ops [(replace-form-op)
                              (insert-form-op "programs/helper.clj")
                              (insert-form-op "programs/helper.clj")]})]
      (is (= m (budget/check-budget m)))))
  (testing "4 form ops exceed :max-top-level-forms 3"
    (let [m (mutation* {:risk :program
                        :ops [(replace-form-op) (insert-form-op)
                              (insert-form-op) (delete-form-op)]})]
      (is (= :evolution/budget-exceeded (thrown-error-type #(budget/check-budget m))))
      (is (some #(= {:class :program :limit :max-top-level-forms
                      :actual 4 :max 3} %)
                (:failures (thrown-error-data #(budget/check-budget m)))))))
  (testing "3 program files exceed :max-files 2"
    (let [m (mutation* {:risk :program
                        :ops [(replace-form-op "programs/a.clj")
                              (insert-form-op "programs/b.clj")
                              (delete-form-op "programs/c.clj")]})]
      (is (= :evolution/budget-exceeded (thrown-error-type #(budget/check-budget m))))
      (is (some #(= {:class :program :limit :max-files :actual 3 :max 2} %)
                (:failures (thrown-error-data #(budget/check-budget m))))))))

(deftest step-2-topology-node-and-edge-changes-are-bounded
  (testing "2 new nodes, 1 removed node, and 4 edge changes fit the v0 profile"
    (let [m (mutation* {:risk :topology
                        :ops [(add-node-op) (add-node-op)
                              (remove-node-op)
                              (add-edge-op) (remove-edge-op)
                              (update-node-op) (add-edge-op)]})]
      (is (= m (budget/check-budget m)))))
  (testing "3 new nodes exceed :max-new-nodes 2"
    (let [m (mutation* {:risk :topology
                        :ops [(add-node-op) (add-node-op) (add-node-op)]})]
      (is (= :evolution/budget-exceeded (thrown-error-type #(budget/check-budget m))))
      (is (some #(= {:class :topology :limit :max-new-nodes :actual 3 :max 2} %)
                (:failures (thrown-error-data #(budget/check-budget m)))))))
  (testing "2 removed nodes exceed :max-removed-nodes 1"
    (let [m (mutation* {:risk :topology
                        :ops [(remove-node-op) (remove-node-op)]})]
      (is (= :evolution/budget-exceeded (thrown-error-type #(budget/check-budget m))))
      (is (some #(= {:class :topology :limit :max-removed-nodes :actual 2 :max 1} %)
                (:failures (thrown-error-data #(budget/check-budget m)))))))
  (testing "5 edge changes exceed :max-edge-changes 4"
    (let [m (mutation* {:risk :topology
                        :ops [(add-edge-op) (add-edge-op) (add-edge-op)
                              (add-edge-op) (add-edge-op)]})]
      (is (= :evolution/budget-exceeded (thrown-error-type #(budget/check-budget m))))
      (is (some #(= {:class :topology :limit :max-edge-changes :actual 5 :max 4} %)
                (:failures (thrown-error-data #(budget/check-budget m))))))))

;; ============================================================================
;; Step 3 — under-declared risk (an R1 mutation carrying an R3 op)
;; ============================================================================

(deftest step-3-a-behavioral-mutation-with-a-graph-op-is-under-declared
  (let [m (mutation* {:risk :behavioral
                      :ops [(set-edn-op) (insert-text-op) (add-node-op)]})]
    (testing "a mutation declaring R1 (:behavioral) with an R3 graph op is rejected"
      (is (= :evolution/under-declared-risk
             (thrown-error-type #(budget/check-budget m)))))
    (testing "the error names the declared risk and the uncovered class"
      (let [d (thrown-error-data #(budget/check-budget m))]
        (is (= :behavioral (:declared d)))
        (is (= #{:topology} (set (:uncovered d))))))
    (testing "the same ops declared at :topology cover all classes and pass"
      (is (= (assoc m :risk :topology)
             (budget/check-budget (assoc m :risk :topology)))))))

(deftest step-3-the-coverage-gate-runs-before-the-limit-gate
  (testing "an under-declared mutation is rejected even when its declared
            class's OWN limits are exceeded — the coverage gate fires first"
    (let [m (mutation* {:risk :behavioral
                        :ops [(insert-text-op)
                              (insert-text-op "skills/b.edn")
                              (insert-text-op "prompts/c.txt")
                              (add-node-op)]})]
      ;; 3 behavioral files would exceed :max-files 2, but the graph op
      ;; makes the declared :behavioral risk under-declared first.
      (is (= :evolution/under-declared-risk
             (thrown-error-type #(budget/check-budget m)))))))

;; ============================================================================
;; Step 4 — R4 (:meta) is not enabled in v0
;; ============================================================================

(deftest step-4-r4-meta-is-explicitly-rejected-in-v0
  (let [m (mutation* {:risk :meta})]
    (testing "the v0 profile does not enable :meta"
      (is (not (contains? budget/v0-profile :meta))))
    (testing "a mutation declaring R4 (:meta) is rejected"
      (is (= :evolution/risk-not-enabled
             (thrown-error-type #(budget/check-budget m)))))
    (testing "the error names the rejected risk class"
      (is (= :meta (:risk (thrown-error-data #(budget/check-budget m))))))
    (testing "the mutation envelope is otherwise irrelevant — the class alone
              is rejected before any op is examined"
      (is (= :evolution/risk-not-enabled
             (thrown-error-type #(budget/check-budget (assoc m :ops []))))))))
