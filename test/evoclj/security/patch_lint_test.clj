(ns evoclj.security.patch-lint-test
  "Task F7 tests for static patch lint (evoclj.security.patch-lint).

  Coverage maps to the module contract:

  - Rule 1 — an op without a :file is :fatal :lint/missing-file, with the
    sanitized op in the detail.
  - Rule 2 — a :file whose first path segment is in :protected-prefixes
    (\"kernel/foo.edn\", \"capability/policy.edn\", ...) is :fatal
    :lint/protected-path.
  - Rule 4 — a :file in a safe, non-protected class
    (\"skills/x.edn\", \"programs/route.clj\") yields no finding with the
    default opts.
  - Rule 3 — when :allowed-classes is provided, an unknown class is :warn
    :lint/undeclared-class and a declared class passes.
  - lint-patch! throws :security/patch-lint-fatal (with :findings in
    ex-data) when any :fatal finding exists, and returns the (non-fatal)
    findings otherwise.
  - empty :ops -> [].
  - malformed inputs (non-map mutation, non-sequential :ops, non-map
    opts) throw :security/patch-lint-invalid."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.security.patch-lint :as lint]))

(defn- op
  "A minimal :set-edn op; an optional override wins."
  [& [overrides]]
  (merge {:op :set-edn
          :file "skills/debugging.edn"
          :path [:workflow :before-edit]
          :expect/hash "sha256:abcdef"
          :value [:reproduce]}
         overrides))

(defn- mutation*
  "A lintable mutation carrying one :set-edn op; an optional override
  wins (including :ops)."
  [& [overrides]]
  (merge {:ops [(op)]} overrides))

(defn- thrown-error
  "The ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- finding-for
  "The single finding for `file` in a one-op mutation, or nil."
  [file & [opts]]
  (first (lint/lint-patch (mutation* {:ops [(op {:file file})]}) opts)))

;; --- Rule 2: protected path prefixes -----------------------------------------

(deftest protected-path-prefixes-are-fatal
  (doseq [file ["kernel/foo.edn" "capability/policy.edn"
                "kernel/trust.edn" "store/cas.clj" "promotion/state.edn"
                "compiler/core.clj" "sci/context.clj"]]
    (testing (pr-str file)
      (let [f (finding-for file)]
        (is (some? f) (pr-str file))
        (is (= :lint/protected-path (:lint/rule f)) (pr-str file))
        (is (= :fatal (:lint/level f)) (pr-str file))
        (is (= file (:lint/file f)) (pr-str file))
        (is (= (keyword (first (clojure.string/split file #"/")))
               (keyword (:class (:lint/detail f))))
            (pr-str file))))))

(deftest protected-path-canonicalizes-before-checking
  (testing "leading ./ and backslashes are normalized to the protected
            first segment before matching"
    (is (= :lint/protected-path (:lint/rule (finding-for "./kernel/foo.edn"))))
    (is (= "kernel/foo.edn" (:lint/file (finding-for "./kernel/foo.edn"))))
    (is (= :lint/protected-path (:lint/rule (finding-for "kernel\\foo.edn"))))))

;; --- Rules 3 & 4: allowed classes and safe classes ---------------------------

(deftest safe-non-protected-classes-produce-no-findings
  (doseq [file ["skills/x.edn" "programs/route.clj" "parameters/t.edn"
                "prompts/main.txt"]]
    (testing (pr-str file)
      (is (= [] (lint/lint-patch (mutation* {:ops [(op {:file file})]})))
          (pr-str file)))))

(deftest custom-protected-prefixes-can-be-overridden
  (let [m (mutation* {:ops [(op {:file "skills/x.edn"})]})]
    (testing "a caller may broaden the protected set"
      (let [f (lint/lint-patch m {:protected-prefixes ["skills"]})]
        (is (= :lint/protected-path (:lint/rule (first f))))))
    (testing "and may set it explicitly"
      (is (= [] (lint/lint-patch m {:protected-prefixes []}))))))

(deftest declared-allowed-classes-flag-unknown-classes-as-warn
  (let [opts {:allowed-classes ["skills" "programs"]}]
    (testing "an undeclared class is :warn with the sorted allowlist"
      (let [f (finding-for "notes/memo.edn" opts)]
        (is (= :lint/undeclared-class (:lint/rule f)))
        (is (= :warn (:lint/level f)))
        (is (= "notes/memo.edn" (:lint/file f)))
        (is (= "notes" (:class (:lint/detail f))))
        (is (= ["programs" "skills"] (:allowed (:lint/detail f))))
        (is (= ["programs" "skills"] (vec (sort (:allowed (:lint/detail f))))))))
    (testing "a declared class passes"
      (is (= [] (lint/lint-patch (mutation* {:ops [(op {:file "skills/x.edn"})]})
                                 opts)))
      (is (= [] (lint/lint-patch (mutation* {:ops [(op {:file "programs/route.clj"})]})
                                 opts))))))

(deftest protected-path-wins-over-allowed-classes
  (testing "a protected prefix is :fatal even when its class is declared
            allowed (rule order: protected wins)"
    (let [f (finding-for "kernel/x.edn" {:allowed-classes ["kernel"]})]
      (is (= :lint/protected-path (:lint/rule f)))
      (is (= :fatal (:lint/level f))))))

;; --- Rule 1: missing :file ---------------------------------------------------

(deftest op-without-a-file-is-fatal
  (let [m (mutation* {:ops [(op {:file nil})]})
        f (lint/lint-patch m)]
    (is (some? f))
    (let [finding (first f)]
      (is (= :lint/missing-file (:lint/rule finding)))
      (is (= :fatal (:lint/level finding)))
      (is (nil? (:lint/file finding)))
      (is (= 0 (:lint/op-index finding)))
      (is (= :set-edn (:op (:op (:lint/detail finding))))
          "the sanitized op is included for operator review"))))

(deftest op-index-reflects-position
  (let [m (mutation* {:ops [(op)
                            (op {:file nil})
                            (op {:file "kernel/x.edn"})]})
        findings (lint/lint-patch m)]
    (is (= [1 2] (mapv :lint/op-index findings)))
    (is (= [:lint/missing-file :lint/protected-path]
           (mapv :lint/rule findings)))))

;; --- lint-patch! -------------------------------------------------------------

(deftest lint-patch-bang-throws-on-fatal-findings
  (testing "a protected-path target throws :security/patch-lint-fatal"
    (let [m (mutation* {:ops [(op {:file "kernel/foo.edn"})]})
          e (thrown-error #(lint/lint-patch! m))]
      (is (some? e))
      (is (= :security/patch-lint-fatal (:error/type (ex-data e))))
      (let [findings (:findings (ex-data e))]
        (is (= 1 (count findings)))
        (is (= :lint/protected-path (:lint/rule (first findings)))))))
  (testing "an op without a :file throws too"
    (let [m (mutation* {:ops [(op {:file nil})]})
          e (thrown-error #(lint/lint-patch! m))]
      (is (= :security/patch-lint-fatal (:error/type (ex-data e))))
      (is (= :lint/missing-file (:lint/rule (first (:findings (ex-data e))))))))
  (testing "fatal wins even when non-fatal findings coexist"
    (let [m (mutation* {:ops [(op {:file "notes/x.edn"})
                              (op {:file "kernel/y.edn"})]})
          e (thrown-error #(lint/lint-patch! m {:allowed-classes ["skills"]}))]
      (is (= :security/patch-lint-fatal (:error/type (ex-data e))))
      (is (= #{:lint/undeclared-class :lint/protected-path}
             (set (map :lint/rule (:findings (ex-data e)))))))))

(deftest lint-patch-bang-returns-non-fatal-findings
  (testing "a warn-only mutation returns its findings"
    (let [m (mutation* {:ops [(op {:file "notes/x.edn"})]})
          findings (lint/lint-patch! m {:allowed-classes ["skills"]})]
      (is (= 1 (count findings)))
      (is (= :lint/undeclared-class (:lint/rule (first findings))))))
  (testing "a fully compliant mutation returns []"
    (let [m (mutation* {:ops [(op {:file "skills/x.edn"})]})]
      (is (= [] (lint/lint-patch! m))))))

;; --- empty :ops and malformed inputs -----------------------------------------

(deftest empty-ops-produce-no-findings
  (is (= [] (lint/lint-patch (mutation* {:ops []}))))
  (is (= [] (lint/lint-patch! (mutation* {:ops []})))))

(deftest malformed-inputs-throw-patch-lint-invalid
  (testing "mutation is not a map"
    (let [e (thrown-error #(lint/lint-patch "not-a-mutation"))]
      (is (= :security/patch-lint-invalid (:error/type (ex-data e))))))
  (testing ":ops is not sequential"
    (let [e (thrown-error #(lint/lint-patch {:ops "nope"}))]
      (is (= :security/patch-lint-invalid (:error/type (ex-data e))))))
  (testing "opts is not a map"
    (let [e (thrown-error #(lint/lint-patch (mutation*) "nope"))]
      (is (= :security/patch-lint-invalid (:error/type (ex-data e)))))))
