(ns evoclj.eval.judge-calibration-test
  "Tests for the judge calibration harness (Task E-judge).

  The calibration harness runs a judge over a fixture of
  known-equivalent and known-different expected/actual output pairs and
  reports agreement statistics against the human labels. The pure
  surface under test is agreement-stats (calibration records -> exact
  agree/disagree counts plus a stable per-label breakdown, zeroed on
  empty input), plus the harness itself: run-calibration drives ANY
  judge fn (fn [expected-output outputs] -> boolean) over the pairs and
  reports the stats.

  In CI the harness runs the FIXTURE JUDGE — llm-judge wired to a
  deterministic, network-free :model-call that decides byte-identical
  equivalence by comparing the rendered EXPECTED and ACTUAL sections of
  the judge's bounded user message — so the full judge pipeline
  (prompt rendering, JSON verdict parse, verdict extraction) is
  exercised with zero provider calls and the fixture reports EXACT
  agreement.

  The on-disk fixture (test/fixtures/evals/calibration.edn) covers the
  three required categories — :equiv, :non-equiv, and :shared-edge —
  with labels chosen to match byte-identical equivalence exactly.

  Error contract under test: :eval/judge-calibration-invalid."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.eval.judge :as judge]))

(def ^:private calibration-path
  "The on-disk calibration fixture (Task E-judge)."
  "test/fixtures/evals/calibration.edn")

(defn- calibration-pairs
  "The calibration fixture pairs, read from disk as EDN."
  []
  (edn/read-string (slurp calibration-path)))

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by f, or nil."
  [f]
  (:error/type (ex-data (try (f) nil
                             (catch clojure.lang.ExceptionInfo e e)))))

;; --- the fixture judge (deterministic, no network — CI) ------------------------

(def ^:private expected-marker "EXPECTED output:\n")
(def ^:private actual-marker "ACTUAL output(s):\n")

(defn- fixture-model-call
  "The deterministic, network-free :model-call the fixture judge is
  wired to. It reads the judge's bounded user message and decides
  BYTE-IDENTICAL equivalence: the rendered ACTUAL output section must
  equal the rendered EXPECTED output section. Returns the exact
  provider :value shape llm-judge parses into a verdict."
  [_model-id messages _options]
  (let [user (some (fn [m] (when (= :user (:role m)) (:content m))) messages)
        expected-start (count expected-marker)
        expected-end (str/index-of user "\n\n" expected-start)
        actual-start (+ (str/index-of user actual-marker expected-start)
                        (count actual-marker))
        expected (subs user expected-start expected-end)
        actual (subs user actual-start)]
    {:value {:model/output {:text (json/generate-string
                                   {:equivalent (= expected actual)})}
             :usage {:prompt-tokens 0 :completion-tokens 0}}}))

(defn- fixture-judge
  "The calibration fixture judge (CI): llm-judge wired to the fixture
  model-call — the FULL judge pipeline, zero provider calls."
  []
  (judge/llm-judge {:model-call fixture-model-call
                    :model/id "fixture/byte-identical"}))

;; --- agreement-stats: pure, exact counts on a hand-built verdict list ----------

(deftest agreement-stats-exact-counts
  (testing "a known mix of agreement/disagreement yields the exact counts"
    (let [records [{:calib/id :calib/a :label :equivalent :judge/equivalent true}
                   {:calib/id :calib/b :label :equivalent :judge/equivalent false}
                   {:calib/id :calib/c :label :different :judge/equivalent true}
                   {:calib/id :calib/d :label :different :judge/equivalent false}]
          s (judge/agreement-stats records)]
      (is (= 4 (:total s)))
      (is (= 2 (:agree s)))
      (is (= 2 (:disagree s)))
      (is (= (:total s) (+ (:agree s) (:disagree s))))
      (is (= {:total 2 :agree 1 :disagree 1}
             (:equivalent (:by-label s))))
      (is (= {:total 2 :agree 1 :disagree 1}
             (:different (:by-label s))))
      (testing "the by-label keys are sorted, so the breakdown order is stable"
        (is (= [:different :equivalent] (keys (:by-label s))))))))

(deftest agreement-stats-derives-agreement-from-label-and-verdict
  (testing "agreement is DERIVED from :label and :judge/equivalent — a
            stale :agree field on the input record is never trusted"
    (let [records [{:calib/id :calib/a :label :equivalent :judge/equivalent true
                    :agree false}
                   {:calib/id :calib/b :label :different :judge/equivalent false
                    :agree true}]
          s (judge/agreement-stats records)]
      (is (= 2 (:agree s)))
      (is (= 0 (:disagree s))))))

(deftest empty-verdict-list-yields-a-zeroed-summary
  (testing "no records -> zeroed stats, never an error"
    (let [s (judge/agreement-stats [])]
      (is (= 0 (:total s)))
      (is (= 0 (:agree s)))
      (is (= 0 (:disagree s)))
      (is (= {:total 0 :agree 0 :disagree 0}
             (:equivalent (:by-label s))))
      (is (= {:total 0 :agree 0 :disagree 0}
             (:different (:by-label s)))))))

(deftest agreement-stats-deterministic-and-pure-edn
  (testing "identical input yields an identical summary every time"
    (let [records [{:calib/id :calib/a :label :equivalent :judge/equivalent true}
                   {:calib/id :calib/b :label :different :judge/equivalent true}]
          s1 (judge/agreement-stats records)
          s2 (judge/agreement-stats records)]
      (is (= s1 s2))))
  (testing "the summary round-trips through EDN (Global Constraint 22)"
    (let [records [{:calib/id :calib/a :label :equivalent :judge/equivalent true}
                   {:calib/id :calib/b :label :different :judge/equivalent false}]
          s (judge/agreement-stats records)]
      (is (= s (edn/read-string (pr-str s)))))))

(deftest agreement-stats-validates-its-input
  (testing "non-sequential input fails loud"
    (is (= :eval/judge-calibration-invalid
           (thrown-error-type #(judge/agreement-stats :nope)))))
  (testing "a record that is not a map fails loud"
    (is (= :eval/judge-calibration-invalid
           (thrown-error-type #(judge/agreement-stats [42])))))
  (testing "a record without a keyword :calib/id fails loud"
    (is (= :eval/judge-calibration-invalid
           (thrown-error-type #(judge/agreement-stats
                                [{:label :equivalent :judge/equivalent true}])))))
  (testing "a record without a keyword :label fails loud"
    (is (= :eval/judge-calibration-invalid
           (thrown-error-type #(judge/agreement-stats
                                [{:calib/id :calib/a :judge/equivalent true}])))))
  (testing "a label outside the binary ground truth fails loud — it is
            NEVER silently treated as :different"
    (is (= :eval/judge-calibration-invalid
           (thrown-error-type #(judge/agreement-stats
                                [{:calib/id :calib/a :label :maybe
                                  :judge/equivalent true}])))))
  (testing "a non-boolean :judge/equivalent fails loud"
    (is (= :eval/judge-calibration-invalid
           (thrown-error-type #(judge/agreement-stats
                                [{:calib/id :calib/a :label :equivalent
                                  :judge/equivalent "yes"}]))))))

;; --- calibration-judgement: one judge decision over one pair --------------------

(deftest calibration-judgement-records-one-decision
  (testing "one equivalent pair: the verdict matches the label and agrees"
    (let [v (judge/calibration-judgement
             (fixture-judge)
             {:calib/id :calib/edge-empty
              :label :equivalent
              :expected-output {:text ""}
              :outputs [{:text ""}]})]
      (is (= :calib/edge-empty (:calib/id v)))
      (is (= :equivalent (:label v)))
      (is (true? (:judge/equivalent v)))
      (is (true? (:agree v)))))
  (testing "one different pair: the verdict matches the label and agrees"
    (let [v (judge/calibration-judgement
             (fixture-judge)
             {:calib/id :calib/non-equiv-value
              :label :different
              :expected-output {:text "hello"}
              :outputs [{:text "goodbye"}]})]
      (is (false? (:judge/equivalent v)))
      (is (true? (:agree v)))))
  (testing "a malformed pair fails loud before the judge runs"
    (is (= :eval/judge-calibration-invalid
           (thrown-error-type #(judge/calibration-judgement
                                (fixture-judge) :nope))))
    (is (= :eval/judge-calibration-invalid
           (thrown-error-type #(judge/calibration-judgement
                                (fixture-judge)
                                {:calib/id :calib/x :label :maybe
                                 :expected-output 1 :outputs []}))))))

;; --- the harness over the on-disk fixture ---------------------------------------

(deftest fixture-covers-the-three-calibration-categories
  (testing "the calibration fixture covers equiv, non-equiv, and
            shared-edge pairs with both binary labels (Task E-judge
            acceptance)"
    (let [pairs (calibration-pairs)]
      (is (seq pairs))
      (is (= #{:equiv :non-equiv :shared-edge}
             (set (map :calib/category pairs))))
      (is (= #{:equivalent :different}
             (set (map :label pairs))))
      (is (= (count pairs) (count (distinct (map :calib/id pairs))))
          "pair ids are unique"))))

(deftest harness-reports-exact-agreement-on-the-fixture
  (testing "the fixture judge over the on-disk calibration fixture
            agrees with every label — exact agreement, zero
            disagreement (CI: no network)"
    (let [pairs (calibration-pairs)
          result (judge/run-calibration (fixture-judge) pairs)
          stats (:stats result)
          verdicts (:verdicts result)
          n (count pairs)]
      (is (= n (count verdicts)))
      (is (= n (:total stats)))
      (is (= n (:agree stats)))
      (is (= 0 (:disagree stats)))
      (testing "every per-pair verdict agrees with its label"
        (is (every? :agree verdicts)))
      (testing "the per-label breakdown accounts for every pair"
        (is (= (+ (:total (:equivalent (:by-label stats)))
                  (:total (:different (:by-label stats))))
               (:total stats))))
      (testing "the labels' totals match the fixture's own tally"
        (is (= (count (filter #(= :equivalent (:label %)) pairs))
               (:total (:equivalent (:by-label stats)))))
        (is (= (count (filter #(= :different (:label %)) pairs))
               (:total (:different (:by-label stats)))))))))

(deftest fixture-judge-is-deterministic-without-network
  (testing "running the fixture judge twice yields identical verdicts
            (deterministic, reproducible calibration)"
    (let [pairs (calibration-pairs)
          r1 (judge/run-calibration (fixture-judge) pairs)
          r2 (judge/run-calibration (fixture-judge) pairs)]
      (is (= (:verdicts r1) (:verdicts r2)))
      (is (= (:stats r1) (:stats r2))))))
