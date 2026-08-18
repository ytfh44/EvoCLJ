(ns evoclj.context.eval
  "Three evaluation classes for context compression quality.

   Every compression run should be evaluated on three axes:
   1. RETENTION  — did the envelope preserve the information that future
      turns need? Measured by comparing residue and evidence coverage
      between the original context and the envelope.
   2. REGRESSION  — did compression cause any regression? Measured by
      checking whether the envelope's task/subgoal state is consistent
      with the original (via crosscheck).
   3. HALLUCINATION — did the model invent information not present in
      the original? Measured by checking whether every residue claim in
      the envelope can be traced to a source in the original context.

   In v0 the actual scoring has heuristic defaults. Callers may still
   override by passing an explicit `score` and optional `details`."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [evoclj.context.error :as err]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.crosscheck :as crosscheck]))

;; ---------------------------------------------------------------------------
;; Eval class keywords
;; ---------------------------------------------------------------------------

(def eval-retention
  "`:eval/retention` — Did the envelope preserve the information future
   turns need? Score: 0.0 (nothing preserved) to 1.0 (everything
   preserved)."
  :eval/retention)

(def eval-regression
  "`:eval/regression` — Did compression cause a regression in task or
   subgoal state? Score: 0.0 (total regression) to 1.0 (no regression)."
  :eval/regression)

(def eval-hallucination
  "`:eval/hallucination` — Did the model invent information not in the
   original? Score: 0.0 (pure hallucination) to 1.0 (no hallucination)."
  :eval/hallucination)

;; ---------------------------------------------------------------------------
;; Status keywords
;; ---------------------------------------------------------------------------

(def status-pass
  "`:status/pass` — The eval class meets the pass threshold."
  :status/pass)

(def status-warn
  "`:status/warn` — The eval class is below pass but above fail; review
   recommended."
  :status/warn)

(def status-fail
  "`:status/fail` — The eval class is below the fail threshold."
  :status/fail)

;; ---------------------------------------------------------------------------
;; Default thresholds
;; ---------------------------------------------------------------------------

(def ^:private default-thresholds
  {:retention  {:pass 0.8  :warn 0.5  :fail 0.0}
   :regression {:pass 0.9  :warn 0.7  :fail 0.0}
   :hallucination {:pass 0.9 :warn 0.7 :fail 0.0}})

;; ---------------------------------------------------------------------------
;; Threshold helpers
;; ---------------------------------------------------------------------------

(defn- threshold [class kind]
  (get-in default-thresholds [class kind] 0.0))

(defn- classify [class score]
  (let [pass (threshold class :pass)
        warn (threshold class :warn)]
    (cond
      (>= score pass) status-pass
      (>= score warn) status-warn
      :else status-fail)))

;; ---------------------------------------------------------------------------
;; Eval record
;; ---------------------------------------------------------------------------

(defn- eval-record
  [class score status details]
  {:eval/class class
   :eval/score score
   :eval/status status
   :eval/details (err/sanitize details)})

;; ---------------------------------------------------------------------------
;; Heuristic scoring helpers
;; ---------------------------------------------------------------------------

(defn- token-count [s]
  (->> (str/split s (java.util.regex.Pattern/compile " "))
       (remove str/blank?)
       (count)))

(defn- jaccard [a b]
  (let [set-a (set a)
        set-b (set b)
        intersection (count (set/intersection set-a set-b))
        union (count (set/union set-a set-b))]
    (if (zero? union) 1.0 (/ (double intersection) union))))

(defn- extract-significant-words [s]
  (let [cleaned (str/replace s #"[^a-zA-Z0-9]" " ")
        parts (str/split cleaned (java.util.regex.Pattern/compile " "))
        words (map str/lower-case (remove str/blank? parts))
        stop-words #{"the" "a" "an" "is" "are" "was" "were" "be" "been"
                      "being" "have" "has" "had" "do" "does" "did"
                      "would" "could" "should" "may" "might" "must" "shall"
                      "to" "of" "in" "for" "on" "with" "at" "by" "from"
                      "as" "into" "through" "during" "before" "after"
                      "above" "below" "between" "out" "off" "over" "under"
                      "again" "further" "then" "once" "here" "there"
                      "when" "where" "why" "how" "all" "both" "each"
                      "few" "more" "most" "other" "some" "such" "no" "nor"
                      "not" "only" "own" "same" "so" "than" "too" "very"
                      "can" "just" "don" "now" "and" "but"
                      "or" "if" "while" "that" "this" "these" "those"
                      "its" "he" "she" "they" "them" "his" "her" "their"
                      "my" "your" "our" "what" "which" "who" "whom" "whose"
                      "ought" "need" "dare"
                      "able" "according" "actually" "afterwards" "against"
                      "almost" "alone" "along" "already" "also"
                      "although" "always" "among" "amongst" "amount" "any"
                      "anyhow" "anyone" "anything" "anywhere" "around"
                      "became" "become" "becomes" "becoming" "beforehand"
                      "behind" "beside" "besides" "beyond" "certainly"
                      "clearly" "couldn" "despite" "done" "down" "either"
                      "elsewhere" "enough" "etc" "everyone" "everything"
                      "everywhere" "except" "first" "get" "gets" "getting"
                      "give" "given" "gives" "go" "goes" "gone" "got"
                      "gotten" "hadn" "hardly" "herself" "himself" "hither"
                      "however" "indeed" "instead" "keep" "keeps" "kept"
                      "last" "latter" "least" "less" "let" "lets" "likely"
                      "little" "long" "look" "looking" "looks" "made" "make"
                      "makes" "man" "many" "meanwhile" "merely" "mightn"
                      "mine" "moreover" "mostly" "much" "mustn" "myself"
                      "namely" "near" "necessary" "neither" "never"
                      "nevertheless" "next" "nine" "nobody" "none"
                      "nothing" "nowhere" "obviously" "often" "one"
                      "onto" "others" "otherwise" "oughtn" "ourselves"
                      "particular" "particularly" "perhaps" "please" "previously"
                      "rather" "really" "regardless" "right" "round"
                      "say" "says" "second" "see" "seem" "seemed"
                      "seeming" "seems" "seen" "selves" "several" "shalln"
                      "show" "showed" "shown" "shows" "since" "six"
                      "somehow" "someone" "something" "sometime"
                      "sometimes" "somewhere" "still" "suppose"
                      "taken" "tell" "tends" "thank" "thanks"
                      "thats" "theres" "thereafter" "thereby" "therefore"
                      "therein" "thereupon" "theyre" "though" "thought"
                      "three" "throughout" "thru" "together" "toward" "towards"
                      "twice" "two" "unless" "unlikely" "until"
                      "upon" "used" "using" "various" "via" "want"
                      "wants" "wasn" "way" "well" "went" "whatever"
                      "whence" "whenever" "whereafter" "whereas"
                      "whereby" "wherein" "whereupon" "wherever"
                      "whether" "whichever" "whilst" "whole"
                      "willing" "within" "without" "wouldn" "yet"
                      "youre" "yours" "yourself" "yourselves"}]
    (remove stop-words words)))

(defn- text-from-envelope [envelope]
  (str (pr-str (:envelope/task envelope))
       " "
       (pr-str (:envelope/subgoals envelope))
       " "
       (pr-str (:envelope/residue envelope))
       " "
       (pr-str (:envelope/evidence envelope))))

(defn- heuristic-retention [envelope original-context]
  (let [env-words (extract-significant-words (text-from-envelope envelope))
        ctx-words (extract-significant-words original-context)
        score (jaccard env-words ctx-words)]
    {:score score
     :details {:method :heuristic
               :env-word-count (count env-words)
               :ctx-word-count (count ctx-words)
               :jaccard score}}))

(defn- heuristic-regression [envelope original-context]
  (let [cc (crosscheck/crosscheck* envelope {:tasks [] :subgoals []})
        mismatches (if cc (count (:crosscheck/mismatches cc)) 0)
        score (max 0.0 (min 1.0 (- 1.0 (* mismatches 0.1))))]
    {:score score
     :details {:method :heuristic
               :mismatches mismatches
               :crosscheck (if cc (:crosscheck/envelope cc) nil)}}))

(defn- heuristic-hallucination [envelope original-context]
  (let [residue-words (extract-significant-words (pr-str (:envelope/residue envelope)))
        ctx-words (extract-significant-words original-context)
        score (jaccard residue-words ctx-words)]
    {:score score
     :details {:method :heuristic
               :residue-word-count (count residue-words)
               :ctx-word-count (count ctx-words)
               :jaccard score}}))

;; ---------------------------------------------------------------------------
;; Actual scoring implementations
;; ---------------------------------------------------------------------------

(defn eval-retention-score
  "Score retention for `envelope` against `original-context`.

   When called with 2 args, uses heuristic Jaccard similarity of
   significant words between envelope and original context.

   When called with 3 args, uses caller-supplied `score` (host/judge
   override).

   `score` must be a number between 0.0 and 1.0.
   `details` is an optional map with scoring rationale.

   Returns an eval record map."
  ([envelope original-context]
   (let [heuristic (heuristic-retention envelope original-context)
         score (:score heuristic)
         details (:details heuristic)]
     (eval-record eval-retention score (classify :retention score) details)))
  ([envelope original-context score]
   (eval-retention-score envelope original-context score nil))
  ([envelope original-context score details]
   (envelope/validate-envelope envelope)
   (when-not (number? score)
     (throw (err/error :context/compression-invalid
                       "score must be a number"
                       {:score (err/sanitize score)})))
   (when (or (< score 0.0) (> score 1.0))
     (throw (err/error :context/compression-invalid
                       "score must be between 0.0 and 1.0"
                       {:score score})))
   (eval-record eval-retention score (classify :retention score) details)))

(defn eval-regression-score
  "Score regression for `envelope` against `original-context`.

   When called with 2 args, uses heuristic based on crosscheck mismatches.

   When called with 3 args, uses caller-supplied `score` (host/judge
   override).

   `score` must be a number between 0.0 and 1.0.
   `details` is an optional map with scoring rationale.

   Returns an eval record map."
  ([envelope original-context]
   (let [heuristic (heuristic-regression envelope original-context)
         score (:score heuristic)
         details (:details heuristic)]
     (eval-record eval-regression score (classify :regression score) details)))
  ([envelope original-context score]
   (eval-regression-score envelope original-context score nil))
  ([envelope original-context score details]
   (envelope/validate-envelope envelope)
   (when-not (number? score)
     (throw (err/error :context/compression-invalid
                       "score must be a number"
                       {:score (err/sanitize score)})))
   (when (or (< score 0.0) (> score 1.0))
     (throw (err/error :context/compression-invalid
                       "score must be between 0.0 and 1.0"
                       {:score score})))
   (eval-record eval-regression score (classify :regression score) details)))

(defn eval-hallucination-score
  "Score hallucination for `envelope` against `original-context`.

   When called with 2 args, uses heuristic Jaccard similarity of
   significant words between residue claims and original context.

   When called with 3 args, uses caller-supplied `score` (host/judge
   override).

   `score` must be a number between 0.0 and 1.0.
   `details` is an optional map with scoring rationale.

   Returns an eval record map."
  ([envelope original-context]
   (let [heuristic (heuristic-hallucination envelope original-context)
         score (:score heuristic)
         details (:details heuristic)]
     (eval-record eval-hallucination score (classify :hallucination score) details)))
  ([envelope original-context score]
   (eval-hallucination-score envelope original-context score nil))
  ([envelope original-context score details]
   (envelope/validate-envelope envelope)
   (when-not (number? score)
     (throw (err/error :context/compression-invalid
                       "score must be a number"
                       {:score (err/sanitize score)})))
   (when (or (< score 0.0) (> score 1.0))
     (throw (err/error :context/compression-invalid
                       "score must be between 0.0 and 1.0"
                       {:score score})))
   (eval-record eval-hallucination score (classify :hallucination score) details)))

;; ---------------------------------------------------------------------------
;; Eval summary
;; ---------------------------------------------------------------------------

(defn eval-summary
  "Build an eval summary from a collection of eval records.

   `records` should be the output of the individual eval score functions.

   Returns a map with:
     :eval/overall-status — the worst status among all records
     :eval/records — the original records vector"
  [records]
  (when-not (coll? records)
    (throw (err/error :context/compression-invalid
                      "records must be a collection"
                      {:value (err/sanitize records)})))
  (let [statuses (map :eval/status records)
        worst (reduce (fn [acc s]
                        (cond
                          (= s status-fail) status-fail
                          (= s status-warn) (if (= acc status-pass) status-warn acc)
                          :else acc))
                      status-pass
                      statuses)]
    {:eval/overall-status worst
     :eval/records (vec records)}))

(defn passing?
  "True when `eval-summary` has overall status :status/pass."
  [summary]
  (= status-pass (:eval/overall-status summary)))

(defn failing?
  "True when `eval-summary` has overall status :status/fail."
  [summary]
  (= status-fail (:eval/overall-status summary)))
