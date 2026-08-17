(ns evoclj.eval.leakage
  "Coarse-grained leakage / contamination heuristic for candidate genomes.

  PURPOSE
  -------
  Before a candidate genome is scored against a hidden evaluation case,
  we want a cheap RED-LIGHT check that the candidate is not obviously
  memorising / hard-coding the specific case it is being graded on
  (overfitting to the exam, i.e. leaking hidden selection-set keywords
  into the candidate's program text).

  WHAT THIS IS NOT
  ----------------
  This is a COARSE, heuristic substring scan. It is NOT a cryptographic
  guarantee, NOT a semantic detector, and NOT a proof of independence.
  A candidate that paraphrases a case, or that uses the same concept
  under a different surface token, will not be caught. It is intended to
  reject the lazy failure mode (copying the problem statement verbatim)
  and to be run as a deterministic pre-filter; it must never be the only
  gate a candidate passes or fails on.

  CONTRACT
  --------
  - pure: no IO, no randomness, fully deterministic.
  - (extract-tokens case-text) -> #{sigificant lowercased tokens}
  - (contaminated? candidate-text case-text) -> boolean
      true iff any significant token from case-text appears as a
      substring of (lowercased candidate-text).")

(def ^:private stop-words
  "Common English stopwords dropped so that boilerplate prose around a
  case does not trigger false contaminations."
  #{"the" "and" "for" "with" "this" "that" "return" "def" "let" "when"
    "from" "into" "true" "false" "nil" "var" "fn" "do" "if" "else"
    "then" "not" "are" "was" "has" "have" "you" "your"})

(defn extract-tokens
  "Return the set of significant tokens in `text`.

  A token is a run of >= 4 alphanumeric characters, lowercased. Stopwords
  (see `stop-words`) are removed. Pure and deterministic."
  [text]
  (let [lower (.toLowerCase (str text))]
    (->> (re-seq #"[a-z0-9]{4,}" lower)
         (remove stop-words)
         (set))))

(defn contaminated?
  "Coarse red-light contamination check.

  `candidate-text` is the candidate genome's program/source text;
  `case-text` is one hidden selection case's text (problem statement,
  expected output, identifiers, ...).

  Returns true iff ANY significant token (length >= 4, lowercased, stop
  words removed) extracted from `case-text` occurs as a substring of the
  lowercased `candidate-text` — i.e. the candidate text appears to embed
  case-specific vocabulary. Otherwise false.

  Pure and deterministic; a heuristic, not a guarantee (see ns doc)."
  [candidate-text case-text]
  (let [haystack (.toLowerCase (str candidate-text))]
    (boolean
     (some (fn [tok] (not= -1 (.indexOf haystack tok)))
           (extract-tokens case-text)))))
