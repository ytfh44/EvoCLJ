(ns evoclj.genome.patch-text
  "Bounded text patch operations for Task 7.4.

  This namespace owns the text ops (:insert-text :replace-text
  :delete-text). Each `apply-op` takes the CURRENT text of the target
  file plus the op map and returns the new text.

  BOUNDED MATCH RULE (normative): a text op may only touch an
  explicitly bounded source range — never an unconstrained global
  replace. An anchor is either:

  - an exact string, which must occur EXACTLY ONCE in the file
    (zero occurrences is :patch/anchor-not-found, two or more is
    :patch/anchor-ambiguous — both fail closed), or
  - a positive integer, a 1-based line offset selecting that whole
    line (a line outside the file fails with :patch/anchor-not-found).

  The operation then applies to exactly that bounded range: replace
  substitutes the range with :text, delete removes it, insert places
  :text at the exact boundary before/after the range. The op's
  :expect/hash (required for replace/delete, optional for insert) is
  checked by the orchestrator (evoclj.genome.patch) BEFORE this
  namespace runs, so a stale patch can never apply.

  Results are byte-for-byte deterministic: same input text plus same
  op yields the same output text (Global Constraint 6)."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err]))

;; --- string anchors --------------------------------------------------------

(defn- string-occurrences
  "Start indices of every occurrence of `anchor` in `content`."
  [content anchor]
  (loop [i 0 acc []]
    (let [j (.indexOf ^String content ^String anchor i)]
      (if (neg? j)
        acc
        (recur (+ j (count anchor)) (conj acc j))))))

(defn- resolve-string-anchor!
  "Require the exact-string anchor to occur exactly once; return its
  [start end) range."
  [content anchor path]
  (let [idxs (string-occurrences content anchor)]
    (cond
      (empty? idxs)
      (throw (err/error :patch/anchor-not-found
                        "text anchor does not occur in the target file"
                        {:path path :anchor anchor}))
      (> (count idxs) 1)
      (throw (err/error :patch/anchor-ambiguous
                        "text anchor is not unique; a bounded patch must match exactly one range"
                        {:path path :anchor anchor :occurrences (count idxs)}))
      :else
      (let [i (first idxs)]
        [i (+ i (count anchor))]))))

;; --- line anchors ----------------------------------------------------------

(defn- lines-of
  "Split `content` into lines with a -1 limit so a trailing empty line
  survives joining (\"a\\nb\\n\" -> [\"a\" \"b\" \"\"])."
  [content]
  (str/split content #"\n" -1))

(defn- resolve-line-anchor!
  "Require the 1-based line anchor to exist; return the 0-based index."
  [content line path]
  (let [n (count (lines-of content))]
    (when (or (< line 1) (> line n))
      (throw (err/error :patch/anchor-not-found
                        "line anchor is outside the target file"
                        {:path path :line line :lines n})))
    (dec line)))

(defn- anchor-range
  "The bounded range selected by the op's anchor: [start end) character
  offsets, or a {:lines lines :index i} map for line anchors."
  [content op]
  (let [a (:anchor op)]
    (cond
      (string? a) {:kind :string :range (resolve-string-anchor! content a (:file op))}
      (pos-int? a) {:kind :line :index (resolve-line-anchor! content a (:file op))}
      :else (throw (err/error :patch/op-invalid
                              "text op :anchor must be an exact string or a 1-based line"
                              {:path (:file op) :anchor a})))))

;; --- op implementations ----------------------------------------------------

(defn- replace-string [content [start end] text]
  (str (subs content 0 start) text (subs content end)))

(defn- replace-line [content index text]
  (let [lines (lines-of content)]
    (str/join "\n" (assoc lines index text))))

(defn- delete-string [content [start end]]
  (str (subs content 0 start) (subs content end)))

(defn- delete-line [content index]
  (let [lines (lines-of content)]
    (str/join "\n" (into (subvec (vec lines) 0 index)
                         (subvec (vec lines) (inc index))))))

(defn- insert-string-before [content [start _] text]
  (str (subs content 0 start) text (subs content start)))

(defn- insert-string-after [content [_ end] text]
  (str (subs content 0 end) text (subs content end)))

(defn- insert-line-before [content index text]
  (let [lines (lines-of content)]
    (str/join "\n" (into (conj (subvec (vec lines) 0 index) text)
                         (subvec (vec lines) index)))))

(defn- insert-line-after [content index text]
  (let [lines (lines-of content)]
    (str/join "\n" (into (conj (subvec (vec lines) 0 (inc index)) text)
                         (subvec (vec lines) (inc index))))))

(defn- replace-text-op [content op]
  (let [r (anchor-range content op)]
    (if (= :string (:kind r))
      (replace-string content (:range r) (:text op))
      (replace-line content (:index r) (:text op)))))

(defn- delete-text-op [content op]
  (let [r (anchor-range content op)]
    (if (= :string (:kind r))
      (delete-string content (:range r))
      (delete-line content (:index r)))))

(defn- insert-text-op [content op]
  (let [r (anchor-range content op)
        before? (= :before (:position op))]
    (if (= :string (:kind r))
      (let [[start end] (:range r)]
        (if before?
          (insert-string-before content [start end] (:text op))
          (insert-string-after content [start end] (:text op))))
      (if before?
        (insert-line-before content (:index r) (:text op))
        (insert-line-after content (:index r) (:text op))))))

;; --- public entry point ----------------------------------------------------

(defn apply-op
  "Apply one text op to `content` (the current text of the op's target
  file). Returns the new text, or throws ExceptionInfo with a stable
  :error/type (see the namespace docstring)."
  [content op]
  (case (:op op)
    :replace-text (replace-text-op content op)
    :delete-text (delete-text-op content op)
    :insert-text (insert-text-op content op)
    (throw (err/error :patch/op-invalid
                      "unknown text op"
                      {:op (:op op)}))))
