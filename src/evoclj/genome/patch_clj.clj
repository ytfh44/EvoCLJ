(ns evoclj.genome.patch-clj
  "Clojure source-form patch operations for Task 7.4.

  This namespace owns the source-preserving form ops (:replace-form
  :insert-form :delete-form) built on rewrite-clj 1.2.55. Each
  `apply-op` takes the CURRENT text of the target .clj file plus the op
  map, parses it with rewrite-clj.parser/parse-string-all, selects one
  form, and rewrites the tree through rewrite-clj.zip (z/edn z/next
  z/sexpr z/replace z/remove z/insert-left z/insert-right
  z/insert-newline-left z/insert-newline-right z/root-string) so that
  comments and whitespace OUTSIDE the selected form survive byte for
  byte (Step 4: replace a target var/form while preserving unrelated
  comments and whitespace).

  SELECTOR RULE (normative): a selector identifies a form by its value,
  matched against the form's element sequence:

  - a scalar (keyword/symbol/string) matches the first form whose
    element sequence starts with that value (so `defn` selects
    `(defn run ...)`, and `run` selects `(run ...)`), and
  - a vector matches the first form whose element sequence starts with
    the selector's elements in order; a positive integer element is a
    position marker requiring an element at that index to exist
    (`[:defn 1]` selects any `(defn <name> ...)` form).

  \"First\" means first in depth-first source order. Forms whose value
  is not a sequence (atoms, keywords, strings, comments, whitespace)
  are never selected. A selector that matches nothing fails with
  :patch/form-not-found before any edit is made.

  Results are deterministic: same source plus same op yields the same
  output text (Global Constraint 6). The op's :expect/hash is checked
  by the orchestrator before this namespace runs."
  (:require [evoclj.kernel.error :as err]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.zip :as z]))

(defn- form-matches?
  "True when the form value `v` is a sequence whose leading elements
  match `sel` (a scalar, normalized to a one-element selector, or a
  vector; a positive integer element requires an element at that
  index to exist, any value)."
  [v sel]
  (let [sel (if (vector? sel) sel [sel])]
    (and (sequential? v)
         (let [es (vec v)]
           (loop [i 0 se (seq sel)]
             (cond
               (empty? se) true
               (>= i (count es)) false
               :else (let [s (first se)]
                       (if (pos-int? s)
                         (recur (inc i) (seq (rest se)))
                         (if (= s (nth es i))
                           (recur (inc i) (seq (rest se)))
                           false)))))))))

(defn- find-form
  "Locate the first form matching `sel` in source order, starting at the
  z/edn location (the first non-whitespace/non-comment child of the
  root), or nil when nothing matches."
  [zloc sel]
  (loop [z zloc]
    (cond
      (z/end? z) nil
      (form-matches? (z/sexpr z) sel) z
      :else (recur (z/next z)))))

(defn- parse!
  "Parse the target file as a Clojure source tree. Any read failure
  fails closed with :patch/clj-invalid."
  [content path]
  (try
    (parser/parse-string-all content)
    (catch Exception e
      (throw (err/error :patch/clj-invalid
                        "target file is not valid Clojure source"
                        {:path path :message (.getMessage e)})))))

(defn- select!
  "Parse and locate the target form, or throw :patch/form-not-found."
  [content op]
  (let [zloc (z/edn (parse! content (:file op)))
        target (find-form zloc (:selector op))]
    (when-not target
      (throw (err/error :patch/form-not-found
                        "no form matches the selector"
                        {:path (:file op) :selector (err/sanitize (:selector op))})))
    target))

(defn- replace-form-op [content op]
  (let [target (select! content op)]
    (z/root-string (z/replace target (:form op)))))

(defn- insert-form-op [content op]
  (let [target (select! content op)
        form (:form op)]
    (z/root-string
     (if (= :before (:position op))
       (z/insert-newline-left (z/insert-left target form))
       (z/insert-newline-right (z/insert-right target form))))))

(defn- delete-form-op [content op]
  (let [target (select! content op)]
    (z/root-string (z/remove target))))

;; --- public entry point ----------------------------------------------------

(defn apply-op
  "Apply one source-form op to `content` (the current text of the op's
  target file). Returns the new text, or throws ExceptionInfo with a
  stable :error/type (see the namespace docstring)."
  [content op]
  (case (:op op)
    :replace-form (replace-form-op content op)
    :insert-form (insert-form-op content op)
    :delete-form (delete-form-op content op)
    (throw (err/error :patch/op-invalid
                      "unknown source-form op"
                      {:op (:op op)}))))
