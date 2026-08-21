(ns evoclj.compiler.program
  "Static discovery and validation of evolvable SCI programs (component).

  Genome programs are DECLARED via descriptors, never inferred from
  arbitrary source files. compile-program-descriptor validates a
  descriptor map against a loaded Genome (the evoclj.genome.load
  result) and returns a pure, fully serializable ProgramDescriptor:

    {:program/id :program/route
     :file \"programs/route.clj\"      ; canonical relative path
     :entry 'agent.route/run          ; declared entry symbol
     :input-schema :schema/route-input
     :output-schema :schema/intent-or-route
     :source/digest \"sha256:<64 hex>\" ; the Genome's canonical file digest
     :source/ns ['agent.route]}        ; namespaces declared by ns forms

  Validation covers file existence, path, entry symbol, and source
  readability, and NEVER executes the program (Global Constraint 22:
  the boundary value is data, not code):

  - The file must normalize to a canonical relative path (no
    traversal, no absolute path), must end in .clj, and must be present
    in the loaded Genome's :files map. The source is decoded from the
    immutable loaded bytes — compilation performs no IO of its own.
  - Source readability means the source parses as Clojure structure.
    Parsing uses rewrite-clj (structure only): nothing is read with a
    host reader and nothing is evaluated.
  - The entry must be a qualified symbol. If the source declares
    namespace(s) via ns forms, the entry's namespace must be one of
    them (when no ns form exists, the entry's namespace is implicitly
    declared). The entry's simple name must be defined at top level
    (def/defn/defn-) so a typo in the descriptor fails at compile time.
  - Compile-policy inspection (best-effort — the SCI sandbox remains
    the final enforcement layer) rejects, where detectable: load-file,
    eval, require/use of undeclared host namespaces, Java class
    literals (e.g. (System/getenv ...), java.io.File, String), host
    interop special forms (. and ..), ns :import clauses, and #=
    reader-eval forms. Quoted data is inert and is not inspected.

  Program descriptors are kept as an in-memory registry validated by
  callers (choice (a) in the task brief); the manifest and topology
  schemas are untouched, and the seed Genome's program registry wiring
  arrives in component/6.x.

  Error types: :program/invalid (malformed descriptor, invalid entry,
  non-.clj file, invalid genome value, entry not defined, entry
  namespace mismatch — distinguished by :reason), :program/path-invalid
  (wraps evoclj.genome.path), :program/file-missing,
  :program/parse-error, and :program/policy-violation (with :reason
  distinguishing :load-file, :eval, :reader-eval, :host-namespace,
  :class-literal, :interop, :undeclared-require, :host-import,
  :invalid-ns-name)."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.genome.path :as path]
            [rewrite-clj.node :as rn]
            [rewrite-clj.parser :as rp]
            [rewrite-clj.zip :as rz])
  (:import (java.nio.charset StandardCharsets)))

;; --- policy rule data ------------------------------------------------------

(def ^:private forbidden-names
  "Symbol names rejected wherever they appear: they grant execution or
  host access and have no legitimate use in an evolvable program."
  #{"eval" "load-file"})

(def ^:private require-names
  "Symbol names that load host namespaces when called."
  #{"require" "use"})

(def ^:private interop-names
  "The host interop special forms."
  #{"." ".."})

(def ^:private core-namespace
  "clojure.core is always part of the static allowlist; requiring or
  fully qualifying into it is harmless under SCI."
  'clojure.core)

(def ^:private max-form-chars 200)

(defn- bounded-str
  "A bounded EDN string for one form, so error data stays small and
  serializable."
  [v]
  (let [s (pr-str v)]
    (if (> (count s) max-form-chars)
      (str (subs s 0 (- max-form-chars 3)) "...")
      s)))

(defn- violation
  "One static policy violation: {:reason <keyword> :symbol <string or
  nil> :form <bounded string or nil>}. Fully serializable."
  [reason symbol form]
  {:reason reason
   :symbol (when symbol (str symbol))
   :form (when (some? form) (bounded-str form))})

(defn- class-like-name?
  "True when a bare symbol name looks like a Java class reference: a
  capitalized name (String, System) or a name containing dots
  (java.io.File, java.io.File., foo.bar). Clojure var names are
  conventionally lowercase and never contain dots, so this is safe for
  pure program code; the only dotted bare symbols in legitimate source
  are the exempt positions handled below (ns form names and
  require/use lib specs)."
  [s]
  (or (str/includes? s ".")
      (re-matches #"^[A-Z].*" s)))

;; --- the structure-only walker --------------------------------------------

(declare scan-list scan-value)

(defn- scan-symbol
  "Policy check for one symbol in any position. Returns the first
  violation map, or nil. Qualified symbols are allowed only when their
  namespace is in the declared allowlist; bare symbols are rejected
  when they name a forbidden function, an interop special form, or a
  class-like reference."
  [s declared]
  (let [n (name s)]
    (cond
      (contains? forbidden-names n)
      (violation (keyword n) s nil)

      (contains? interop-names n)
      (violation :interop s nil)

      (namespace s)
      (if (contains? declared (symbol (namespace s)))
        nil
        (violation :host-namespace s nil))

      (class-like-name? n)
      (violation :class-literal s nil)

      :else nil)))

(defn- scan-elements
  "Scan every element of a seqable collection, returning the first
  violation in structural order, or nil."
  [coll declared]
  (loop [xs (seq coll)]
    (if-not xs
      nil
      (or (scan-value (first xs) declared)
          (recur (next xs))))))

(defn- scan-value
  "Policy check for one parsed value (a form or subform). Keywords,
  numbers, strings, chars, booleans, and nil are inert; collections are
  scanned recursively; lists go through head-aware handling in
  scan-list."
  [v declared]
  (cond
    (seq? v) (scan-list v declared)
    (vector? v) (scan-elements v declared)
    (set? v) (scan-elements v declared)
    (map? v) (scan-elements (mapcat identity v) declared)
    (symbol? v) (scan-symbol v declared)
    :else nil))

(defn- lib-symbol
  "Extract the namespace symbol from a require/use lib spec: 'foo,
  '[foo & opts], foo, or [foo & opts]. Returns nil for malformed
  specs."
  [spec]
  (cond
    (and (seq? spec) (= "quote" (name (first spec))))
    (lib-symbol (second spec))

    (symbol? spec)
    spec

    (and (vector? spec) (symbol? (first spec)))
    (first spec)

    :else nil))

(defn- check-lib
  "A require/use target must be in the declared allowlist; anything
  else is a require of an undeclared (host) namespace."
  [lib declared]
  (if (and lib (contains? declared lib))
    nil
    (violation :undeclared-require lib nil)))

(defn- check-require-libs
  "Check the lib specs of a (require ...)/(use ...) form or of a
  (:require ...)/(:use ...) ns clause."
  [specs declared]
  (loop [ss (seq specs)]
    (if-not ss
      nil
      (or (check-lib (lib-symbol (first ss)) declared)
          (recur (next ss))))))

(defn- check-ns-clauses
  "Scan the clauses of an ns form (everything after the namespace
  name). :require/:use clauses are checked against the allowlist;
  :import clauses are rejected outright (they import host classes);
  every other clause is scanned generically."
  [clauses declared]
  (loop [cs (seq clauses)]
    (if-not cs
      nil
      (let [clause (first cs)]
        (if (and (seq? clause) (keyword? (first clause)))
          (let [head-k (first clause)]
            (cond
              (contains? #{:require :use} head-k)
              (or (check-require-libs (rest clause) declared)
                  (recur (next cs)))

              (= :import head-k)
              (violation :host-import head-k clause)

              :else
              (or (scan-elements clause declared)
                  (recur (next cs)))))
          (or (scan-value clause declared)
              (recur (next cs))))))))

(defn- scan-list
  "Head-aware policy check for a list form. (ns ...) declares the
  namespace and exempts its name; (require ...)/(use ...) check their
  lib specs; (quote ...) is inert data and is not inspected; every
  other form is scanned generically (the head is just another symbol
  position, so e.g. (System/getenv ...) fails the qualified-symbol
  allowlist and (java.io.File. ...) fails the class-like rule)."
  [form declared]
  (let [head (first form)]
    (cond
      (and (symbol? head) (= "ns" (name head)))
      (let [ns-name (second form)]
        (if-not (and (symbol? ns-name) (nil? (namespace ns-name)))
          (violation :invalid-ns-name ns-name form)
          (check-ns-clauses (drop 2 form) (conj declared ns-name))))

      (and (symbol? head) (contains? require-names (name head)))
      (check-require-libs (rest form) declared)

      (and (symbol? head) (= "quote" (name head)))
      nil

      :else
      (scan-elements form declared))))

;; --- reader-eval (#=) detection -------------------------------------------

(defn- reader-eval-violation
  "rewrite-clj parses #= forms as a :eval reader-macro node. The zip
  walk below finds that node type directly, so #= is reported as
  :reader-eval and can never be confused with a real (eval ...) call
  (whose sexpr happens to be identical after node->sexpr conversion)."
  [root]
  (loop [loc (rz/of-node root)]
    (if (rz/end? loc)
      nil
      (if (= :eval (rn/tag (rz/node loc)))
        (violation :reader-eval "#="
                   (try (rz/sexpr loc)
                        (catch Exception _ :eval)))
        (recur (rz/next loc))))))

;; --- structure inspection --------------------------------------------------

(defn- top-level-forms
  "The parsed top-level forms of a forms root as a seq of values
  (rewrite-clj sexprs — structure only, nothing is evaluated).
  Comment, newline, and whitespace nodes carry no form and are
  skipped; all remaining root children are form-bearing nodes."
  [root]
  (map rn/sexpr
       (remove #(contains? #{:whitespace :comment :newline} (rn/tag %))
               (rn/children root))))

(defn- top-level-ns-names
  "The namespaces declared by top-level (ns ...) forms, as a sorted set
  of symbols (deterministic regardless of source order)."
  [root]
  (into (sorted-set)
        (keep (fn [f]
                (when (and (seq? f)
                           (symbol? (first f))
                           (= "ns" (name (first f))))
                  (second f))))
        (top-level-forms root)))

(defn- top-level-def-names
  "The simple names defined at top level by def/defn/defn- forms
  (def names may carry metadata before the name)."
  [root]
  (into #{}
        (keep (fn [f]
                (when (and (seq? f) (symbol? (first f)))
                  (let [head (name (first f))]
                    (cond
                      (= "defn" head) (second f)
                      (= "defn-" head) (second f)
                      (= "def" head)
                      (let [candidate (second f)]
                        (if (map? candidate)
                          (nth f 2 nil)
                          candidate)))))))
        (top-level-forms root)))

(defn- inspect-program
  "Parse `source` with rewrite-clj (structure only — nothing is
  evaluated) and return {:declared <sorted vec of ns symbols>
  :violation <first policy violation or nil>}. Throws the rewrite-clj
  parse exception when the source is not parseable."
  [source]
  (let [root (rp/parse-string-all source)
        declared (top-level-ns-names root)
        allowlist (conj declared core-namespace)
        violation (or (reader-eval-violation root)
                      (scan-elements (top-level-forms root) allowlist))]
    {:declared (vec declared)
     :violation violation}))

;; --- public policy entry point --------------------------------------------

(defn policy-violation
  "Return the first static compile-policy violation map for `source`,
  or nil when the source is clean.

  `source` is parsed with rewrite-clj (structure only — never
  evaluated). A violation map is {:reason <keyword> :symbol <string or
  nil> :form <bounded string or nil>} where :reason is one of
  :load-file, :eval, :reader-eval, :host-namespace, :class-literal,
  :interop, :undeclared-require, :host-import, :invalid-ns-name. The
  policy is best-effort static inspection; the SCI sandbox remains the
  final enforcement layer. Quoted data is inert and never inspected.

  Throws the rewrite-clj parse exception when `source` is not
  parseable (compile-program-descriptor converts that into
  :program/parse-error)."
  [source]
  (:violation (inspect-program source)))

;; --- descriptor shape validation ------------------------------------------

(def ^:private descriptor-keys
  "The closed key set of a program descriptor; anything else is
  rejected (closed maps at trust boundaries, as in component)."
  #{:program/id :file :entry :input-schema :output-schema})

(defn- validate-descriptor!
  "Require the descriptor to be a map with exactly the closed key set
  and the expected value types."
  [descriptor]
  (when-not (map? descriptor)
    (throw (err/error :program/invalid
                      "program descriptor must be a map"
                      {:reason :invalid-descriptor
                       :value (err/sanitize descriptor)})))
  (when-let [extra (seq (remove descriptor-keys (keys descriptor)))]
    (throw (err/error :program/invalid
                      "program descriptor contains unknown keys"
                      {:reason :unknown-descriptor-key
                       :keys (vec extra)})))
  (doseq [[k v] [[:program/id (:program/id descriptor)]
                 [:file (:file descriptor)]
                 [:entry (:entry descriptor)]
                 [:input-schema (:input-schema descriptor)]
                 [:output-schema (:output-schema descriptor)]]]
    (when (nil? v)
      (throw (err/error :program/invalid
                        (str "program descriptor is missing required key " k)
                        {:reason :invalid-descriptor :key k})))
    (cond
      (= k :program/id)
      (when-not (keyword? v)
        (throw (err/error :program/invalid
                          ":program/id must be a keyword"
                          {:reason :invalid-descriptor :key k :value v})))

      (= k :file)
      (when-not (string? v)
        (throw (err/error :program/invalid
                          ":file must be a string"
                          {:reason :invalid-descriptor :key k :value v})))

      (= k :entry)
      (when-not (and (symbol? v) (namespace v))
        (throw (err/error :program/invalid
                          ":entry must be a qualified symbol (ns/name)"
                          {:reason :invalid-entry :entry v})))

      :else
      (when-not (keyword? v)
        (throw (err/error :program/invalid
                          (str k " must be a keyword")
                          {:reason :invalid-descriptor :key k :value v}))))))

(defn- validate-genome!
  [loaded-genome]
  (when-not (and (map? loaded-genome) (map? (:files loaded-genome)))
    (throw (err/error :program/invalid
                      "loaded genome must be a map with a :files map"
                      {:reason :invalid-genome
                       :value (err/sanitize loaded-genome)}))))

;; --- program file resolution ----------------------------------------------

(defn- resolve-program-file!
  "Normalize the descriptor's :file to its canonical relative path,
  require a .clj extension, and require the file to exist in the loaded
  Genome's :files map. Returns the canonical path and its payload."
  [descriptor loaded-genome]
  (let [file (:file descriptor)
        path (try
               (path/normalize-relative-path file)
               (catch clojure.lang.ExceptionInfo e
                 (throw (err/error :program/path-invalid
                                   "program file path is not a valid canonical relative path"
                                   {:file file
                                    :reason (or (:reason (ex-data e)) :path-invalid)}))))]
    (when-not (str/ends-with? path ".clj")
      (throw (err/error :program/invalid
                        "program source must be a .clj file"
                        {:reason :not-clojure-source :file path})))
    (if-let [payload (get-in loaded-genome [:files path])]
      [path payload]
      (throw (err/error :program/file-missing
                        "program file is not part of the loaded genome"
                        {:file path})))))

;; --- public entry point ----------------------------------------------------

(defn compile-program-descriptor
  "Validate a declared program descriptor against a loaded Genome and
  return a pure serializable ProgramDescriptor.

  `descriptor` is the component shape: {:program/id <keyword> :file
  \"programs/route.clj\" :entry 'agent.route/run :input-schema <keyword>
  :output-schema <keyword>}. `loaded-genome` is the evoclj.genome.load
  result (its :files map holds the immutable program bytes).

  Validation order: descriptor shape (closed key set, value types) ->
  canonical path + .clj extension -> file existence in the bundle ->
  source readability (rewrite-clj structure parse) -> compile-policy
  inspection -> entry symbol consistency (namespace declared by the
  source, simple name defined at top level). Nothing is executed.

  Returns {:program/id ... :file <canonical path> :entry ... 
  :input-schema ... :output-schema ... :source/digest <sha256:<64
  hex>> :source/ns [<declared namespaces>]}, where :source/digest is
  the Genome's canonical CRLF-normalized text digest of the program
  file (evoclj.genome.hash/text-digest — the same digest that appears
  in the Genome tree), so a compiled program's digest always matches
  the Genome that declared it. The result round-trips through
  pr-str / clojure.edn read-string (Global Constraint 22).

  Throws ExceptionInfo with a stable :error/type: :program/invalid
  (:reason distinguishes :invalid-descriptor, :unknown-descriptor-key,
  :invalid-entry, :not-clojure-source, :invalid-genome, :entry-missing,
  :entry-namespace-mismatch), :program/path-invalid,
  :program/file-missing, :program/parse-error, or
  :program/policy-violation (see policy-violation for :reason values)."
  [descriptor loaded-genome]
  (validate-descriptor! descriptor)
  (validate-genome! loaded-genome)
  (let [entry (:entry descriptor)
        [file payload] (resolve-program-file! descriptor loaded-genome)
        source (String. ^bytes (byte-array (:bytes payload))
                        StandardCharsets/UTF_8)
        {:keys [declared violation]} (try
                                       (inspect-program source)
                                       (catch Exception e
                                         (throw (err/error :program/parse-error
                                                           "program source failed to parse as Clojure structure"
                                                           {:file file
                                                            :message (.getMessage e)}))))
        entry-ns (symbol (namespace entry))]
    (when violation
      (throw (err/error :program/policy-violation
                        "program violates the static compile policy"
                        (assoc violation :file file :program/id (:program/id descriptor)))))
    (when (and (seq declared)
               (not (contains? (set declared) entry-ns)))
      (throw (err/error :program/invalid
                        "entry namespace is not declared by the program source"
                        {:reason :entry-namespace-mismatch
                         :entry entry :entry/ns entry-ns
                         :declared declared})))
    (let [root (rp/parse-string-all source)
          def-names (top-level-def-names root)]
      (when-not (contains? def-names (symbol (name entry)))
        (throw (err/error :program/invalid
                          "entry var is not defined at the program's top level"
                          {:reason :entry-missing
                           :entry entry :file file})))
      {:program/id (:program/id descriptor)
       :file file
       :entry entry
       :input-schema (:input-schema descriptor)
       :output-schema (:output-schema descriptor)
       :source/digest (:digest payload)
       :source/ns declared})))
