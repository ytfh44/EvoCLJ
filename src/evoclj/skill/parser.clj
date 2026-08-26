(ns evoclj.skill.parser
  "SKILL.md parser with YAML frontmatter strictness.

  Frontmatter is `---` delimited YAML at the top of SKILL.md.
  Body is the remaining markdown.

  Strictness:
  - :lenient (external discovery): allow missing name/description, larger limits, best-effort.
  - :strict (vendored/evolution compile): require name & description, forbid unknown tags, small limits.

  Security/fail-closed posture (W/O-S8), in BOTH modes:
  - Key allowlist: only the explicit frontmatter keys in `allowed-keys` are accepted;
    any unknown top-level key is typed-rejected (`:skill/invalid-descriptor`).
  - Tag-event interception: a custom SafeConstructor intercepts every YAML tag and
    only builds plain data (`str/seq/map/int/float/bool/null`); EVERY other tag
    (custom `!foo`, `!!java/*`, `!!binary`, `!!set`, `!!omap`, `!!pairs`, `!!merge`)
    is typed-rejected (`:skill/yaml-invalid`) so arbitrary tag resolution can never
    construct a JVM object/polymorphic instance. Explicit `!!`-prefixed tags are
    additionally banned by a pre-scan guard (defense in depth).
  - Timestamp-as-string: the standard YAML timestamp tag is read as its raw STRING,
    never coerced into a `java.util.Date`/`Temporal` object (no type confusion).

  allowed-tools is preserved raw and also parsed into normalized tokens for visibility hint.
  scripts/ is never execution authority — just files in the RO mount.

  allowed-tools token grammar (W/O-S9): a valid token is a non-empty, non-blank
  identifier composed of letters, digits, and the tool-id namespacing separators
  `. _ / : + -` (no whitespace, comma, or any other punctuation). The grammar is
  judged by this namespace and surfaced through the production parse result:
  - strict mode: any invalid token => typed :skill/invalid-tool-token (fail-closed).
  - lenient mode: invalid tokens => parse continues but a WARN diagnostic is
    surfaced via :warnings / :allowed-tools-invalid (never silent, never fatal)."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err])
  (:import (org.yaml.snakeyaml Yaml LoaderOptions DumperOptions)
           (org.yaml.snakeyaml.constructor SafeConstructor)
           (org.yaml.snakeyaml.representer Representer)
           (org.yaml.snakeyaml.nodes Tag ScalarNode)
           (java.nio.charset StandardCharsets)))

(def ^:private max-lenient-bytes 300000)
(def ^:private max-strict-bytes 200000)
(def ^:private max-lenient-aliases 50)
(def ^:private max-strict-aliases 10)
(def ^:private max-codepoint-lenient 3000000)
(def ^:private max-codepoint-strict 1000000)

(def ^:private allowed-keys
  "Explicit allowlist of SKILL.md frontmatter keys the parser accepts. Any other
  top-level YAML key is rejected fail-closed (typed). :allowed_tools and :tools
  are accepted legacy aliases of :allowed-tools (the parser reads all three)."
  #{:name :description :allowed-tools :allowed_tools :tools})

(def ^:private allowed-tagset
  "Tags the YAML constructor may resolve to plain data. The standard YAML
  timestamp tag is allowed but READ AS A STRING (never a Date/Temporal), so
  date-like scalars never become objects. Every other tag is rejected. This is
  the tag-event allowlist (W/O-S8): no arbitrary tag resolution -> no object or
  polymorphic injection."
  #{Tag/STR Tag/SEQ Tag/MAP Tag/INT Tag/FLOAT Tag/BOOL Tag/NULL Tag/TIMESTAMP})

(defn- timestamp-string-construct
  "A SnakeYAML Construct that reads a timestamp-tagged node as its raw scalar
  STRING instead of a java.util.Date/Temporal. Eliminates type confusion: a YAML
  date/timestamp is never coerced into a JVM temporal object."
  []
  (proxy [org.yaml.snakeyaml.constructor.Construct] []
    (construct [node]
      (if (instance? ScalarNode node)
        (.getValue ^ScalarNode node)
        (throw (err/error :skill/yaml-invalid
                          "timestamp tag on a non-scalar node is not allowed"
                          {:node (str node)}))))
    (construct2ndStep [node data] nil)))

(defn- safe-constructor
  "A SafeConstructor subclass that intercepts every YAML tag event (INV-05):
  only the plain-data tags in allowed-tagset may be constructed. The standard
  YAML timestamp tag is read as a STRING (never a Date/Temporal), and ANY other
  tag is rejected fail-closed so arbitrary tag resolution cannot build objects
  or polymorphic instances (INV-01/INV-09)."
  [opts]
  (proxy [SafeConstructor] [opts]
    (getConstructor [node]
      (let [^Tag tag (.getTag node)]
        (if (contains? allowed-tagset tag)
          (if (= tag Tag/TIMESTAMP)
            (timestamp-string-construct)
            (proxy-super getConstructor node))
          (throw (err/error :skill/yaml-invalid
                            "YAML tag is not allowed (no arbitrary tag resolution)"
                            {:tag (str tag)})))))))

(defn- loader-options
  [mode]
  (let [opts (LoaderOptions.)]
    (if (= mode :strict)
      (do
        (.setMaxAliasesForCollections opts max-strict-aliases)
        (.setCodePointLimit opts max-codepoint-strict)
        (.setNestingDepthLimit opts 50)
        (.setAllowDuplicateKeys opts false)
        (.setAllowRecursiveKeys opts false))
      (do
        (.setMaxAliasesForCollections opts max-lenient-aliases)
        (.setCodePointLimit opts max-codepoint-lenient)
        (.setNestingDepthLimit opts 100)
        (.setAllowDuplicateKeys opts false)
        (.setAllowRecursiveKeys opts false)))
    opts))

(defn- yaml-load
  "Load YAML string via SafeConstructor. Throws :skill/yaml-invalid on failure or forbidden tags."
  [yaml-str mode]
  (when (str/includes? yaml-str "!!")
    ;; forbid explicit tags like !!java, !!js, !!python etc.
    ;; SafeConstructor would already reject, but we give a clear error early.
    ;; Allow only standard YAML tags that SafeConstructor permits; explicit !! is disallowed.
    (throw (err/error :skill/yaml-invalid "YAML must not contain explicit tags (forbid arbitrary JVM objects)" {:yaml (subs yaml-str 0 (min 200 (count yaml-str)))})))
  (when (> (count yaml-str) (if (= mode :strict) max-strict-bytes max-lenient-bytes))
    (throw (err/error :skill/yaml-invalid "YAML frontmatter exceeds size limit" {:size (count yaml-str) :mode mode})))
  (let [opts (loader-options mode)
        constr (safe-constructor opts)
        dopts (DumperOptions.)
        yaml (Yaml. constr (Representer. dopts) dopts opts)]
    (try
      (let [raw (.load yaml yaml-str)
            data (cond
                     (nil? raw) {}
                     (instance? java.util.Map raw) (into {} (map (fn [[k v]] [k (cond
                                                                                    (instance? java.util.Map v) (into {} v)
                                                                                    (instance? java.util.Collection v) (vec v)
                                                                                    :else v)]) raw))
                     (map? raw) raw
                     :else raw)]
        ;; convert nested java collections recursively
        (letfn [(convert [v]
                  (cond
                    (instance? java.util.Map v) (into {} (map (fn [[k vv]] [k (convert vv)]) v))
                    (instance? java.util.Collection v) (vec (map convert v))
                    :else v))]
          (let [converted (convert data)]
            (cond
              (nil? converted) {}
              (map? converted) converted
              :else (throw (err/error :skill/yaml-invalid "YAML frontmatter must be a map" {:data converted}))))))
      (catch org.yaml.snakeyaml.error.YAMLException e
        (throw (err/error :skill/yaml-invalid "YAML parsing failed" {:message (.getMessage e) :mode mode})))
      (catch clojure.lang.ExceptionInfo e (throw e))
      (catch Exception e
        (throw (err/error :skill/yaml-invalid "YAML parsing failed" {:message (.getMessage e) :mode mode}))))))

(defn- check-plain-value
  "Recursively ensure value contains only plain scalars, maps, lists. No custom objects."
  [v]
  (cond
    (nil? v) v
    (string? v) v
    (boolean? v) v
    (number? v) v
    (map? v) (do (doseq [[k val] v]
                   (when-not (or (string? k) (keyword? k) (number? k) (boolean? k))
                     ;; keys must be plain
                     (throw (err/error :skill/yaml-invalid "YAML map keys must be plain scalars" {:key k})))
                   (check-plain-value val))
                 v)
    (sequential? v) (do (doseq [e v] (check-plain-value e)) v)
    (set? v) (do (doseq [e v] (check-plain-value e)) v)
    :else (throw (err/error :skill/yaml-invalid "YAML value must be plain scalar/map/list" {:value v :type (type v)}))))

(defn- check-allowlist
  "Fail-closed allowlist (W/O-S8): reject any frontmatter key outside the explicit
  allowed set. Both :strict and :lenient reject unknown keys so the parser never
  silently accepts new/unknown config surface (INV-01/INV-09)."
  [fm mode]
  (let [unknown (remove allowed-keys (keys fm))]
    (when (seq unknown)
      (throw (err/error :skill/invalid-descriptor
                        "frontmatter contains disallowed key(s)"
                        {:frontmatter fm :keys (vec unknown) :mode mode})))))

(defn- normalize-frontmatter
  "Keywordize keys, validate plain scalars, preserve raw."
  [m]
  (let [kwm (into {} (map (fn [[k v]] [(if (keyword? k) k (keyword (str/trim (str k)))) v]) m))]
    (check-plain-value kwm)
    kwm))

(def ^:private tool-token-pattern
  "Canonical allowed-tools token grammar (W/O-S9). A valid token is a non-empty,
  non-blank identifier: a leading alphanumeric followed by alphanumerics and/or the
  tool-id namespacing separators `. _ / : + -`. This rejects blanks, internal
  whitespace, commas, and any other punctuation/UTF-8 junk a malformed token would
  carry. It is judged by #\"^[A-Za-z0-9][A-Za-z0-9._:/+-]*$\"."
  #"^[A-Za-z0-9][A-Za-z0-9._:/+-]*$")

(defn valid-tool-token?
  "True when `t` is a well-formed allowed-tools tool token per the canonical grammar
  (see tool-token-pattern). A nil/blank/whitespace/comma-heavy or otherwise malformed
  token is NOT valid. Single implementation of the grammar (INV-05) — the
  parse path calls this and nothing else re-derives the grammar."
  [t]
  (and (string? t)
       (not (str/blank? t))
       (boolean (re-matches tool-token-pattern t))))

(defn- parse-allowed-tools
  "Parse allowed-tools / tools value. Returns {:raw original :parsed [...] :normalized [...]
   :known [...] :invalid [...] :valid? bool} or nil when absent.
   Does not mint leases; only a visibility hint. Classifies each parsed token by the
   canonical grammar (valid-tool-token?) so strict can fail-closed and lenient can
   surface an invalid-token diagnostic."
  [fm]
  (let [raw (or (:allowed-tools fm) (:allowed_tools fm) (:tools fm))]
    (when raw
      (let [tokens (cond
                     (string? raw)
                     ;; comma or space separated
                     (let [s (str/trim raw)]
                       (if (str/blank? s) []
                           (->> (str/split s #"[,\s]+")
                                (remove str/blank?)
                                vec)))
                     (sequential? raw)
                     (vec (map str raw))
                     (set? raw)
                     (vec (map str raw))
                     :else [(str raw)])
            known #{"Read" "Write" "Edit" "Bash" "Grep" "Glob" "Skill" "Agent" "AskUserQuestion" "TodoWrite" "WebFetch" "WebSearch" "Task" "NotebookEdit" "ExitPlanMode"}
            invalid (vec (remove valid-tool-token? tokens))
            valid? (empty? invalid)
            normalized (mapv (fn [t]
                               (let [lower (str/lower-case t)]
                                 (keyword lower)))
                             tokens)]
        {:raw raw
         :parsed tokens
         :normalized normalized
         :known known
         :invalid invalid
         :valid? valid?}))))

(defn extract-frontmatter
  "Split SKILL.md content into {:frontmatter-yaml (or nil) :body string :had-frontmatter? bool}
   Frontmatter is `---` on first line to next `---` line. Body is remainder."
  [content]
  (let [s (str content)
        lines (str/split-lines s)]
    (if (and (seq lines) (= (str/trim (first lines)) "---"))
      (let [rest-lines (rest lines)
            idx (first (keep-indexed (fn [i l] (when (= (str/trim l) "---") i)) rest-lines))]
        (if (nil? idx)
          {:frontmatter-yaml nil :body s :had-frontmatter? false}
          (let [yaml-lines (take idx rest-lines)
                body-lines (drop (inc idx) rest-lines)
                yaml-str (str/join "\n" yaml-lines)
                body (str/join "\n" body-lines)]
            {:frontmatter-yaml yaml-str :body body :had-frontmatter? true})))
      {:frontmatter-yaml nil :body s :had-frontmatter? false})))

(defn parse-skill-content
  "Parse full SKILL.md string.

   mode :lenient or :strict
   lenient: missing name/description allowed, larger limits, best-effort.
   strict: requires name & description, forbid oversized, forbid explicit tags, etc.

   In BOTH modes a fail-closed frontmatter key allowlist and YAML tag-event
   interception apply (see ns docstring): unknown frontmatter keys are typed-rejected
   (`:skill/invalid-descriptor`) and unknown/unsafe YAML tags are typed-rejected
   (`:skill/yaml-invalid`). Date/timestamp scalars are read as STRINGS, never Date/Temporal.

   Returns {:frontmatter {...keywordized...} :body string :raw string :allowed-tools ...}
   Throws :skill/yaml-invalid or :skill/invalid-descriptor on strict violations."
  ([content] (parse-skill-content content :lenient))
  ([content mode]
   (when-not (string? content)
     (throw (err/error :skill/invalid-descriptor "SKILL.md content must be string" {:content content})))
   (let [{:keys [frontmatter-yaml body had-frontmatter?]} (extract-frontmatter content)
         fm (if had-frontmatter?
              (let [raw-map (yaml-load frontmatter-yaml mode)
                    kwm (normalize-frontmatter raw-map)]
                (check-allowlist kwm mode)
                kwm)
              {})]
     ;; validation
     (when (= mode :strict)
       (when-not (and (contains? fm :name) (string? (:name fm)) (not (str/blank? (:name fm))))
         (throw (err/error :skill/invalid-descriptor "strict SKILL.md requires non-empty :name" {:frontmatter fm})))
       (when-not (and (contains? fm :description) (string? (:description fm)) (not (str/blank? (:description fm))))
         (throw (err/error :skill/invalid-descriptor "strict SKILL.md requires non-empty :description" {:frontmatter fm}))))
     ;; allowed-tools handling
     (let [at (parse-allowed-tools fm)
           fm-with-at (if at (assoc fm :allowed-tools (:raw at) :allowed-tools-parsed at) fm)
           invalid (when at (:invalid at))
           _ (when (and (= mode :strict) at (seq invalid))
               (throw (err/error :skill/invalid-tool-token
                                 "allowed-tools contains invalid tool token(s)"
                                 {:invalid invalid :allowed-tools (:raw at) :mode mode})))
           warnings (when (and (not= mode :strict) at (seq invalid))
                      [{:type :skill/invalid-tool-token
                        :tokens invalid
                        :allowed-tools (:raw at)
                        :mode mode}])]
       {:frontmatter fm-with-at
        :body (str/trim body)
        :raw content
        :had-frontmatter? had-frontmatter?
        :allowed-tools at
        ;; stable diagnostic shape: absent/valid => empty vectors, never nil.
        :allowed-tools-invalid (vec (or invalid []))
        :warnings (vec (or warnings []))
        :mode mode}))))

(defn validate-skill-metadata
  "Validate parsed frontmatter for required fields. Throws :skill/invalid-descriptor if invalid.
   For lenient, only checks types; for strict, requires name+description."
  ([parsed] (validate-skill-metadata parsed :lenient))
  ([parsed mode]
   (let [fm (:frontmatter parsed)]
     (when (= mode :strict)
       (when-not (and (:name fm) (string? (:name fm)) (not (str/blank? (:name fm))))
         (throw (err/error :skill/invalid-descriptor "strict requires :name" {:frontmatter fm})))
       (when-not (and (:description fm) (string? (:description fm)) (not (str/blank? (:description fm))))
         (throw (err/error :skill/invalid-descriptor "strict requires :description" {:frontmatter fm}))))
     ;; type checks for lenient as well
     (when (and (:name fm) (not (string? (:name fm))))
       (throw (err/error :skill/invalid-descriptor ":name must be string" {:frontmatter fm})))
     (when (and (:description fm) (not (string? (:description fm))))
       (throw (err/error :skill/invalid-descriptor ":description must be string" {:frontmatter fm})))
     parsed)))

(defn skill-name-from
  "Derive skill name from frontmatter or directory name."
  [frontmatter dir-name]
  (or (:name frontmatter)
      (when (string? dir-name) dir-name)
      (throw (err/error :skill/invalid-descriptor "skill name not found in frontmatter nor directory" {:frontmatter frontmatter}))))

