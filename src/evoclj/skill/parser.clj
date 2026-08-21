(ns evoclj.skill.parser
  "SKILL.md parser with YAML frontmatter strictness.

  Frontmatter is `---` delimited YAML at the top of SKILL.md.
  Body is the remaining markdown.

  Strictness:
  - :lenient (external discovery): allow missing name/description, larger limits, best-effort.
  - :strict (vendored/evolution compile): require name & description, forbid unknown tags, small limits.

  YAML loading uses SnakeYAML SafeConstructor only (plain scalars/maps/lists).
  Forbidden: arbitrary JVM object tags (!!java, !!js, custom !!), aliases beyond limit, oversized docs.

  allowed-tools is preserved raw and also parsed into normalized tokens for visibility hint.
  scripts/ is never execution authority — just files in the RO mount."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err])
  (:import (org.yaml.snakeyaml Yaml LoaderOptions DumperOptions)
           (org.yaml.snakeyaml.constructor SafeConstructor)
           (java.nio.charset StandardCharsets)))

(def ^:private max-lenient-bytes 300000)
(def ^:private max-strict-bytes 200000)
(def ^:private max-lenient-aliases 50)
(def ^:private max-strict-aliases 10)
(def ^:private max-codepoint-lenient 3000000)
(def ^:private max-codepoint-strict 1000000)

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
        constr (SafeConstructor. opts)
        yaml (Yaml. constr)]
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

(defn- normalize-frontmatter
  "Keywordize keys, validate plain scalars, preserve raw."
  [m]
  (let [kwm (into {} (map (fn [[k v]] [(if (keyword? k) k (keyword (str/trim (str k)))) v]) m))]
    (check-plain-value kwm)
    kwm))

(defn- parse-allowed-tools
  "Parse allowed-tools / tools value. Returns {:raw original :parsed [...] :normalized [...]}
   or nil when absent. Does not mint leases; only a visibility hint."
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
            normalized (mapv (fn [t]
                               (let [lower (str/lower-case t)]
                                 (keyword lower)))
                             tokens)]
        {:raw raw :parsed tokens :normalized normalized :known known}))))

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
   lenient: missing name/description allowed, unknown keys tolerated, larger limits.
   strict: requires name & description, forbid oversized, forbid explicit tags, etc.

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
           fm-with-at (if at (assoc fm :allowed-tools (:raw at) :allowed-tools-parsed at) fm)]
       {:frontmatter fm-with-at
        :body (str/trim body)
        :raw content
        :had-frontmatter? had-frontmatter?
        :allowed-tools at
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

