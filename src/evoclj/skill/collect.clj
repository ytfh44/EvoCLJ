(ns evoclj.skill.collect
  "S12 — deterministic same-name skill resolution across scopes.

  A SkillSource discovers skills from multiple roots belonging to distinct
  scopes:

    :project — the project's own skills (<project>/.agents/skills)
    :user    — the user's shared skills (~/.agents/skills)
    :extra   — caller-supplied extra roots

  When the SAME skill name is discoverable from more than one scope, the
  standard behavior resolves it by a FIXED precedence order

      :project > :user > :extra

  — never by registration order and never by an arbitrary tie-break. A caller
  that opts into the fail-closed mode `:on-collision :error` instead gets a
  TYPED :skill/name-collision error raised on any cross-scope same-name
  collision, and the snapshot fails closed (no winner is silently picked).

  These functions are pure and are called by the production SkillSource
  snapshot path (evoclj.skill.adapter); the end-to-end collision behavior is
  observable through the real snapshot + catalog projection."
  (:require [evoclj.kernel.error :as err]))

(def precedence-order
  "Fixed skill-scope precedence, highest to lowest (project wins)."
  [:project :user :extra])

(def valid-scopes
  "The set of recognized skill scopes."
  #{:project :user :extra})

(def ^:private scope-rank
  "Scope -> numeric rank derived from precedence-order (single source of truth).
   Earlier = higher: project is the highest, extra the lowest. An unrecognized
   scope maps to 0 so a malformed entry can never shadow a real precedence."
  (zipmap precedence-order (range (count precedence-order) 0 -1)))

(defn- precedence-rank
  "Numeric rank of a scope (project highest, extra lowest); unknown = 0."
  [scope]
  (get scope-rank scope 0))

(defn precedence-winner
  "Pick the higher-precedence of two same-name entries.

   Cross-scope: the scope with the higher fixed precedence wins. A same-scope
   tie (two roots of the same scope sharing a name) is broken deterministically
   by the greatest :tree/id — a stable content key — so two identical skills
   collapse to the same entry and two different-content skills of the same scope
   resolve by hash, never by registration order."
  [e1 e2]
  (let [r1 (precedence-rank (:scope e1))
        r2 (precedence-rank (:scope e2))]
    (cond
      (> r1 r2) e1
      (< r1 r2) e2
      :else (if (>= (compare (str (:tree/id e1)) (str (:tree/id e2))) 0) e1 e2))))

(defn colliding-names
  "Sorted vector of skill names present in two or more DISTINCT scopes. These
   are exactly the names the precedence resolution would otherwise have to
   silently pick a winner for."
  [entries]
  (->> (group-by :skill/name entries)
       (keep (fn [[name group]]
               (when (> (count (set (map :scope group))) 1)
                 name)))
       sort
       vec))

(defn- collision-data
  "Serializable payload describing every cross-scope collision."
  [entries colls]
  {:skill/name (first colls)
   :collisions colls
   :collision-mode :error
   :scopes (into (sorted-map)
                 (map (fn [n]
                        [n (vec (sort (set (map :scope
                                                (filter #(= n (:skill/name %))
                                                        entries)))))])
                      colls))})

(defn resolve-skills
  "Collapse scoped skill discoveries into a single map skill-name -> winning
   entry, choosing by fixed precedence (project > user > extra).

   `entries` is a seq of maps of the form
     {:skill/name <string> :scope <:project|:user|:extra> :tree/id <sha256>
      :frontmatter <map> :body <string>}
   produced by the per-skill capture+parse path of the SkillSource snapshot.

   `collision-mode`:
     :precedence (default) — cross-scope same-name resolves to the
       highest-precedence scope; same-scope duplicates tie-break by tree/id.
     :error — any cross-scope same-name raises a typed :skill/name-collision
       (fail-closed) instead of picking a winner.

   Returns {} for an empty `entries` (no skills)."
  [entries collision-mode]
  (let [colls (colliding-names entries)]
    (when (and (= :error collision-mode) (seq colls))
      (throw (err/error :skill/name-collision
                        "same-name skill collision across scopes"
                        (collision-data entries colls))))
    (reduce (fn [acc e]
              (let [n (:skill/name e)]
                (assoc acc n (if-let [cur (get acc n)]
                               (precedence-winner cur e)
                               e))))
            {}
            entries)))

(defn validate-collision-mode
  "Validate an :on-collision value. Returns a normalized keyword:
   nil/:precedence -> :precedence, :error -> :error. Anything else throws a
   typed :skill/invalid-collision-mode (fail-closed)."
  [mode]
  (case mode
    (nil :precedence) :precedence
    :error :error
    (throw (err/error :skill/invalid-collision-mode
                      "collision mode must be :precedence or :error"
                      {:on-collision mode}))))

(defn validate-root-scopes
  "Validate a :root-scopes map (scope keyword -> vector of root paths).
   Returns the map unchanged on success; an unrecognized scope keyword throws
   a typed :skill/invalid-root-scope (fail-closed)."
  [root-scopes]
  (doseq [[scope _] root-scopes]
    (when-not (contains? valid-scopes scope)
      (throw (err/error :skill/invalid-root-scope
                        (str "unknown skill scope: " scope)
                        {:scope scope
                         :valid-scopes (vec (sort valid-scopes))
                         :root-scopes root-scopes}))))
  root-scopes)
