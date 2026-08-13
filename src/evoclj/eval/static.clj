(ns evoclj.eval.static
  "Kernel-side registry of deterministic evaluation suites (Task 8.2).

  G3 invokes registered deterministic unit/property suites against a
  candidate's loaded data in a fresh workspace. The registry lives
  HERE — in kernel source, never in a Genome — so it is NOT
  genome-mutable (Global Constraints 19, 24): a candidate can never
  register, replace, or disable the suites that judge it, because
  nothing in the candidate's bundle is consulted to build the suite
  list.

  A suite is a closed map:

      {:suite/id   <keyword>           ; stable identifier
       :suite/type :unit | :property   ; unit or property suite
       :check      (fn [candidate] ...)} ; nil => pass, else failure map

  :check receives the G3 candidate context — a plain map
  {:candidate/loaded <loaded Genome map> :candidate/root <path string>
   :workspace {:workspace/root <fresh temp path> ...}} — and MUST be
  deterministic and free of ambient authority (no network, no secrets;
  Global Constraint 7). It returns nil (pass) or a serializable
  failure map. A throwing suite is recorded as a per-suite :error by
  the gate, never a gate :error.

  Error contract (Global Constraint 22): :eval/suite-invalid
  (:reason distinguishes :not-a-map, :missing-key, :bad-suite-id,
  :bad-suite-type, :bad-check, :duplicate-id)."
  (:require [evoclj.kernel.error :as err]))

(def ^:private suite-types
  "The two supported deterministic suite kinds."
  #{:unit :property})

(defn- check-suite-shape!
  [suite]
  (when-not (map? suite)
    (throw (err/error :eval/suite-invalid
                      "a suite must be a map"
                      {:reason :not-a-map :value (err/sanitize suite)})))
  (doseq [[k present?] [[:suite/id (contains? suite :suite/id)]
                        [:suite/type (contains? suite :suite/type)]
                        [:check (contains? suite :check)]]]
    (when-not present?
      (throw (err/error :eval/suite-invalid
                        "suite is missing a required key"
                        {:reason :missing-key :key k
                         :value (err/sanitize suite)}))))
  (when-not (keyword? (:suite/id suite))
    (throw (err/error :eval/suite-invalid
                      ":suite/id must be a keyword"
                      {:reason :bad-suite-id
                       :value (err/sanitize (:suite/id suite))})))
  (when-not (contains? suite-types (:suite/type suite))
    (throw (err/error :eval/suite-invalid
                      ":suite/type must be :unit or :property"
                      {:reason :bad-suite-type
                       :value (err/sanitize (:suite/type suite))})))
  (when-not (fn? (:check suite))
    (throw (err/error :eval/suite-invalid
                      ":check must be a deterministic function"
                      {:reason :bad-check
                       :value (err/sanitize (:check suite))}))))

(def ^:private suites
  "The kernel-side suite registry. Lives in kernel source only; it is
  never loaded from, written to, or mutated by any Genome."
  (atom []))

(defn register-suite!
  "Register one deterministic suite in the kernel-side registry.

  The suite must satisfy the closed suite contract (see the namespace
  docstring); duplicate :suite/id values are rejected so the registry
  stays deterministic. Returns the suite unchanged.

  Throws :eval/suite-invalid on any contract violation."
  [suite]
  (check-suite-shape! suite)
  (when (some #(= (:suite/id suite) (:suite/id %)) @suites)
    (throw (err/error :eval/suite-invalid
                      "a suite with this id is already registered"
                      {:reason :duplicate-id :suite/id (:suite/id suite)})))
  (swap! suites conj suite)
  suite)

(defn registered-suites
  "The currently registered suites as a vector (deterministic
  registration order)."
  []
  @suites)

(defn clear-suites!
  "Remove every registered suite. Kernel/operator tooling (and the
  Task 8.2 tests) use this to reset the registry between runs."
  []
  (reset! suites [])
  nil)
