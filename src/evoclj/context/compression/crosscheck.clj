(ns evoclj.context.compression.crosscheck
  "Cross-validation of an envelope's STRUCTURED fields against a
   caller-supplied source of truth.

   The envelope's TASK and SUBGOALS sections are structured, so they
   CAN be re-derived from registered structured-field providers (todo
   trackers, goal registries, etc.). This module does that derivation
   and compares.

   Only fields that are auto-correctable are silently fixed; any
   disagreement that is NOT auto-correctable throws
   :context/crosscheck-mismatch so the operator sees it.

   The residue section is deliberately NOT cross-checked here: residue
   is the non-structured autobiography that no tool can re-derive, so
   there is nothing to compare against. Trying to cross-check residue
   would be the error of believing structured tools cover everything.

   In v0 there is no live structured-field provider wired into the
   host. The crosscheck takes a caller-supplied `structured-sections`
   data structure — the canonical shape any registered provider would
   produce. This keeps the module pure and testable without a host
   dependency.

   Backward compatibility: the old `crosscheck` signature
   `[envelope todo]` is retained as a deprecated wrapper. New code
   should call `crosscheck*` with `[envelope structured-sections]`.

   `structured-sections` shape:
     {:tasks [{:task/id <string>
               :task/status <keyword>
               :task/description <string>
               :task/owner <string or nil>} ...]
      :subgoals [{:subgoal/id <string>
                  :subgoal/status <keyword>
                  :subgoal/description <string>
                  :subgoal/parent <string or nil>} ...]}"
  (:require [evoclj.context.compression.error :as err]
            [evoclj.context.compression.envelope :as envelope]))

;; ---------------------------------------------------------------------------
;; Private helpers
;; ---------------------------------------------------------------------------

(defn- find-task [sections task-id]
  (some (fn [t] (when (= (:task/id t) task-id) t)) (:tasks sections)))

(defn- find-subgoal [sections subgoal-id]
  (some (fn [s] (when (= (:subgoal/id s) subgoal-id) s)) (:subgoals sections)))

(def ^:private auto-correctable-task-keys
  #{:task/status})

(def ^:private auto-correctable-subgoal-keys
  #{:subgoal/status})

(defn- correct-task [envelope-task source-task]
  (reduce-kv (fn [acc k v]
               (assoc acc k v))
             envelope-task
             (select-keys source-task auto-correctable-task-keys)))

(defn- correct-subgoal [envelope-subgoal source-subgoal]
  (reduce-kv (fn [acc k v]
               (assoc acc k v))
             envelope-subgoal
             (select-keys source-subgoal auto-correctable-subgoal-keys)))

(defn- mismatch [kind id field envelope-value source-value]
  {:crosscheck/kind kind
   :crosscheck/id id
   :crosscheck/field field
   :crosscheck/envelope-value (err/sanitize envelope-value)
   :crosscheck/source-value (err/sanitize source-value)})

(defn- check-task [envelope-task sections]
  (let [task-id (:task/id envelope-task)
        source-task (find-task sections task-id)]
    (cond
      (nil? source-task)
      ;; The envelope names a task the source of truth does not know.
      ;; This is NOT auto-correctable: we cannot invent a task.
      [(mismatch :task task-id :task/id (:task/id envelope-task) nil)]

      :else
      (reduce-kv
       (fn [acc k envelope-v]
         (let [source-v (k source-task)]
           (if (= envelope-v source-v)
             acc
             (if (contains? auto-correctable-task-keys k)
               acc                      ; auto-corrected, not a mismatch
               (conj acc (mismatch :task task-id k envelope-v source-v))))))
       []
       (dissoc envelope-task :task/id)))))

(defn- check-subgoals [envelope-subgoals sections]
  (reduce
   (fn [acc sg]
     (let [sg-id (:subgoal/id sg)
           source-sg (find-subgoal sections sg-id)]
       (cond
         (nil? source-sg)
         (conj acc (mismatch :subgoal sg-id :subgoal/id sg-id nil))

         :else
         (reduce-kv
          (fn [acc2 k envelope-v]
            (let [source-v (k source-sg)]
              (if (= envelope-v source-v)
                acc2
                (if (contains? auto-correctable-subgoal-keys k)
                  acc2
                  (conj acc2 (mismatch :subgoal sg-id k envelope-v source-v))))))
          acc
          (dissoc sg :subgoal/id)))))

   []
   envelope-subgoals))

;; ---------------------------------------------------------------------------
;; Core implementation (generic, no tool names)
;; ---------------------------------------------------------------------------

(defn crosscheck*
  "Cross-validate `envelope`'s structured fields against `structured-sections`.

   `structured-sections` is the authoritative task/subgoal data
   ({:tasks [...] :subgoals [...]}). The envelope's TASK and SUBGOALS
   sections are compared against it. Auto-correctable fields
   (:task/status, :subgoal/status) are silently corrected in the
   returned envelope. Any other disagreement is collected.

   Returns a map:
     {:crosscheck/envelope <corrected envelope>
      :crosscheck/mismatches [<mismatch> ...]
      :crosscheck/valid? <bool>}

   Throws :context/crosscheck-mismatch when the envelope is malformed
   (fails EnvelopeSchema) or `structured-sections` is not a map. Does
   NOT throw on a substantive disagreement — it reports it so the
   caller decides."
  [envelope structured-sections]
  (envelope/validate-envelope envelope)
  (when-not (map? structured-sections)
    (throw (err/error :context/crosscheck-mismatch
                      "structured-sections must be a map with :tasks and :subgoals"
                      {:value (err/sanitize structured-sections)})))
  (let [env-task (:envelope/task envelope)
        env-subgoals (:envelope/subgoals envelope [])
        mismatches (vec (concat (if env-task (check-task env-task structured-sections) [])
                                (check-subgoals env-subgoals structured-sections)))
        corrected-task (when env-task
                         (let [source-task (find-task structured-sections (:task/id env-task))]
                           (if source-task
                             (correct-task env-task source-task)
                             env-task)))
        corrected-subgoals (mapv (fn [sg]
                                   (let [source-sg (find-subgoal structured-sections (:subgoal/id sg))]
                                     (if source-sg
                                       (correct-subgoal sg source-sg)
                                       sg)))
                                 env-subgoals)
        corrected (cond-> envelope
                    env-task (assoc :envelope/task corrected-task)
                    true     (assoc :envelope/subgoals corrected-subgoals))]
    {:crosscheck/envelope corrected
     :crosscheck/mismatches mismatches
     :crosscheck/valid? (empty? mismatches)}))

;; ---------------------------------------------------------------------------
;; Backward-compatible deprecated wrappers
;; ---------------------------------------------------------------------------

(defn crosscheck
  "Cross-validate `envelope`'s structured fields against `structured-sections`.

   DEPRECATED: use `crosscheck*` instead. This wrapper is retained for
   backward compatibility and will be removed in a future version.

   `structured-sections` should be a map with :tasks and :subgoals
   keys (the shape produced by todo/goal tools).

   Returns the same map as `crosscheck*`."
  [envelope structured-sections]
  (crosscheck* envelope structured-sections))

(defn crosscheck-valid?
  "True when `envelope` cross-checks cleanly against `structured-sections`.
   Convenience wrapper that discards the corrected envelope."
  [envelope structured-sections]
  (:crosscheck/valid? (crosscheck* envelope structured-sections)))

(defn crosscheck-mismatches
  "The vector of mismatches from crosschecking `envelope` against
   `structured-sections`. Empty when the envelope is consistent with
   the source of truth."
  [envelope structured-sections]
  (:crosscheck/mismatches (crosscheck* envelope structured-sections)))
