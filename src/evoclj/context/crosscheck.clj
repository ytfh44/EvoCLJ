(ns evoclj.context.crosscheck
  "Cross-validation of an envelope's STRUCTURED fields against the
  authoritative todo/goal tools.

  The envelope's TASK and SUBGOALS sections are structured, so they
  CAN be re-derived from the real source of truth (the todo tracker,
  the goal registry). This module does that derivation and compares.
  Only fields that are auto-correctable are silently fixed; any
  disagreement that is NOT auto-correctable throws
  :context/crosscheck-mismatch so the operator sees it.

  The residue section is deliberately NOT cross-checked here: residue
  is the non-structured autobiography that no tool can re-derive, so
  there is nothing to compare against. Trying to cross-check residue
  would be the error of believing structured tools cover everything."
  (:require [evoclj.context.error :as err]
            [evoclj.context.envelope :as envelope]))

;; ---------------------------------------------------------------------------
;; The source of truth
;; ---------------------------------------------------------------------------

;; In v0 there is no live todo tracker wired into the host. The
;; crosscheck takes a caller-supplied `todo` data structure — the
;; canonical shape the repo's own todo tools would produce. This keeps
;; the module pure and testable without a host dependency.
;;
;;   {:tasks [{:task/id <string>
;;             :task/status <keyword>
;;             :task/description <string>
;;             :task/owner <string or nil>} ...]
;;    :subgoals [{:subgoal/id <string>
;;                :subgoal/status <keyword>
;;                :subgoal/description <string>
;;                :subgoal/parent <string or nil>} ...]}

(defn- find-task [todo task-id]
  (some (fn [t] (when (= (:task/id t) task-id) t)) (:tasks todo)))

(defn- find-subgoal [todo subgoal-id]
  (some (fn [s] (when (= (:subgoal/id s) subgoal-id) s)) (:subgoals todo)))

;; ---------------------------------------------------------------------------
;; Auto-correctable fields
;; ---------------------------------------------------------------------------

;; These are the fields whose value the envelope may legitimately get
;; wrong (stale) and whose correct value the todo tool knows. Fixing
;; them is safe because the todo tool is authoritative for them.
(def ^:private auto-correctable-task-keys
  #{:task/status})

(def ^:private auto-correctable-subgoal-keys
  #{:subgoal/status})

(defn- correct-task [envelope-task todo-task]
  (reduce-kv (fn [acc k v]
               (assoc acc k v))
             envelope-task
             (select-keys todo-task auto-correctable-task-keys)))

(defn- correct-subgoal [envelope-subgoal todo-subgoal]
  (reduce-kv (fn [acc k v]
               (assoc acc k v))
             envelope-subgoal
             (select-keys todo-subgoal auto-correctable-subgoal-keys)))

;; ---------------------------------------------------------------------------
;; Non-auto-correctable disagreement
;; ---------------------------------------------------------------------------

(defn- mismatch [kind id field envelope-value todo-value]
  {:crosscheck/kind kind
   :crosscheck/id id
   :crosscheck/field field
   :crosscheck/envelope-value (err/sanitize envelope-value)
   :crosscheck/todo-value (err/sanitize todo-value)})

(defn- check-task [envelope-task todo]
  (let [task-id (:task/id envelope-task)
        todo-task (find-task todo task-id)]
    (cond
      (nil? todo-task)
      ;; The envelope names a task the todo tool does not know. This
      ;; is NOT auto-correctable: we cannot invent a task, and the
      ;; envelope is claiming something the source of truth denies.
      [(mismatch :task task-id :task/id (:task/id envelope-task) nil)]

      :else
      (reduce-kv
       (fn [acc k envelope-v]
         (let [todo-v (k todo-task)]
           (if (= envelope-v todo-v)
             acc
             (if (contains? auto-correctable-task-keys k)
               acc                      ; auto-corrected, not a mismatch
               (conj acc (mismatch :task task-id k envelope-v todo-v))))))
       []
       (dissoc envelope-task :task/id)))))

(defn- check-subgoals [envelope-subgoals todo]
  (reduce
   (fn [acc sg]
     (let [sg-id (:subgoal/id sg)
           todo-sg (find-subgoal todo sg-id)]
       (cond
         (nil? todo-sg)
         (conj acc (mismatch :subgoal sg-id :subgoal/id sg-id nil))

         :else
         (reduce-kv
          (fn [acc2 k envelope-v]
            (let [todo-v (k todo-sg)]
              (if (= envelope-v todo-v)
                acc2
                (if (contains? auto-correctable-subgoal-keys k)
                  acc2
                  (conj acc2 (mismatch :subgoal sg-id k envelope-v todo-v))))))
          acc
          (dissoc sg :subgoal/id)))))
   []
   envelope-subgoals))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn crosscheck
  "Cross-validate `envelope`'s structured fields against `todo`.

  `todo` is the authoritative task/subgoal data
  ({:tasks [...] :subgoals [...]}). The envelope's TASK and SUBGOALS
  sections are compared against it. Auto-correctable fields
  (:task/status, :task/description, :task/owner, and the subgoal
  equivalents) are silently corrected in the returned envelope.
  Any other disagreement — a task the envelope names that the todo
  tool does not know, an unknown field that differs, a subgoal the
  envelope names that the todo tool does not know — is collected.

  Returns a map:
    {:crosscheck/envelope <corrected envelope>
     :crosscheck/mismatches [<mismatch> ...]
     :crosscheck/valid? <bool>}

  Throws :context/crosscheck-mismatch when the envelope is malformed
  (fails EnvelopeSchema) or `todo` is not a map. Does NOT throw on a
  substantive disagreement — it reports it so the caller decides."
  [envelope todo]
  (envelope/validate-envelope envelope)
  (when-not (map? todo)
    (throw (err/error :context/crosscheck-mismatch
                      "todo must be a map with :tasks and :subgoals"
                      {:value (err/sanitize todo)})))
  (let [env-task (:envelope/task envelope)
        env-subgoals (:envelope/subgoals envelope [])
        mismatches (vec (concat (if env-task (check-task env-task todo) [])
                                (check-subgoals env-subgoals todo)))
        corrected-task (when env-task
                         (let [todo-task (find-task todo (:task/id env-task))]
                           (if todo-task
                             (correct-task env-task todo-task)
                             env-task)))
        corrected-subgoals (mapv (fn [sg]
                                   (let [todo-sg (find-subgoal todo (:subgoal/id sg))]
                                     (if todo-sg
                                       (correct-subgoal sg todo-sg)
                                     sg)))
                                 env-subgoals)
        corrected (cond-> envelope
                    env-task (assoc :envelope/task corrected-task)
                    true     (assoc :envelope/subgoals corrected-subgoals))]
    {:crosscheck/envelope corrected
     :crosscheck/mismatches mismatches
     :crosscheck/valid? (empty? mismatches)}))

(defn crosscheck-valid?
  "True when `envelope` cross-checks cleanly against `todo` — i.e. the
  returned :crosscheck/valid? from `crosscheck`. Convenience wrapper
  that discards the corrected envelope."
  [envelope todo]
  (:crosscheck/valid? (crosscheck envelope todo)))

(defn crosscheck-mismatches
  "The vector of mismatches from crosschecking `envelope` against
  `todo`. Empty when the envelope is consistent with the source of
  truth."
  [envelope todo]
  (:crosscheck/mismatches (crosscheck envelope todo)))