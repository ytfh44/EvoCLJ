(ns evoclj.context.compression.archivers
  "Concrete archiver implementations for the context-compression subsystem.

   Archivers implement `CompacterArchive` and are registered with
   `registry/register!`. During compaction, the registry collects their
   manifests and includes them in the footer text.")

;; ---------------------------------------------------------------------------
;; Protocol import
;; ---------------------------------------------------------------------------

(require '[evoclj.context.compression.registry :as registry])

;; ---------------------------------------------------------------------------
;; Simple state holder
;; ---------------------------------------------------------------------------

(defrecord SimpleState [id description state])

;; ---------------------------------------------------------------------------
;; Archiver implementations
;; ---------------------------------------------------------------------------

(defn make-simple-archiver
  "Create a simple archiver that records an arbitrary EDN-safe state map.

   `id`       — keyword, unique archiver identifier.
   `desc`     — string, human-readable description.
   `state-map` — EDN-safe map summarizing what was archived.

   Returns a record satisfying `CompacterArchive`."
  [id desc state-map]
  (->SimpleState id desc state-map))

(extend-protocol registry/CompacterArchive
  SimpleState
  (archive-manifest [this]
    {:archiver/id (:id this)
     :archiver/description (:description this)
     :archiver/serialized (:state this)}))

;; ---------------------------------------------------------------------------
;; Built-in archivers
;; ---------------------------------------------------------------------------

(defn todo-archiver
  "Return an archiver that records a todo-list snapshot.

   `todos` should be a vector of maps with at least:
     {:todo/id <keyword>
      :todo/status <keyword>
      :todo/description <string>}

   The serialized form is a simplified summary suitable for the footer."
  [todos]
  (->SimpleState
    :archiver/todo-list
    "Todo list snapshot"
    {:count (count todos)
     :items (mapv (fn [t]
                    {:id (:todo/id t)
                     :status (:todo/status t)
                     :description (:todo/description t)})
                  todos)}))

(defn goal-archiver
  "Return an archiver that records a goal-state snapshot.

   `goals` should be a vector of maps with at least:
     {:goal/id <string>
      :goal/status <keyword>
      :goal/objective <string>}

   The serialized form is a simplified summary suitable for the footer."
  [goals]
  (->SimpleState
    :archiver/goal-registry
    "Goal registry snapshot"
    {:count (count goals)
     :items (mapv (fn [g]
                    {:id (:goal/id g)
                     :status (:goal/status g)
                     :objective (:goal/objective g)})
                  goals)}))

(defn capability-archiver
  "Return an archiver that records available capabilities.

   `caps` should be a collection of capability keywords or strings.

   The serialized form is a simplified summary suitable for the footer."
  [caps]
  (->SimpleState
    :archiver/capabilities
    "Available capabilities snapshot"
    {:count (count caps)
     :items (vec caps)}))
