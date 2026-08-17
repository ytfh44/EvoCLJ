(ns evoclj.context.footer
  "Footer text generation for the context-compression subsystem.

   The footer is a special instruction block inserted at the end of the
   compressed context. It tells the next agent turn: what was done,
   what remains, and what load-bearing constraints must be honored.

   The footer is NOT stored in the envelope. It is generated fresh at
   compression time from:
   1. The envelope's structured fields (task, subgoals, residue, evidence)
   2. The archiver reports from the registry (tools that registered
      themselves via `CompacterArchive`)

   This keeps the envelope small and the footer expressive."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- task-summary [task]
  (when task
    (let [id (:task/id task)
          status (:task/status task)
          desc (:task/description task)]
      (str id " [" status "] " (or desc "")))))

(defn- subgoal-summary [sg]
  (str (:subgoal/id sg) " [" (:subgoal/status sg) "] "
       (or (:subgoal/description sg) "")))

(defn- residue-summary [r]
  (str "  [" (:residue/kind r) "] " (:residue/text r)))

;; ---------------------------------------------------------------------------
;; Core footer builder
;; ---------------------------------------------------------------------------

(defn build-footer
  "Build the footer text for a compressed context.

   `envelope` is the compression envelope map.
   `opts` is an optional map with:
     :archiver-reports — vector of archiver manifest maps (from registry/archiver-reports)
     :max-residue     — max residue entries to include (default 20)

   Returns a string suitable for appending to the compressed context."
  ([envelope]
   (build-footer envelope nil))
  ([envelope opts]
   {:pre [(map? envelope)]}
   (let [opts (or opts {})
         archiver-reports (or (:archiver-reports opts) [])
         max-residue (or (:max-residue opts) 20)
         task (:envelope/task envelope)
         subgoals (:envelope/subgoals envelope [])
         residues (:envelope/residue envelope [])
         evidence (:envelope/evidence envelope [])
         base-parts (-> []
                        (conj "[CONTEXT COMPRESSION]")
                        (conj "You are continuing a compressed session.")
                        (conj (str "Envelope version: " (:envelope/version envelope)))
                        (conj "")
                        (conj "--- Task ---")
                        (conj (or (task-summary task) "(none)"))
                        (conj "")
                        (conj "--- Subgoals ---")
                        (conj (if (seq subgoals)
                                (str/join "\n" (map subgoal-summary subgoals))
                                "(none)"))
                        (conj "")
                        (conj "--- Residue (load-bearing, do NOT drop) ---")
                        (conj (if (seq residues)
                                (str/join "\n"
                                          (map residue-summary
                                               (take max-residue residues)))
                                "(none)"))
                        (conj "")
                        (conj "--- Evidence ---")
                        (conj (if (seq evidence)
                                (str/join "\n" (map :evidence/text evidence))
                                "(none)"))
                        (conj ""))]
     (if (seq archiver-reports)
       (let [archiver-lines (mapv #(str "- " (:archiver/description %)
                                         ": "
                                         (pr-str (:archiver/serialized %)))
                                  archiver-reports)
             all-parts (into base-parts (cons "[TOOL ARCHIVES]" archiver-lines))]
         (str/join "\n" all-parts))
       (str/join "\n" base-parts)))))
