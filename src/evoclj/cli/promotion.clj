(ns evoclj.cli.promotion
  "The promotion-facing CLI commands (Task 10.2): `promote`,
  `rollback`, and `lineage`.

  `promote` and `rollback` are the ONLY cli commands that move the
  CURRENT generation pointer, and both go exclusively through the
  public Promotion APIs (promotion.promote/promote! and
  promotion.rollback/rollback! — the atomic CURRENT compare-and-set,
  Global Constraint 15). This namespace contains no SQL of any kind
  and no dependency on the CURRENT machinery (no promotion.current
  alias): the current generation is read through the public recovery
  scan (evoclj.cli.session/current-generation-info) and the pointer
  moves only inside promote!/rollback!'s transactions. `lineage` is a
  read-only reconstruction through promotion.lineage/lineage."
  (:require [clojure.string :as str]
            [evoclj.cli.session :as session]
            [evoclj.compiler.core :as compiler]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.kernel.error :as err]
            [evoclj.promotion.lineage :as lineage]
            [evoclj.promotion.promote :as promote]
            [evoclj.promotion.rollback :as rollback])
  (:import (java.util UUID)))

;; --- shared helpers ----------------------------------------------------------

(defn- positional
  [opts n]
  (let [pos (:positionals opts)]
    (or (nth pos n nil)
        (throw (err/error :cli/usage-invalid
                          "missing positional argument"
                          {:usage (str "expected " (inc n) " positional argument(s)")})))))

(defn- required-opt
  [opts k usage]
  (or (get-in opts [:options k])
      (throw (err/error :cli/usage-invalid
                        (str "missing required option --" (name k))
                        {:usage usage}))))

(defn- uuid-arg [s]
  (try (UUID/fromString (str s))
       (catch Exception _
         (throw (err/error :cli/usage-invalid
                           "expected a uuid"
                           {:value s})))))

(defn- parse-keyword
  "Parse a keyword option value (':canary-regression' or
  'canary-regression')."
  [s]
  (let [t (str s)]
    (if (str/starts-with? t ":")
      (keyword (subs t 1))
      (keyword t))))

(defn- compiled-resolution-id
  "The compiled ResolutionId of a candidate Genome bundle (compilation
  is the host's job — promote! never compiles)."
  [bundle-root]
  (:compiled/resolution-id
   (compiler/compile-genome (session/load-genome-for-execution bundle-root)
                            session/provider-catalog)))

;; --- commands ----------------------------------------------------------------

(defn promote!
  "evoclj promote <candidate-id> --evaluation <id>

  Promote an :evaluated candidate through promotion.promote/promote!
  (the atomic CURRENT compare-and-set — Global Constraint 15). The
  CLI builds the promotion-system with the candidate Genome's compiled
  Resolution id and a fresh operator session pinned to the candidate's
  parent generation (the event anchor promote! requires). Returns the
  outcome ({:status :promoted :from :to} or {:status :stale ...})."
  [opts]
  (let [cand-id (uuid-arg (positional opts 0))
        eval-id (uuid-arg (required-opt opts :evaluation
                                        "evoclj promote <candidate-id> --evaluation <id>"))
        system (session/build-system opts)
        store (session/store-of system)
        c (candidate/find-candidate store cand-id)]
    (when-not c
      (throw (err/error :cli/candidate-not-found
                        "no candidate with this id"
                        {:candidate/id cand-id})))
    (let [parent-gen-id (:parent/generation-id c)
          op-session (session/operator-session! opts system parent-gen-id)
          candidate-root (session/candidate-bundle-root opts
                                                        (:candidate/genome-id c))
          promotion-system {:store store
                            :resolution/id (compiled-resolution-id candidate-root)
                            :event/session-id op-session}
          result (promote/promote! promotion-system
                                   {:candidate-id cand-id
                                    :evaluation-id eval-id
                                    :expected-parent-generation parent-gen-id})]
      result)))

(defn rollback!
  "evoclj rollback --to <generation-id> --reason <keyword>

  Move the CURRENT pointer back to a superseded generation through
  promotion.rollback/rollback! (selection-only — Global Constraint
  18; nothing is deleted and no external effect is compensated). The
  operator session is pinned to the CURRENT (from-) generation."
  [opts]
  (let [to (required-opt opts :to
                         "evoclj rollback --to <generation-id> --reason <keyword>")
        reason (required-opt opts :reason
                             "evoclj rollback --to <generation-id> --reason <keyword>")
        system (session/build-system opts)
        current (session/current-generation-info system)]
    (when-not current
      (throw (err/error :promotion/cas-invalid
                        "no CURRENT generation to roll back from"
                        {})))
    (let [from (:generation/id current)
          identity (session/generation-identity opts system from)
          op-session (session/operator-session! opts system from)
          promotion-system {:store (session/store-of system)
                            :resolution/id (:resolution/id identity)
                            :event/session-id op-session}
          result (rollback/rollback! promotion-system
                                     {:from-generation from
                                      :to-generation to
                                      :reason (parse-keyword reason)})]
      result)))

(defn lineage!
  "evoclj lineage <generation-id>

  Reconstruct the complete evolutionary history of a generation
  (promotion.lineage/lineage, strict integrity verification)."
  [opts]
  (let [generation (positional opts 0)
        system (session/build-system opts)
        store (session/store-of system)
        result (lineage/lineage store generation)]
    result))
