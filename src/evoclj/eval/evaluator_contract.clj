(ns evoclj.eval.evaluator-contract
  "The shared evaluator-context boundary RULE: an evaluator context must
  be a map carrying its site's required keys.

  The three evaluation entry points (evoclj.eval.core's orchestrator,
  evoclj.eval.paired's G5 runner, evoclj.eval.replay's G4 runner) each
  receive a DIFFERENT subset of the evaluator map — core's
  replay-context / paired-context are pure select-keys projections, so
  the required-key list is genuinely per-site. What must not be
  duplicated is the rule itself and its message template, so this
  namespace owns exactly that:

    - the map? check            => reason :not-a-map
    - the required-key check    => the site's reason per missing key
    - the messages              => \"<label> must be a map\" and
                                   \"<label> is missing the <key> key\"

  The calling site owns its error TYPE (a constructor fn such as
  :eval/evaluator-invalid / :eval/paired-context-invalid /
  :eval/replay-context-invalid), its reason keywords, its label, its
  required keys, and every structural check beyond the two rules above
  (e.g. core's :store/:profiles/:genome-roots/:dataset-roots shapes and
  paired's :artifact/root type).")

(defn validate-evaluator!
  "Validate `evaluator` against one site's context contract.

  `site` keys:
    :error-fn  the site's error CONSTRUCTOR — called as
               (error-fn reason message value); the returned throwable
               is thrown here, so a non-throwing constructor still
               fails closed.
    :label     the human subject of the messages (\"evaluator\",
               \"paired evaluator context\", \"replay evaluator
               context\").
    :required  an ordered [[key reason] ...] vector of the keys the site
               demands.

  Returns `evaluator` unchanged when valid; throws otherwise."
  [{:keys [error-fn required label]} evaluator]
  (when-not (map? evaluator)
    (throw (error-fn :not-a-map
                     (str label " must be a map")
                     evaluator)))
  (doseq [[k reason] required]
    (when-not (contains? evaluator k)
      (throw (error-fn reason
                       (str label " is missing the " k " key")
                       evaluator))))
  evaluator)
