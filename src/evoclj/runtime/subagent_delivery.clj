(ns evoclj.runtime.subagent-delivery
  "S5 — result delivery of a settled child terminal to its parent's causal chain.

  Split out of evoclj.runtime.subagent, which had grown to 1400+ lines across
  three unrelated concerns. This namespace owns the delivery cluster: the
  canonical Work-handle paths (deliver-result-for-works! /
  deliver-failure-for-works!), the legacy session-id entry points
  (deliver-result! / deliver-failure!), and the runner's automatic
  close-the-loop call (auto-deliver-child-terminal!).

  Every delivery records an explicit causal link
  (:causal-links #{ {:from <child-terminal> :type :subagent/result} }) rather
  than overloading :prev/event-id, so cross-session causality is visible in the
  graph (E1). Each append is ONE BEGIN IMMEDIATE transaction that FIRST
  re-checks the child Work is still in its expected terminal state (the
  provenance CAS), so a concurrent cancel/settle aborts delivery instead of
  recording a lie.

  `resolve-child-work-id!` lives HERE rather than with the spawn lifecycle
  because both the lifecycle (run-subagent!) and delivery need it, and
  delivery's legacy entry points resolve the child's Work by session id. Owning
  it here keeps the dependency edge one-way (lifecycle -> delivery) instead of
  creating a cycle."
  (:require [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.event :as event]
            [evoclj.store.event-schema :as es]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.work :as work-store]))

;; The Delivery record is the delivery request value (F1). Moved here with the
;; cluster it belongs to; CancelPlan stays behind (it is the cancel plan).
(defrecord Delivery [child-terminal parent-work causal-link payload-ref])

;; --- child Work resolution (shared by the lifecycle and delivery) -----------

(defn resolve-child-work-id!
  "Resolve the single execution Work for child session `child-id`.
  When `work-id` is supplied it must exist and belong to `child-id`
  (:store/work-not-found / :store/work-invalid). When nil, the child's
  latest Work is used (nil for a never-spawned bare session, whose run
  mints its own :session/run Work). More than one Work on the child is
  :store/work-invalid :reason :multiple-child-works — one spawn mints
  exactly one Work, so two rows mean a second spawn wrote where only a
  run should read."
  [db child-id work-id]
  (if (some? work-id)
    (let [w (work-store/fetch-work (sqlite/db-spec db) work-id)]
      (when-not w
        (throw (err/error :store/work-not-found "child work not found" {:work/id work-id})))
      (when (not= child-id (:work/session-id w))
        (throw (err/error :store/work-invalid "child work belongs to another session"
                          {:reason :child-work-mismatch
                           :work/id (:work/id w)
                           :work/session-id (:work/session-id w)
                           :child/session-id child-id})))
      (:work/id w))
    (let [works (work-store/list-works db child-id)]
      (when (> (count works) 1)
        (throw (err/error :store/work-invalid "child session has more than one Work"
                          {:reason :multiple-child-works
                           :child/session-id child-id
                           :work/ids (mapv :work/id works)})))
      (some-> (last works) :work/id))))

;; ---------------------------------------------------------------------------
;; S5 — result delivery to parent chain
;; ---------------------------------------------------------------------------

(defn- sha256-cas-ref?
  [s]
  (and (string? s) (boolean (re-matches #"^sha256:[0-9a-f]{64}$" s))))

(defn- delivered-result-event
  "The existing :subagent/result event on `parent-id` that already names
  `terminal-event-id` in its metadata, or nil. Delivery is idempotent:
  re-delivering the same child terminal returns the recorded event
  instead of appending a duplicate."
  [db parent-id terminal-event-id]
  (some (fn [ev]
          (when (and (= :subagent/result (:event/type ev))
                     (= terminal-event-id (get-in ev [:metadata :terminal/event-id])))
            ev))
        (try (event/events-for-session db parent-id) (catch Exception _ nil))))

(defn- check-delivery-terminal!
  "Provenance gate shared by both delivery paths: `terminal-event-id`
  must name the child's LATEST event and it must carry `expected-type`
  (:session/completed or :session/failed). Returns the terminal event."
  [db child-id terminal-event-id expected-type]
  (let [terminal (try (event/get-event-by-id db terminal-event-id) (catch Exception _ nil))]
    (when-not terminal
      (throw (err/error :store/event-invalid "delivery terminal event not found"
                        {:terminal/event-id terminal-event-id :child/session-id child-id})))
    (when-not (and (= child-id (:session/id terminal))
                   (= expected-type (:event/type terminal)))
      (throw (err/error :store/event-invalid "delivery terminal is not the child's expected terminal event"
                        {:terminal/event-id terminal-event-id
                         :child/session-id child-id
                         :event/session-id (:session/id terminal)
                         :event/type (:event/type terminal)
                         :expected/type expected-type})))
    (let [latest-id (some-> (last (try (event/events-for-session db child-id)
                                       (catch Exception _ nil)))
                            :event/id)]
      (when-not (= terminal-event-id latest-id)
        (throw (err/error :store/event-invalid "delivery terminal is not the child's latest event (stale link)"
                          {:terminal/event-id terminal-event-id
                           :latest/event-id latest-id
                           :child/session-id child-id}))))
    terminal))

(defn- append-result-event-tx!
  "Append the :subagent/result event to `parent-id` inside ONE BEGIN
  IMMEDIATE transaction that FIRST re-checks the child Work is still in
  `expected-state` (the provenance CAS — a concurrent cancel/settle that
  moved the row aborts delivery instead of recording a lie). The causal
  link points at the explicit `terminal-event-id`, never 'the latest'.
  Returns the appended event."
  [db parent child-work-id terminal-event-id expected-state metadata]
  (let [parent-id (:session/id parent)
        spec (sqlite/db-spec db)]
    (sqlite/with-write-tx [conn spec]
      (let [row (first (sqlite/query-raw! conn "SELECT state FROM works WHERE id = ?" [(str child-work-id)]))
            state (:state row)]
        (when-not (= ({:succeeded "succeeded" :failed "failed"} expected-state) state)
          (throw (err/error (if (= :succeeded expected-state) :subagent/not-completed :subagent/not-failed)
                            "child work left its terminal state inside the delivery transaction"
                            {:work/id child-work-id :work/state state}))))
      (let [prev-id (event/latest-event-id-on-conn conn parent-id)]
        (when-not prev-id
          (throw (err/error :store/event-invalid "parent session has no events"
                            {:session/id parent-id})))
        (let [req {:session/id parent-id
                   :generation/id (:generation/id parent)
                   :phenotype/id (:phenotype/id parent)
                   :event/type :subagent/result
                   :prev/event-id prev-id
                   :causal-links #{{:from terminal-event-id :type :subagent/result}}
                   :payload-ref nil
                   :metadata metadata}]
          (es/validate-append-request req)
          (event/append-event-on-conn! conn req))))))

(defn deliver-result-for-works!
  "Canonical Work-handle success delivery: link child Work `child-work-id`
  (must be :succeeded) to its parent under an explicit terminal event.

  `parent-session-id` names the receiving session; `parent-work-id` names
  the exact parent Work (nil for legacy parents that own no Work — the
  link-equality check is then skipped, child provenance still enforced).
  `terminal-event-id` must be the child's latest event and a
  :session/completed; `cas-ref` must be sha256:<64 hex> AND equal the
  child Work's :work/payload-ref whenever the row carries one (the
  scheduler persists the output CAS ref on success — a caller-supplied
  ref that differs is :store/cas-mismatch, never recorded).

  The event carries {:child/session-id _ :child/work-id _
  :terminal/event-id _ :result/cas-ref _ :result/status :succeeded} and a
  causal link from the terminal event. Commit is one BEGIN IMMEDIATE
  transaction (provenance re-check + append). Idempotent: re-delivering
  the same terminal returns the recorded event.

  Typed errors: :store/session-invalid (nil db), :store/work-not-found,
  :subagent/not-found (child session missing), :store/session-not-found
  (parent missing), :subagent/not-completed, :store/work-invalid (child
  not attributed to the parent work), :store/cas-invalid,
  :store/cas-mismatch, :store/event-invalid (unknown, foreign, stale, or
  non-terminal event)."
  [db parent-session-id parent-work-id child-work-id terminal-event-id cas-ref]
  (when (nil? db)
    (throw (ex-info "deliver-result-for-works! requires a db/store handle" {:error/type :store/session-invalid})))
  (when-not (sha256-cas-ref? cas-ref)
    (throw (ex-info (str "invalid cas-ref: " cas-ref)
                    {:error/type :store/cas-invalid
                     :cas-ref cas-ref})))
  (let [spec (sqlite/db-spec db)
        child-work (work-store/fetch-work spec child-work-id)]
    (when-not child-work
      (throw (err/error :store/work-not-found "child work not found" {:work/id child-work-id})))
    (when-not (= :succeeded (:work/state child-work))
      (throw (err/error :subagent/not-completed (str "child not completed: work=" (:work/state child-work))
                        {:work/id child-work-id
                         :work/state (:work/state child-work)})))
    (let [parent-work (when parent-work-id (work-store/fetch-work spec parent-work-id))]
      (when (and parent-work-id (not parent-work))
        (throw (err/error :store/work-not-found "parent work not found" {:work/id parent-work-id})))
      (when (and parent-work
                 (:work/parent-work-id child-work)
                 (not= (:work/id parent-work) (:work/parent-work-id child-work)))
        (throw (err/error :store/work-invalid "child work is not attributed to the parent work"
                          {:reason :work-link-mismatch
                           :work/id child-work-id
                           :parent/work-id parent-work-id
                           :work/parent-work-id (:work/parent-work-id child-work)})))
      (when (and parent-work
                 (not= (:work/session-id parent-work)
                        (try (types/session-id parent-session-id) (catch Exception _ parent-session-id))))
        (throw (err/error :store/work-invalid "parent work belongs to another session"
                          {:reason :parent-work-mismatch
                           :parent/work-id parent-work-id
                           :parent/session-id parent-session-id})))
      (let [child-id (:work/session-id child-work)
            parent-id (try (types/session-id parent-session-id) (catch Exception _ parent-session-id))]
        (when-not (session/get-session db child-id)
          (throw (ex-info (str "child session not found: " child-id)
                          {:error/type :subagent/not-found
                           :session/id child-id})))
        (let [parent (session/get-session db parent-id)]
          (when-not parent
            (throw (ex-info (str "parent session not found: " parent-id)
                            {:error/type :store/session-not-found
                             :session/id parent-id})))
          (check-delivery-terminal! db child-id terminal-event-id :session/completed)
          (let [bound (:work/payload-ref child-work)]
            (when (and (string? bound) (not= bound cas-ref))
              (throw (err/error :store/cas-mismatch "supplied cas-ref differs from the child Work payload_ref"
                                {:work/id child-work-id
                                 :work/payload-ref bound
                                 :result/cas-ref cas-ref}))))
          (let [delivery (->Delivery terminal-event-id parent-work-id
                                    {:from terminal-event-id :type :subagent/result}
                                    cas-ref)]
            (if-let [existing (delivered-result-event db parent-id (:child-terminal delivery))]
              existing
              (append-result-event-tx! db parent child-work-id (:child-terminal delivery) :succeeded
                                       {:child/session-id child-id
                                        :child/work-id child-work-id
                                        :terminal/event-id (:child-terminal delivery)
                                        :result/cas-ref (:payload-ref delivery)
                                        :result/status :succeeded}))))))))

(defn deliver-failure-for-works!
  "Canonical Work-handle failure delivery: link child Work `child-work-id`
  (must be :failed) to its parent under an explicit terminal event. The
  recorded :error is DERIVED from the canonical rows — the child terminal
  :session/failed event's metadata (:error/artifact-ref, :error/type,
  :status) — never trusted from the caller. `opts` may carry
  :error/fallback, used only when the terminal event records no error
  detail (else {:error/type :subagent/child-failed}). Same atomicity,
  idempotency, and typed-error contract as deliver-result-for-works!
  (with :subagent/not-failed for a non-failed child)."
  ([db parent-session-id parent-work-id child-work-id terminal-event-id]
   (deliver-failure-for-works! db parent-session-id parent-work-id child-work-id terminal-event-id nil))
  ([db parent-session-id parent-work-id child-work-id terminal-event-id opts]
   (when (nil? db)
     (throw (ex-info "deliver-failure-for-works! requires a db/store handle" {:error/type :store/session-invalid})))
   (let [spec (sqlite/db-spec db)
         child-work (work-store/fetch-work spec child-work-id)]
     (when-not child-work
       (throw (err/error :store/work-not-found "child work not found" {:work/id child-work-id})))
     (when-not (= :failed (:work/state child-work))
       (throw (err/error :subagent/not-failed (str "child not failed: work=" (:work/state child-work))
                         {:work/id child-work-id
                          :work/state (:work/state child-work)})))
     (let [parent-work (when parent-work-id (work-store/fetch-work spec parent-work-id))]
       (when (and parent-work-id (not parent-work))
         (throw (err/error :store/work-not-found "parent work not found" {:work/id parent-work-id})))
       (when (and parent-work
                  (:work/parent-work-id child-work)
                  (not= (:work/id parent-work) (:work/parent-work-id child-work)))
         (throw (err/error :store/work-invalid "child work is not attributed to the parent work"
                           {:reason :work-link-mismatch
                            :work/id child-work-id
                            :parent/work-id parent-work-id
                            :work/parent-work-id (:work/parent-work-id child-work)})))
       (let [child-id (:work/session-id child-work)
             parent-id (try (types/session-id parent-session-id) (catch Exception _ parent-session-id))]
         (when-not (session/get-session db child-id)
           (throw (ex-info (str "child session not found: " child-id)
                           {:error/type :subagent/not-found
                            :session/id child-id})))
         (let [parent (session/get-session db parent-id)]
           (when-not parent
             (throw (ex-info (str "parent session not found: " parent-id)
                             {:error/type :store/session-not-found
                              :session/id parent-id})))
           (let [terminal (check-delivery-terminal! db child-id terminal-event-id :session/failed)
                 derived (select-keys (:metadata terminal) [:error/artifact-ref :error/type :status])
                 fallback (:error/fallback opts)
                 error (cond (seq derived) (cond-> derived
                                             (and fallback (not= fallback derived))
                                             (assoc :error/caller fallback))
                             (some? fallback) fallback
                             :else {:error/type :subagent/child-failed})]
            (let [delivery (->Delivery terminal-event-id parent-work-id
                                      {:from terminal-event-id :type :subagent/result}
                                      nil)]
              (if-let [existing (delivered-result-event db parent-id (:child-terminal delivery))]
                existing
                (append-result-event-tx! db parent child-work-id (:child-terminal delivery) :failed
                                         {:child/session-id child-id
                                          :child/work-id child-work-id
                                          :terminal/event-id (:child-terminal delivery)
                                          :result/status :failed
                                          :error error}))))))))))

(defn deliver-result!
  "Deliver a successful child subagent result to its parent's causal chain
  (legacy session-id entry point — resolves the child's single Work, the
  attributed (or latest) parent Work, and the child's latest event, then
  delegates to deliver-result-for-works!). Prefer the Work-handle
  canonical path for new callers.

  E1: appends a :subagent/result event to the parent's chain with
  `:prev/event-id = parent's latest` and
  `:causal-links #{ {:from <child-terminal-event-id> :type :subagent/result} }`
  so cross-session causality is explicit in the graph, not overloaded
  onto prev. The event also carries :child/work-id and
  :terminal/event-id.

  The supplied `cas-ref` must equal the child Work's :work/payload-ref
  whenever the row carries one (:store/cas-mismatch otherwise).

  Typed errors: :store/session-invalid when db nil, :store/session-not-found
  when parent missing, :subagent/not-found when child missing,
  :subagent/not-completed when child is not :succeeded, :store/cas-invalid
  when cas-ref is not sha256:<64 hex>, :store/cas-mismatch on provenance
  drift, :store/work-not-found / :store/work-invalid on handle problems,
  :store/event-invalid on terminal problems."
  [db parent-session-id child-session-id cas-ref]
  (when (nil? db)
    (throw (ex-info "deliver-result! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [parent-id (types/session-id parent-session-id)
        child-id (types/session-id child-session-id)]
    (when-not (sha256-cas-ref? cas-ref)
      (throw (ex-info (str "invalid cas-ref: " cas-ref)
                      {:error/type :store/cas-invalid
                       :cas-ref cas-ref})))
    (when-not (session/get-session db child-id)
      (throw (ex-info (str "child session not found: " child-id)
                      {:error/type :subagent/not-found
                       :session/id child-id})))
    (let [child-work-id (resolve-child-work-id! db child-id nil)
          child-work (when child-work-id (work-store/fetch-work (sqlite/db-spec db) child-work-id))]
      (when-not (and child-work (= :succeeded (:work/state child-work)))
        (throw (ex-info (str "child not completed: " child-id " work=" (:work/state child-work))
                        {:error/type :subagent/not-completed
                         :session/id child-id
                         :work/state (:work/state child-work)})))
      (when-not (session/get-session db parent-id)
        (throw (ex-info (str "parent session not found: " parent-id)
                        {:error/type :store/session-not-found
                         :session/id parent-id})))
      (let [parent-work-id (or (:work/parent-work-id child-work)
                               (some-> (last (work-store/list-works db parent-id)) :work/id))
            terminal-id (some-> (last (try (event/events-for-session db child-id)
                                           (catch Exception _ nil)))
                                :event/id)]
        (when-not terminal-id
          (throw (err/error :store/event-invalid "child session has no terminal event"
                            {:session/id child-id})))
        (deliver-result-for-works! db parent-id parent-work-id child-work-id terminal-id cas-ref)))))

(defn deliver-failure!
  "Deliver a failed child subagent result to its parent's causal chain
  (legacy session-id entry point — resolves handles, then delegates to
  deliver-failure-for-works!). The recorded :error is derived from the
  canonical child terminal event; the supplied `error` is kept only as a
  fallback when the terminal records none.

  E1: appends :subagent/result with prev = parent latest and
  causal-links #{ {:from <child-terminal> :type :subagent/result} }.
  Typed errors mirror deliver-result! but with :subagent/not-failed when child not failed."
  [db parent-session-id child-session-id error]
  (when (nil? db)
    (throw (ex-info "deliver-failure! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [parent-id (types/session-id parent-session-id)
        child-id (types/session-id child-session-id)]
    (when-not (session/get-session db child-id)
      (throw (ex-info (str "child session not found: " child-id)
                      {:error/type :subagent/not-found
                       :session/id child-id})))
    (let [child-work-id (resolve-child-work-id! db child-id nil)
          child-work (when child-work-id (work-store/fetch-work (sqlite/db-spec db) child-work-id))]
      (when-not (and child-work (= :failed (:work/state child-work)))
        (throw (ex-info (str "child not failed: " child-id " work=" (:work/state child-work))
                        {:error/type :subagent/not-failed
                         :session/id child-id
                         :work/state (:work/state child-work)})))
      (when-not (session/get-session db parent-id)
        (throw (ex-info (str "parent session not found: " parent-id)
                        {:error/type :store/session-not-found
                         :session/id parent-id})))
      (let [parent-work-id (or (:work/parent-work-id child-work)
                               (some-> (last (work-store/list-works db parent-id)) :work/id))
            terminal-id (some-> (last (try (event/events-for-session db child-id)
                                           (catch Exception _ nil)))
                                :event/id)]
        (when-not terminal-id
          (throw (err/error :store/event-invalid "child session has no terminal event"
                            {:session/id child-id})))
        (deliver-failure-for-works! db parent-id parent-work-id child-work-id terminal-id
                                    (when (some? error) {:error/fallback error}))))))

(defn auto-deliver-child-terminal!
  "Auto-deliver a settled child's terminal to its parent (structured
  result handoff — the runner, not the model, closes the loop). Reads
  the child Work's own :work/parent-work-id for the parent handle and
  the child's latest event for the terminal; success delivers with the
  Work's persisted payload ref as the CAS ref, failure derives the error
  from the canonical terminal event. Never throws: returns
  {:delivered true :event/id _} or {:delivered false :reason _
  [:work/state _] [:error/type _]} for the run result's :delivery."
  [db parent-id child-id child-work-id]
  (try
    (let [child-work (work-store/fetch-work (sqlite/db-spec db) child-work-id)]
      (cond
        (nil? child-work)
        {:delivered false :reason :work-not-found :work/id child-work-id}
        ;; A nil :work/parent-work-id (legacy tool-path spawns against a
        ;; workless parent) still delivers — the canonical path skips only
        ;; the link-equality check, child provenance still enforced.
        :else
        (let [terminal-id (some-> (last (try (event/events-for-session db child-id)
                                             (catch Exception _ nil)))
                                  :event/id)]
          (if-not terminal-id
            {:delivered false :reason :no-terminal-event}
            (case (:work/state child-work)
              :succeeded
              (if-let [cas (:work/payload-ref child-work)]
                {:delivered true
                 :event/id (:event/id (deliver-result-for-works! db parent-id (:work/parent-work-id child-work)
                                                                 child-work-id terminal-id cas))}
                {:delivered false :reason :no-payload-ref :work/id child-work-id})
              :failed
              {:delivered true
               :event/id (:event/id (deliver-failure-for-works! db parent-id (:work/parent-work-id child-work)
                                                                 child-work-id terminal-id))}
              {:delivered false :reason :not-terminal :work/state (:work/state child-work)})))))
    (catch clojure.lang.ExceptionInfo e
      {:delivered false :reason :delivery-error :error/type (:error/type (ex-data e))})
    (catch Throwable t
      {:delivered false :reason :delivery-error :error/type :subagent/delivery-failed})))
