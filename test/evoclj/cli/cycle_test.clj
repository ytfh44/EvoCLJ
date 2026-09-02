(ns evoclj.cli.cycle-test
  "Feature B - the cycle CLI command: one operator command that walks the
  full loop (evolve -> eval -> promote) through the public subsystem APIs.

  These are OFFLINE end-to-end tests on the FIXTURE path (no models): the
  real pattern Diagnostician over a recorded Evolution set plus a
  deterministic Mutator (the recording-mutator pattern), exactly as
  test/evoclj/promotion/e2e_evolution_test.clj and the cli
  cli-evolve-eval-promote-rollback-end-to-end test provision them. A single
  'evoclj cycle' invocation materializes the candidate
  (:evaluation-pending), evaluates it through evaluate-candidate! with the
  host-injected replay/selection cases and fixture providers, and - when its
  :eligibility :eligible? is exactly true - promotes it via the atomic
  promotion.promote/promote! CAS (moving CURRENT from generation-1 to the
  candidate generation).

  The report shape under test:

      {:generation/id <stable-id>
       :phases {:evolve {:run? bool :candidates [<candidate-shape> ...]}
                :eval [{:candidate/id <uuid> :evaluation/id <uuid>
                        :eligibility {:eligible? bool :reasons [...]}} ...]
                :promote [{:candidate/id <uuid> :status <kw> :outcome <result>
                           :error <data>} ...]}}

  Fixture genomes: test/fixtures/evolution-e2e/route-a is G1 (tool A serves
  :echo-a; :echo-b fails), route-b is the reference G2 the deterministic
  mutation reproduces, and the selection dataset
  (test/fixtures/evolution-e2e/selection) holds the hidden paired cases.

  IMPORTANT (Global Constraint 15 - cycle never activates on its own): the
  CURRENT pointer moves ONLY inside promotion.promote/promote! atomic CAS
  transaction, never in the cli layer. The promote phase additionally
  performs the host bookkeeping (Database Invariant 7) of storing the
  candidate Genome canonical body into the CAS so promote! integrity
  re-hash passes - the same bookkeeping a standalone 'promote' assumes an
  operator already did."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.cli.main :as main]
            [evoclj.compiler.core :as compiler]
            [evoclj.eval.core :as eval-core]
            [evoclj.eval.replay :as replay]
            [evoclj.genome.load :as load]
            [evoclj.genome.path :as gpath]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.promotion.current :as current]
            [evoclj.promotion.lineage :as lineage]
            [evoclj.runtime.episode :as episode]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

;; ----------------------------------------------------------------------------
;; fixture identity + plumbing (mirrors cli_test / e2e_evolution_test)
;; ----------------------------------------------------------------------------

(def ^:private generation-id "generation-1")

(defn- route-a-root [] (str (io/file "test" "fixtures" "evolution-e2e" "route-a")))
(defn- selection-root [] (str (io/file "test" "fixtures" "evolution-e2e" "selection")))

(defn- route-descriptor []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry (quote agent.route/run)
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- fixture-catalog []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- dash [id] (str/replace id ":" "-"))

(def ^:private temp-paths (atom []))

(defn- temp-dir [prefix]
  (let [d (str (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
    (swap! temp-paths conj d)
    d))

(defn- delete-tree! [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup! []
  (doseq [p @temp-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- copy-tree! [src dest]
  (let [from (Paths/get src (make-array String 0))
        to (Paths/get dest (make-array String 0))]
    (with-open [stream (Files/walk from (make-array FileVisitOption 0))]
      (doseq [p (iterator-seq (.iterator stream))]
        (let [rel (.relativize from p)
              target (.resolve to rel)]
          (when (Files/isDirectory p (make-array LinkOption 0))
            (Files/createDirectories target (make-array FileAttribute 0)))
          (when (Files/isRegularFile p (make-array LinkOption 0))
            (Files/createDirectories (.getParent target)
                                     (make-array FileAttribute 0))
            (Files/copy p target (make-array java.nio.file.CopyOption 0))))))))

(defn- genome-index-bytes [loaded]
  (apply str
         (map (fn [[p {:keys [digest]}]]
                (str p "\u0000" digest "\n"))
              (sort-by (fn [[p _]] p) gpath/bytewise-compare (:files loaded)))))

(defn- program-sources [loaded compiled]
  (into {}
        (map (fn [[program-id descriptor]]
               [program-id
                (String. ^bytes (byte-array
                                 (get-in loaded [:files (:file descriptor) :bytes]))
                        StandardCharsets/UTF_8)]))
        (:programs compiled)))

(defn- compile-bundle [bundle-root]
  (let [loaded (assoc (load/load-genome bundle-root)
                      :programs [(route-descriptor)])]
    {:loaded loaded
     :compiled (compiler/compile-genome loaded (fixture-catalog))}))

(defn- provision!
  "A temp state dir provisioned like a real host deployment: migrated db, the
  generation-1 row (current = 1), G1 canonical body in the CAS, and the G1
  bundle at <state-dir>/genomes/<id-as-dash>."
  []
  (let [dir (temp-dir "evoclj-cycle-state-")
        _ (Files/createDirectories (Paths/get (str dir "/db")
                                              (make-array String 0))
                                   (make-array FileAttribute 0))
        db-path (str dir "/db/evoclj.db")
        db (sqlite/spec db-path)
        _ (migrate/migrate! db)
        {:keys [loaded compiled]} (compile-bundle (route-a-root))
        genome-id (:compiled/genome-id compiled)
        resolution-id (:compiled/resolution-id compiled)
        cas-root (str dir "/cas")
        cas-store (cas/->cas cas-root)]
    (sqlite/with-db [conn db]
      (doseq [[artifact-id media-type]
              [[genome-id "application/octet-stream"]
               [resolution-id "application/edn"]
               [(:compiled/phenotype-id compiled) "application/edn"]]]
        (jdbc/insert! conn :artifacts
                      {:hash artifact-id
                       :media_type media-type
                       :size 0
                       :created_at "2025-01-01T00:00:00Z"}))
      (jdbc/insert! conn :genomes
                    {:id genome-id
                     :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    (cas/put-bytes! cas-store
                    (.getBytes (genome-index-bytes loaded) StandardCharsets/UTF_8)
                    {})
    (copy-tree! (route-a-root) (str dir "/genomes/" (dash genome-id)))
    {:state-dir dir :db db :db-path db-path :cas-root cas-root
     :cas-store cas-store :loaded loaded :compiled compiled
     :genome-id genome-id :resolution-id resolution-id}))

(defn- echo-lease [phenotype-id]
  (let [now (java.util.Date.)]
    {:cap/id (random-uuid)
     :principal {:principal/type :session :session/id #uuid "00000000-0000-4000-a000-000000000000"}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 100}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(defn- run-fixture-session! [ctx task]
  (let [{:keys [db cas-store compiled loaded]} ctx
        reg (registry/create-registry)
        _ (registry/register! reg (fixture/echo-provider {}))
        _ (registry/register! reg (fixture/non-idempotent-provider {}))
        usage (atom {})
        lease (echo-lease (:compiled/phenotype-id compiled))
        ph (phenotype/instantiate
            compiled
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry reg}
             :capabilities {:leases [lease] :usage usage}
             :program-sources (program-sources loaded compiled)})
        executor {:phenotype ph
                  :stores {:sqlite db :cas cas-store}
                  :dispatch (dispatch/make-broker-context
                             {:registry reg :leases [lease] :usage usage})}
        sid (:session/id
             (session/create-session!
              db
              {:genome/id (:compiled/genome-id compiled)
               :resolution/id (:compiled/resolution-id compiled)
               :phenotype/id (:compiled/phenotype-id compiled)
               :generation/id generation-id}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id generation-id
                          :phenotype/id (:compiled/phenotype-id compiled)
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    (let [result (scheduler/run-session! executor sid task)]
      (episode/materialize-episode! {:sqlite db :cas cas-store} sid)
      {:session/id sid :result result :executor executor})))

(defn- read-artifact [store artifact-id]
  (edn/read-string
   (String. (cas/get-bytes (:cas store) artifact-id) StandardCharsets/UTF_8)))

(defn- replay-case-from-session [ctx sid task expected-output]
  (let [store {:sqlite (:db ctx) :cas (:cas-store ctx)}
        events (event/events-for-session (:db ctx) sid)
        completed (first (filter #(= :provider/call-completed (:event/type %))
                                 events))
        response (read-artifact store (:payload-ref completed))
        decision (first expected-output)
        payload (get-in decision [:action :payload])]
    (replay/build-replay-case
     {:episode/id (random-uuid)
      :outcome {:status :completed}}
     [{:intent/type :intent/tool-call
       :effect :read
       :payload payload
       :response response}]
     {:case/id :replay/a
      :task-input task
      :expected-output expected-output
      :mode :fixture})))

;; ----------------------------------------------------------------------------
;; evolution + evaluation fixtures (mirrors cli_test)
;; ----------------------------------------------------------------------------

(defn- deterministic-uuid [s]
  (UUID/nameUUIDFromBytes (.getBytes s StandardCharsets/UTF_8)))

(defn- echo-b-provider []
  (reify proto/Provider
    (describe [_]
      {:tool/id :fixture/echo-b
       :effect :pure
       :input-schema [:map [:text :string]]
       :output-schema [:map [:text :string]]
       :required-action :invoke
       :retry {:safe? true}})
    (normalize-request [_ intent]
      (let [args (get-in intent [:payload :args])]
        (when-not (map? args)
          (throw (ex-info "tool-call payload must carry an :args map"
                          {:error/type :provider/input-invalid})))
        {:tool/id :fixture/echo-b
         :resource {:kind :tool :id :fixture/echo-b}
         :args args}))
    (execute-request! [_ authorized-request]
      {:text (get-in authorized-request [:args :text])})))

(defn- g2-case-form []
  (list (quote case) (quote op)
        :echo-a {:action (list (quote tool-call-intent) :fixture/echo
                               {:text (list (quote get) (quote input) :text)})}
        :echo-b {:action (list (quote tool-call-intent) :fixture/echo-b
                               {:text (list (quote get) (quote input) :text)})}
        {:action (list (quote finish-intent) (quote input))}))

(defn- route-replacement-op [parent-genome form]
  {:op :replace-form
   :file "programs/route.clj"
   :selector [(quote case)]
   :expect/hash (get-in parent-genome [:files "programs/route.clj" :digest])
   :form form})

(defn- delta-mutation [parent diagnosis hypothesis form suffix]
  (let [content {:parent/genome-id (:genome/id parent)
                 :hypothesis/id (:hypothesis/id hypothesis)
                 :evidence/id (:evidence/id diagnosis)
                 :risk :program
                 :ops [(route-replacement-op parent form)]
                 :expected-effect {:primary-metric :task/success
                                   :direction :increase}}]
    (assoc content
           :mutation/id (deterministic-uuid (pr-str [content suffix])))))

(defn- propose-deltas [ctx]
  (when-let [hypothesis (some #(when (= :task/success (:pattern %)) %)
                              (:hypotheses (:diagnosis ctx)))]
    (let [parent (:parent-genome ctx)
          diagnosis (:diagnosis ctx)]
      [(delta-mutation parent diagnosis hypothesis (g2-case-form) "g2")])))

(defn- selection-cases []
  (into {}
        (map (fn [f]
               (let [c (edn/read-string (slurp f))]
                 [(:case/id c) c])))
        [(io/file (selection-root) "sel-a.edn")
         (io/file (selection-root) "sel-b.edn")]))

(defn- cycle-overrides
  "The single :overrides seam for one cycle: the deterministic Mutator
  (evolution) AND the evaluator hidden cases/fixtures (eval)."
  [captured replay-case]
  {:evolution/system
   {:mutator (fn [ctx]
               (reset! captured ctx)
               (propose-deltas ctx))}
   :eval/system
   {:selection/cases (selection-cases)
    :selection/fixtures
    {:fixture/echo (fn [_seed] (fixture/echo-provider {}))
     :fixture/echo-b (fn [_seed] (echo-b-provider))}
    :replay/cases {:replay/a replay-case}
    :replay/fixtures
    {:fixture/echo (fn [] (fixture/echo-provider {}))
     :fixture/echo-b (fn [] (echo-b-provider))}}})

(defn- provision-evolution-context
  "Provision + record the Evolution set (1 success + 2 failures) that makes
  the deterministic :task/success hypothesis fire, and build the replay case."
  []
  (let [ctx (provision!)
        task {:op :echo-a :text "hi"}
        expected [{:action {:intent/type :intent/tool-call
                            :payload {:tool/id :fixture/echo
                                      :args {:text "hi"}}}}
                  {:text "hi"}]
        run (run-fixture-session! ctx task)
        sid (:session/id run)]
    (run-fixture-session! ctx {:op :echo-b :text "bo"})
    (run-fixture-session! ctx {:op :echo-b :text "go"})
    (assoc ctx :task task :expected expected
           :replay-case (replay-case-from-session ctx sid task expected))))

;; ----------------------------------------------------------------------------
;; the full loop: evolve -> eval -> promote in ONE cycle invocation
;; ----------------------------------------------------------------------------

(deftest cycle-evolve-eval-promote-end-to-end
  (let [{:keys [state-dir db] :as ctx} (provision-evolution-context)
        captured (atom nil)
        overrides (cycle-overrides captured (:replay-case ctx))]
    (testing "evoclj cycle (default current) walks the full loop"
      (let [{:keys [exit data]} (main/execute ["cycle"]
                                               {:state-dir state-dir
                                                :overrides overrides})]
        (is (= 0 exit) "the cycle command exits 0")
        (is (= generation-id (:generation/id data)))
        (let [phases (:phases data)]
          (testing "EVOLVE produced exactly one :evaluation-pending candidate"
            (let [evolve (:evolve phases)]
              (is (true? (:run? evolve)))
              (let [candidates (:candidates evolve)]
                (is (= 1 (count candidates)))
                (let [g2 (first candidates)]
                  (is (= :evaluation-pending (:state g2)))
                  (is (= (:genome-id ctx) (:parent/genome-id g2)))
                  (is (not= (:genome-id ctx) (:candidate/genome-id g2)))
                  (is (= :program (:risk g2)))))))
          (testing "EVAL produced one passing Evaluation"
            (let [evals (:eval phases)]
              (is (= 1 (count evals)))
              (let [e (first evals)]
                (is (uuid? (:candidate/id e)))
                (is (uuid? (:evaluation/id e)))
                (is (true? (get-in e [:eligibility :eligible?]))))))
          (testing "PROMOTE moved CURRENT atomically"
            (let [promotes (:promote phases)]
              (is (= 1 (count promotes)))
              (let [p (first promotes)]
                (is (= :promoted (:status p)))
                (is (= generation-id (get-in p [:outcome :from])))
                (let [to (get-in p [:outcome :to])]
                  (is (not= generation-id to))
                  (is (= to (:id (current/current-generation db)))
                      "CURRENT now names the promoted generation")
                  (testing "lineage reconstructs G1 to G2"
                    (let [lg (lineage/lineage db generation-id)]
                      (is (= 1 (count (:children lg))))
                      (let [child (first (:children lg))]
                        (is (= to (get-in child [:generation :generation/id])))
                        (is (= :promoted (get-in child [:promotion :decision])))
                        (is (true? (get-in child [:evaluation :eligibility :eligible?])))))))))))))))

;; ----------------------------------------------------------------------------
;; a candidate whose evaluation FAILS a hard gate is NOT promoted
;; ----------------------------------------------------------------------------

(deftest cycle-does-not-promote-an-ineligible-candidate
  (let [{:keys [state-dir db] :as ctx} (provision-evolution-context)
        captured (atom nil)
        overrides (cycle-overrides captured (:replay-case ctx))]
    (testing "cycle records a hard-gate failure and leaves CURRENT unmoved"
      (with-redefs [eval-core/evaluate-candidate!
                    (fn [_evaluator cid _profile-id]
                      {:evaluation/id (random-uuid)
                       :candidate/id cid
                       :eligibility {:eligible? false
                                     :reasons [{:dimension :hard
                                                :rule :hard-violation}]}})]
        (let [{:keys [exit data]} (main/execute ["cycle"]
                                                {:state-dir state-dir
                                                 :overrides overrides})]
          (is (= 0 exit))
          (let [evals (:eval (:phases data))
                promotes (:promote (:phases data))]
            (is (= 1 (count evals)))
            (is (false? (get-in (first evals) [:eligibility :eligible?])))
            (is (= 1 (count (get-in (first evals) [:eligibility :reasons]))))
            (is (empty? promotes)
                "an ineligible candidate has NO promote entry")
            (is (= generation-id (:id (current/current-generation db)))
                "CURRENT still names generation-1 - nothing was promoted")))))))

;; ----------------------------------------------------------------------------
;; --no-promote evaluates but never moves CURRENT
;; ----------------------------------------------------------------------------

(deftest cycle-no-promote-evaluates-but-does-not-move-current
  (let [{:keys [state-dir db] :as ctx} (provision-evolution-context)
        captured (atom nil)
        overrides (cycle-overrides captured (:replay-case ctx))]
    (testing "evoclj cycle --no-promote produces a real evaluation but skips the move"
      (let [{:keys [exit data]} (main/execute ["cycle" "--no-promote"]
                                               {:state-dir state-dir
                                                :overrides overrides})]
        (is (= 0 exit))
        (let [evals (:eval (:phases data))
              promotes (:promote (:phases data))]
          (testing "eval ran and is eligible"
            (is (= 1 (count evals)))
            (is (uuid? (get-in (first evals) [:evaluation/id])))
            (is (true? (get-in (first evals) [:eligibility :eligible?]))))
          (testing "promote is reported as a would-be"
            (is (= 1 (count promotes)))
            (let [p (first promotes)]
              (is (= :skipped (:status p)))
              (is (= :no-promote (:reason p)))
              (is (true? (:eligible? p)))))
          (is (= generation-id (:id (current/current-generation db)))
              "CURRENT still names generation-1 under --no-promote"))))))

;; ----------------------------------------------------------------------------
;; a per-candidate eval failure is collected, not thrown
;; ----------------------------------------------------------------------------

(deftest cycle-collects-a-per-candidate-eval-failure
  (let [{:keys [state-dir db] :as ctx} (provision-evolution-context)
        captured (atom nil)
        overrides (cycle-overrides captured (:replay-case ctx))]
    (testing "a throwing evaluate-candidate! is recorded as per-candidate evidence"
      (with-redefs [eval-core/evaluate-candidate!
                    (fn [_evaluator _cid _profile-id]
                      (throw (ex-info "boom" {:error/type :eval/candidate-not-found})))]
        (let [{:keys [exit data]} (main/execute ["cycle"]
                                                {:state-dir state-dir
                                                 :overrides overrides})]
          (is (= 0 exit) "one failed eval does not abort the cycle")
          (let [evals (:eval (:phases data))]
            (is (= 1 (count evals)))
            (is (= :eval/candidate-not-found
                   (get-in (first evals) [:error :error/type])))
            (is (nil? (seq (:promote (:phases data))))
                "no passing evaluation => no promote entries"))
          (is (= generation-id (:id (current/current-generation db)))
              "CURRENT is untouched when nothing passes"))))))