(ns evoclj.runtime.scheduler-bindings-test
  "WO-B1 — scheduler production wiring for session bindings.

  Two contracts pinned here, both through run-session! (the production
  entry — the fixture model provider stands in for a remote endpoint
  exactly like evoclj.provider.fixture/echo-provider does for tools;
  dispatch, broker, assembler, and binding store are all real):

  1. DEGRADE-WITH-COUNTED-ERROR. fetch-bindings used to swallow ANY
     Throwable and return [] silently. Now a failing bindings query is
     recorded as a typed degradation event (:scheduler/bindings-degraded,
     chained into the session's causal log with sanitized error data)
     before degrading to []. The session still runs (degradation, not
     abort) but the failure is observable and countable — never silent.
  2. RESTORE! PRODUCTION WIRING. store/binding restore! was only ever
     reachable from tests. run-session! now restores the session's
     durable bindings' runtime state (mount/context registries carried
     on the executor's :stores) BEFORE the session leaves :created.
     A pinned binding whose bundle can no longer be verified fails
     closed: typed :store/binding-invalid, no state transition, no
     :session/started event."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.compiler.topology :as topology]
            [evoclj.context.binding :as ctx-binding]
            [evoclj.environment.bundle :as env-bundle]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.surface :as surf]
            [evoclj.intent.dispatch :as dispatch]
            [malli.core :as m]
            [evoclj.mount.backend :as mount-backend]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.sci.boundary :as boundary]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.binding :as binding-store]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util Date)))

;; ---------------------------------------------------------------------------
;; Fixture identity / stores (mirrors scheduler-test)
;; ---------------------------------------------------------------------------

(def ^:private hex64
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private genome-id (str "sha256:" hex64))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype-id (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private generation-id "generation-1")
(def ^:private fixture-model-id "fixture/model")

(def ^:private temp-paths (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-schedb1-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir []
  (let [d (Files/createTempDirectory "evoclj-schedb1-cas-"
                                     (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
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

(defn- fresh-db
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    (doseq [[artifact-id media-type]
            [[genome-id "application/octet-stream"]
             [resolution-id "application/edn"]
             [phenotype-id "application/edn"]]]
      (artifact/ensure-artifact! db artifact-id media-type 0))
    (artifact/ensure-genome! db genome-id)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 0
                     :created_at "2025-01-01T00:00:00Z"}))
    db))

;; ---------------------------------------------------------------------------
;; Deterministic fixture MODEL provider (real Provider protocol instance
;; registered through the kernel-owned model registry atom — the same
;; idiom as provider.fixture for tools; no production component replaced)
;; ---------------------------------------------------------------------------

(def ^:private fixture-model-descriptor
  {:tool/id :model/fixture
   :effect :model-call
   :input-schema [:map {:closed false}
                  [:model/id any?]
                  [:messages [:vector :map]]
                  [:options {:optional true} :map]]
   :output-schema [:map {:closed true}
                   [:model/output [:map {:closed false}
                                   [:text string?]]]
                   [:usage [:map {:closed true}
                            [:model-input-tokens :int]
                            [:model-output-tokens :int]]]]
   :required-action :invoke
   :retry {:safe? true}})

(defn- fixture-model-provider
  "A deterministic in-memory :effect :model-call provider: validates its
  payload against the descriptor's input-schema and returns a fixed
  model output. No network, no keys."
  []
  (reify proto/Provider
    (describe [_] fixture-model-descriptor)
    (normalize-request [_ intent]
      (let [payload (:payload intent)]
        (when-not (boundary/edn-safe? payload)
          (throw (ex-info "model payload not EDN-safe" {:error/type :provider/input-invalid})))
        (when-not (m/validate (:input-schema fixture-model-descriptor) payload)
          (throw (ex-info "model payload failed input-schema"
                          {:error/type :provider/input-invalid})))
        {:tool/id :model/fixture
         :resource {:kind :model :id fixture-model-id}
         :args (select-keys payload [:model/id :messages :options])}))
    (execute-request! [_ _authorized-request]
      {:model/output {:text "the-fixture-model-spoke"}
       :usage {:model-input-tokens 1 :model-output-tokens 2}})))

(defn- fixture-model-registry
  []
  (atom {fixture-model-id {:model/id fixture-model-id
                           :provider (fixture-model-provider)
                           :reason nil
                           :style :openai-compatible
                           :base-url nil}}))

(defn- model-lease
  []
  (let [now (Date.)]
    {:cap/id (random-uuid)
     :principal {:principal/type :session :session/id #uuid "00000000-0000-4000-a000-000000000000"}
     :resource {:kind :model :id "fixture/*"}
     :actions #{:invoke}
     :constraints {:max-calls 10}
     :issued-at now
     :expires-at (Date. (+ (.getTime now) 60000))}))

;; ---------------------------------------------------------------------------
;; Executor assembly
;; ---------------------------------------------------------------------------

(defn- llm-emit-topology
  []
  {:graph/id :graph/schedb1
   :entry :node/llm
   :nodes {:node/llm {:node/type :llm :model :planner :next :node/emit}
           :node/emit {:node/type :emit}}
   :limits {:max-steps 16}})

(defn- compiled-genome
  []
  {:compiled/genome-id genome-id
   :compiled/resolution-id resolution-id
   :compiled/phenotype-id phenotype-id
   :abi {}
   :manifest {}
   :resolution {:models {:planner {:provider-model fixture-model-id}}}
   :topology (topology/compile-topology (llm-emit-topology))
   :programs {}})

(defn- build-executor
  "Executor carrying runtime binding registries (:mount-registry /
  :context-store on :stores) so the B1 restore wiring has production
  state to republish into."
  []
  (let [reg (registry/create-registry)
        usage (atom {})
        lease (model-lease)
        ph (phenotype/instantiate
            (compiled-genome)
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry reg}
             :capabilities {:leases [lease] :usage usage}
             :program-sources {}})
        executor {:phenotype ph
                  :stores {:sqlite (fresh-db)
                           :cas (cas/->cas (str (temp-cas-dir)))
                           :mount-registry (mount-backend/create-registry)
                           :context-store (ctx-binding/create-store)}
                  :dispatch (dispatch/make-broker-context
                             {:registry reg
                              :leases [lease]
                              :usage usage
                              :model-registry (fixture-model-registry)})}]
    executor))

(defn- create-pinned-session
  [executor]
  (let [db (:sqlite (:stores executor))
        sid (:session/id
             (session/create-session!
              db {:genome/id genome-id
                  :resolution/id resolution-id
                  :phenotype/id phenotype-id
                  :generation/id generation-id}))]
    (event/append-event! db {:session/id sid
                             :generation/id generation-id
                             :phenotype/id phenotype-id
                             :event/type :session/created
                             :prev/event-id nil
                             :payload-ref nil
                             :metadata {}})
    sid))

(defn- events-of-type
  [db sid event-type]
  (filter #(= event-type (:event/type %)) (event/events-for-session db sid)))

(defn- make-durable-binding-bundle
  "A skill bundle (context + directory siblings) whose revision exists
  in the given CAS handle."
  [cas-handle logical payload]
  (cas/put-bytes! cas-handle (.getBytes ^String payload StandardCharsets/UTF_8)
                  {:media-type "text/plain"})
  (let [rev (rev/payload->id payload)]
    (env-bundle/make-bundle
     {:bundle-id (str "bundle:" rev ":" (pr-str logical))
      :revision-id rev
      :logical-id logical
      :surfaces [(surf/make-context-surface {:id (keyword (name (first logical)) "-ctx")
                                             :descriptor {:prompt payload}
                                             :materializer identity
                                             :revision/id rev})
                 (surf/make-directory-surface {:id (keyword (name (first logical)) "-dir")
                                               :backend {:type :memory :root "/tmp"}
                                               :access-max #{:read :list :stat}
                                               :revision/id rev})]})))

;; ---------------------------------------------------------------------------
;; 1. Degrade-with-counted-error on bindings query failure
;; ---------------------------------------------------------------------------

(deftest b1-fetch-bindings-degrades-with-typed-count-not-silently
  (let [executor (build-executor)
        db (:sqlite (:stores executor))
        sid (create-pinned-session executor)]
    ;; production-shaped fault: the durable bindings table is missing
    ;; (partial schema / migration drift). Everything else works.
    (jdbc/execute! db ["DROP TABLE session_bindings"])
    (let [result (scheduler/run-session! executor sid {:op :ask :text "hi"})
          degraded (events-of-type db sid :scheduler/bindings-degraded)]
      (testing "session still completes — degradation, not abort"
        (is (= :completed (:status result))))
      (testing "the model really ran through the broker"
        (is (= 1 (count (events-of-type db sid :provider/call-completed)))))
      (testing "the degradation is COUNTED as typed causal events"
        (is (= 2 (count degraded))
            "exactly two markers for the two degraded sites: one at the
             session-start restore wiring, one at the model-round fetch"))
      (testing "the degradation event carries sanitized typed error data"
        (let [meta (:metadata (first degraded))]
          (is (= :bindings-fetch (:degradation meta)))
          (is (string? (get-in meta [:error :error/class]))
              "sanitized error map names the Java class of the failure")
          (is (not (instance? clojure.lang.ExceptionInfo (:error meta)))
              "metadata crosses the boundary as plain data, never a Throwable"))))))

(deftest b1-no-degradation-marker-on-healthy-bindings-query
  (let [executor (build-executor)
        db (:sqlite (:stores executor))
        sid (create-pinned-session executor)
        result (scheduler/run-session! executor sid {:op :ask :text "hi"})]
    (testing "healthy path stays clean"
      (is (= :completed (:status result)))
      (is (zero? (count (events-of-type db sid :scheduler/bindings-degraded)))
          "no degradation noise when nothing degraded"))))

;; ---------------------------------------------------------------------------
;; 2. restore! production wiring at session start
;; ---------------------------------------------------------------------------

(deftest b1-run-session-restores-durable-bindings-before-running
  (let [executor (build-executor)
        db (:sqlite (:stores executor))
        sid (create-pinned-session executor)
        mounts (:mount-registry (:stores executor))
        ctx (:context-store (:stores executor))
        logical [:skill "debugging"]
        bundle (make-durable-binding-bundle (:cas (:stores executor)) logical "skill body")]
    ;; durable binding exists BEFORE the process "restarts" into this run
    (binding-store/activate! db sid bundle {:cas (:cas (:stores executor))})
    (let [result (scheduler/run-session! executor sid {:op :ask :text "hi"})]
      (testing "session completed normally"
        (is (= :completed (:status result))))
      (testing "durable binding's runtime state was restored by run-session! itself"
        (is (= logical (:logical/id (ctx-binding/get-binding ctx logical)))
            "context binding republished into the executor's registry")
        (is (pos? (count (mount-backend/list-mounts mounts)))
            "runtime mounts republished")))))

(deftest b1-run-session-refuses-to-start-on-unverifiable-pinned-binding
  (let [executor (build-executor)
        db (:sqlite (:stores executor))
        sid (create-pinned-session executor)
        ;; activate against a CAS that will NOT be the executor's at run time
        ;; (simulated GC of the pinned artifact)
        ghost-cas (cas/->cas (str (temp-cas-dir)))
        bundle (make-durable-binding-bundle ghost-cas [:skill "gc-victim"] "vanished body")]
    (binding-store/activate! db sid bundle {:cas ghost-cas})
    (let [thrown (try (scheduler/run-session! executor sid {:op :ask :text "hi"}) nil
                      (catch Throwable t t))
          pin (session/get-session db sid)]
      (testing "fail-closed typed refusal"
        (is (some? thrown) "run-session! must not silently run with an unverifiable binding")
        (is (= :store/binding-invalid (:error/type (ex-data thrown)))))
      (testing "the session never left :created"
        (is (= :created (:state pin)))
        (is (zero? (count (events-of-type db sid :session/started)))
            "no :session/started — the refusal happened before any transition")))))

(deftest b1-run-session-without-bindings-is-unaffected-by-wiring
  (let [executor (build-executor)
        sid (create-pinned-session executor)
        result (scheduler/run-session! executor sid {:op :ask :text "hi"})]
    (is (= :completed (:status result)))
    (is (= 1 (count (events-of-type (:sqlite (:stores executor)) sid :session/completed)))
        "zero-binding sessions take the plain path with zero restore overhead events")))
