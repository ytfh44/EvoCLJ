(ns evoclj.adversarial.authority-test
  "component — Self-authority escalation suite (adversarial release gate).

  This suite ATTACKS the system's own architectural claims. Every case
  must FAIL CLOSED before any protected effect occurs:

  Step 1 — the closed SCI context denies every ambient-authority form
  (System/getenv, java.io.File, Runtime.exec/ProcessBuilder, slurp,
  spit, load-file, eval) at analysis/eval time, so malicious programs
  never run these forms; the hostile Genome is rejected by the static
  compile gate; capability requests beyond the host grant are denied
  by evoclj.capability.broker (reused here); and a phenotype cannot
  reuse another phenotype's capability ID (subject matching is EXACT,
  Global Constraint 9).

  Step 2 — a denial is visible in the append-only audit log: the
  scheduler persists :intent/denied chained to :intent/proposed,
  carrying the broker's :reason code, and the denial was decided on
  the NORMALIZED (canonical) resource, never the raw request.

  Step 3 — a denial does NOT automatically grant a fallback broader
  tool: re-dispatching the same intent is denied again with the same
  reason, no new lease appears, and no call budget is consumed.

  Malicious fixtures live under test/fixtures/adversarial/authority/:

  - sci-hostile/         a Genome whose program carries System/getenv,
                         java.io.File, Runtime/getRuntime,
                         ProcessBuilder, slurp, spit, load-file, eval
  - network-capability/  a Genome whose manifest REQUESTS a network
                         capability (:capability/network) the host
                         never grants; its program emits :net/fetch
  - filesystem-escalation/ a Genome whose program DEMANDS the
                         filesystem root (\"/\"), broader than the host
                         grant of /protected/work
  - child-extension/     a successor Genome that reuses the parent's
                         capability REQUEST (:tool/call) and tries to
                         run with the parent Phenotype's capability ID"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.capability.broker :as broker]
            [evoclj.compiler.core :as core]
            [evoclj.genome.load :as load]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.sci.context :as sci-ctx]
            [evoclj.sci.execute :as execute]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]
            [sci.core :as sci])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; ============================================================================
;; shared fixtures
;; ============================================================================

(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private cause-event-id 42)
(def ^:private budget {:wall-ms 1000})
(def ^:private generation-id "generation-1")
(def ^:private authority-fixture-root "test/fixtures/adversarial/authority")

(def ^:private parent-cap-id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def ^:private child-cap-id #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")

(def ^:private pid-pattern #"^sha256:[0-9a-f]{64}$")

(defn- fixture-catalog
  "The on-disk provider catalog fixture (component Resolution)."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- loaded-genome
  "Load one malicious fixture bundle from
  test/fixtures/adversarial/authority/<name> with its in-memory program
  registry attached (component choice (a))."
  [name descriptor]
  (assoc (load/load-genome (str authority-fixture-root "/" name))
         :programs [descriptor]))

(defn- route-descriptor
  "A route program descriptor whose :file is programs/route.clj."
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- hostile-descriptor
  "The hostile program descriptor (programs/hostile.clj)."
  []
  {:program/id :program/hostile
   :file "programs/hostile.clj"
   :entry 'agent.hostile/run
   :input-schema :schema/any
   :output-schema :schema/any})

(defn- compiled-fixture
  "Load + compile one malicious fixture genome."
  [name]
  (core/compile-genome (loaded-genome name (route-descriptor))
                       (fixture-catalog)))

(defn- hostile-loaded
  "The hostile fixture loaded (not compiled — compilation must fail)."
  []
  (loaded-genome "sci-hostile" (hostile-descriptor)))

(defn- hostile-source
  "The hostile program's source text, decoded from the immutable bundle."
  []
  (String. ^bytes (byte-array
                   (get-in (hostile-loaded) [:files "programs/hostile.clj" :bytes]))
           StandardCharsets/UTF_8))

(defn- seed-loaded
  "The REAL seed Genome (the parent Phenotype the child extension tries
  to reuse capability from)."
  []
  (assoc (load/load-genome "genomes/seed")
         :programs [(route-descriptor)]))

(defn- program-sources
  "Decode every compiled program's source text from the immutable
  loaded bundle :files (the CompiledGenome carries only :source/digest
  references, Global Constraint 22)."
  [loaded compiled]
  (into {}
        (map (fn [[program-id descriptor]]
               [program-id
                (String. ^bytes (byte-array
                                 (get-in loaded
                                         [:files (:file descriptor) :bytes]))
                        StandardCharsets/UTF_8)]))
        (:programs compiled)))

;; --- temp stores (test temp dirs only) -------------------------------------

(def ^:private temp-paths (atom []))
(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-authority-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-authority-cas-"
                                     (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- delete-tree!
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup!
  []
  (doseq [p @temp-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

;; Every store-owning test registers its temp db/cas paths and is
;; cleaned up after itself, so the suite leaves no scratch dirs behind.
(use-fixtures :each (fn [f] (f) (cleanup!)))

;; --- store / executor assembly ---------------------------------------------

(defn- fresh-db
  "A migrated database backed by a fresh temp file, seeded with the
  generation row sessions are pinned to (current = 1) and all compiled
  identity rows required by session foreign keys."
  [genome-id resolution-id phenotype-id]
  (let [path (temp-db-path)
        db (sqlite/spec path)]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (doseq [[artifact-id media-type]
              [[genome-id "application/octet-stream"]
               [resolution-id "application/edn"]
               [phenotype-id "application/edn"]]]
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
    [db path]))

(defn- make-executor
  "Assemble the component executor map for a compiled genome:

    {:phenotype <instantiated Phenotype>
     :stores {:sqlite <migrated db> :cas <CAS root>}
     :dispatch <broker context carrying exactly `leases`>}

  `loaded` is the loaded Genome the compiled value was compiled from
  (with its :programs registry attached), so program sources are
  decoded from the immutable bundle. The provider `registry` is passed
  through; `usage` is the broker's per-:cap/id call-count atom. Returns
  {:executor ... :usage ... :db ... :db-path ... :cas-root ...}."
  [compiled loaded registry leases usage]
  (let [[db db-path] (fresh-db (:compiled/genome-id compiled)
                               (:compiled/resolution-id compiled)
                               (:compiled/phenotype-id compiled))
        cas-root (temp-cas-dir)
        ph (phenotype/instantiate
            compiled
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry registry}
             :capabilities {:leases leases :usage usage}
             :program-sources (program-sources loaded compiled)})]
    {:executor {:phenotype ph
                :stores {:sqlite db :cas (cas/->cas cas-root)}
                :dispatch (dispatch/make-broker-context
                           {:registry registry :leases leases :usage usage})}
     :usage usage
     :db db
     :db-path db-path
     :cas-root cas-root}))

(defn- create-pinned-session
  "create-session! pinned to the compiled identity, then append the
  :session/created root event (the host's job). Returns the session id."
  [executor compiled]
  (let [db (:sqlite (:stores executor))
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
    sid))

;; --- capability lease helpers ----------------------------------------------

(defn- tool-lease
  "A valid :invoke lease granting ONE phenotype the given tool."
  [cap-id subject-pid tool-id]
  (let [now (java.util.Date.)]
    {:cap/id cap-id
     :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id subject-pid}
     :resource {:kind :tool :id tool-id}
     :actions #{:invoke}
     :constraints {:max-calls 10}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(defn- fs-lease
  "A valid :invoke lease granting ONE phenotype filesystem scope under
  `path` (the host's actual grant — narrower than what an attacker
  requests)."
  [cap-id subject-pid path]
  (let [now (java.util.Date.)]
    {:cap/id cap-id
     :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id subject-pid}
     :resource {:kind :filesystem :path path}
     :actions #{:invoke}
     :constraints {:max-calls 10}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

;; --- the inline network provider the host never grants ----------------------

(defn- net-fetch-provider
  "An inline REGISTERED :net/fetch provider — visible and requestable,
  but the host NEVER issues it a lease. :execution-count proves a
  denied request never reaches execute-request!."
  [& {:keys [execution-count] :or {execution-count (atom 0)}}]
  (let [descriptor {:tool/id :net/fetch
                    :effect :pure
                    :input-schema [:map [:url :string]]
                    :output-schema [:map [:status int?]]
                    :required-action :invoke}]
    (reify proto/Provider
      (describe [_] descriptor)
      (normalize-request [_ intent]
        (let [args (get-in intent [:payload :args])]
          (when-not (and (map? args) (string? (:url args)))
            (throw (err/error :provider/input-invalid
                              "net/fetch requires {:url <string>}"
                              {:value args})))
          {:tool/id :net/fetch
           :resource {:kind :tool :id :net/fetch}
           :args args}))
      (execute-request! [_ authorized-request]
        (swap! execution-count inc)
        {:status 200}))))

;; --- small helpers ----------------------------------------------------------

(defn- tool-call-intent
  "A validated :intent/tool-call for one tool from `pid`."
  [pid tool-id args]
  (intent/tool-call session-id pid :node/tool cause-event-id
                    {:tool/id tool-id :args args} budget))

(defn- normalized-tool-request
  "The canonical normalized request for a :tool resource (the exact
  form provider normalize-request returns, component)."
  [tool-id args]
  {:tool/id tool-id
   :resource {:kind :tool :id tool-id}
   :args args})

(defn- denied-event
  "The :intent/denied event of a session's event log, or nil."
  [events]
  (first (filter #(= :intent/denied (:event/type %)) events)))

(defn- throws-with-type?
  "True when (f) throws ExceptionInfo carrying :error/type t."
  [f t]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e
         (= t (:error/type (ex-data e))))))

;; ============================================================================
;; STEP 1 — every case fails closed before any protected effect occurs
;; ============================================================================

(def ^:private ambient-forms
  "The eight ambient-authority attempts an evolvable program may make.
  Each one must throw inside the closed SCI context."
  [[:system/getenv "(System/getenv \"HOME\")"]
   [:java/io-file "(java.io.File. \"/etc/passwd\")"]
   [:runtime/exec "(Runtime/getRuntime)"]
   [:process-builder "(new java.lang.ProcessBuilder [\"ls\"])"]
   [:slurp "(slurp \"/etc/passwd\")"]
   [:spit "(spit \"/tmp/evoclj-pwned\" \"x\")"]
   [:load-file "(load-file \"/tmp/evoclj-x.clj\")"]
   [:eval "(eval 1)"]])

(deftest closed-sci-context-denies-ambient-forms
  (testing "every ambient form throws inside the closed SCI context"
    (let [ctx (sci-ctx/make-context {})]
      (doseq [[label form] ambient-forms]
        (testing (str label)
          (is (thrown? Throwable (sci/eval-string* ctx form))
              (str label " must never evaluate — fail closed before any effect"))))))
  (testing "a hostile source is denied as a whole, not just one-off forms"
    (let [ctx (sci-ctx/make-context {})]
      (is (thrown? Throwable (sci-ctx/run-form ctx (hostile-source)
                                               'agent.hostile/run {}))
          "run-form on the hostile program never runs it")
      (is (thrown? Throwable
                   (execute/load-program! ctx (hostile-descriptor)
                                          (hostile-source)))
          "load-program! never installs a hostile program into the sandbox")))
  (testing "the protected effect really never occurred — spit never wrote"
    (let [p (str (Files/createTempFile "evoclj-spit-" ".pwned"
                                       (make-array FileAttribute 0)))
          _ (Files/deleteIfExists (Paths/get p (make-array String 0)))
          ctx (sci-ctx/make-context {})]
      (is (thrown? Throwable
                   (sci/eval-string* ctx (format "(spit %s \"pwned\")"
                                                 (pr-str p)))))
      (is (not (Files/exists (Paths/get p (make-array String 0))
                               (make-array LinkOption 0)))
          "the target file was never created — the denial preceded any effect"))))

(deftest hostile-genome-fails-the-static-compile-gate
  (testing "the hostile Genome never compiles — the static policy gate
            rejects it before any program could run"
    (is (throws-with-type?
         #(core/compile-genome (hostile-loaded) (fixture-catalog))
         :program/policy-violation))))

(deftest network-capability-beyond-host-grant-is-denied
  (let [compiled (compiled-fixture "network-capability")
        pid (:compiled/phenotype-id compiled)]
    (testing "the request compiles — a manifest capability REQUEST is a
              declaration, never authority (Global Constraint 9)"
      (is (re-matches pid-pattern pid)))
    (let [net-intent (tool-call-intent pid :net/fetch {:url "https://example.com"})
          normalized (normalized-tool-request :net/fetch {:url "https://example.com"})
          reg (registry/create-registry)]
      (testing "registering and exposing :net/fetch never grants it"
        (registry/register! reg (net-fetch-provider))
        (is (some? (registry/lookup reg :net/fetch))))
      (testing "the host never grants :capability/network -> deny :capability/missing"
        (let [d (broker/authorize {:intent net-intent
                                   :normalized-request normalized
                                   :leases []
                                   :usage {}
                                   :now (java.util.Date.)})]
          (is (= {:decision :deny :reason :capability/missing} d))
          (is (= d (edn/read-string (pr-str d)))
              "the deny decision is plain EDN data (Global Constraint 22)")))
      (testing "a lease for ANY other tool is not a fallback for the network request"
        (let [leases [(tool-lease parent-cap-id pid :fixture/echo)]]
          (is (= :capability/scope-denied
                 (:reason (broker/authorize {:intent net-intent
                                             :normalized-request normalized
                                             :leases leases
                                             :usage {}
                                             :now (java.util.Date.)})))))))))

(deftest broader-filesystem-scope-than-host-grant-is-denied
  (let [compiled (compiled-fixture "filesystem-escalation")
        pid (:compiled/phenotype-id compiled)
        host-grant (fs-lease child-cap-id pid "/protected/work")
        root-intent (tool-call-intent pid :fixture/path-resolve {:path "/"})
        root-request {:tool/id :fixture/path-resolve
                      :resource {:kind :filesystem :path "/"}
                      :args {:path "/"}}
        esc-intent (tool-call-intent pid :fixture/path-resolve {:path "/etc/passwd"})
        esc-request {:tool/id :fixture/path-resolve
                     :resource {:kind :filesystem :path "/etc/passwd"}
                     :args {:path "/etc/passwd"}}
        in-scope-intent (tool-call-intent pid :fixture/path-resolve {:path "x"})
        in-scope-request {:tool/id :fixture/path-resolve
                          :resource {:kind :filesystem :path "/protected/work/x"}
                          :args {:path "x"}}]
    (testing "resource normalization precedes capability matching — the
              canonical resource is decided on, never the raw request"
      (let [provider (fixture/path-resolve-provider {:root "/protected/work"})]
        (is (= {:kind :filesystem :path "/"}
               (:resource (proto/normalize-request provider root-intent)))
            "a request for the filesystem ROOT normalizes to the canonical \"/\"")
        (is (= {:kind :filesystem :path "/etc/passwd"}
               (:resource (proto/normalize-request provider esc-intent)))
            "an absolute escape stays absolute and is never re-rooted")
        (is (= {:kind :filesystem :path "/protected/work/x"}
               (:resource (proto/normalize-request provider in-scope-intent)))
            "a request INSIDE the grant normalizes to the granted root")))
    (testing "a request broader than the host grant is denied :capability/scope-denied"
      (is (= :capability/scope-denied
             (:reason (broker/authorize {:intent root-intent
                                         :normalized-request root-request
                                         :leases [host-grant]
                                         :usage {}
                                         :now (java.util.Date.)}))))
      (is (= :capability/scope-denied
             (:reason (broker/authorize {:intent esc-intent
                                         :normalized-request esc-request
                                         :leases [host-grant]
                                         :usage {}
                                         :now (java.util.Date.)})))))
    (testing "a request INSIDE the host's grant is allowed (positive control)"
      (let [d (broker/authorize {:intent in-scope-intent
                                 :normalized-request in-scope-request
                                 :leases [host-grant]
                                 :usage {}
                                 :now (java.util.Date.)})]
        (is (= :allow (:decision d)))
        (is (= child-cap-id (:lease-id d)))))))

(deftest capability-id-reuse-by-child-extension-is-denied
  (let [parent (core/compile-genome (seed-loaded) (fixture-catalog))
        child (compiled-fixture "child-extension")
        parent-pid (:compiled/phenotype-id parent)
        child-pid (:compiled/phenotype-id child)]
    (testing "the child/future extension is a DIFFERENT phenotype from its parent"
      (is (re-matches pid-pattern parent-pid))
      (is (re-matches pid-pattern child-pid))
      (is (not= parent-pid child-pid)))
    (let [parent-lease (tool-lease parent-cap-id parent-pid :fixture/echo)
          child-intent (tool-call-intent child-pid :fixture/echo {:text "reuse"})
          parent-intent (tool-call-intent parent-pid :fixture/echo {:text "own"})
          request (normalized-tool-request :fixture/echo {:text "reuse"})]
      (testing "the child's re-dispatch with the parent's capability ID is
                denied — subject matching is EXACT (Global Constraint 9)"
        (is (= :capability/principal-mismatch
               (:reason (broker/authorize {:intent child-intent
                                           :normalized-request request
                                           :leases [parent-lease]
                                           :usage {}
                                           :now (java.util.Date.)}))))
        (testing "even a lease for the SAME tool and SAME request shape
                  does not transfer — the grant binds ONE phenotype"
          (is (= :capability/principal-mismatch
                 (:reason (broker/authorize {:intent child-intent
                                             :normalized-request request
                                             :leases [(tool-lease child-cap-id
                                                                 parent-pid
                                                                 :fixture/echo)]
                                             :usage {}
                                             :now (java.util.Date.)}))))))
      (testing "the parent's OWN dispatch with the same capability is allowed
                (positive control — the lease is real, just not reusable)"
        (let [d (broker/authorize {:intent parent-intent
                                   :normalized-request request
                                   :leases [parent-lease]
                                   :usage {}
                                   :now (java.util.Date.)})]
          (is (= :allow (:decision d)))
          (is (= parent-cap-id (:lease-id d))))))))

;; ============================================================================
;; STEP 2 — denial is visible in audit events with normalized resource/reason
;; ============================================================================

(deftest denials-are-audited-with-reason-and-normalized-resource
  (testing "a requested capability with no host grant fails the static lattice gate"
    (let [executions (atom 0)
          compiled (compiled-fixture "network-capability")
          reg (registry/create-registry)
          _ (registry/register! reg (net-fetch-provider {:execution-count executions}))
          usage (atom {})
          {:keys [executor db]} (make-executor compiled
                                               (loaded-genome "network-capability"
                                                              (route-descriptor))
                                               reg [] usage)
          sid (create-pinned-session executor compiled)
          error (try
                  (scheduler/run-session! executor sid {})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))
          events (event/events-for-session db sid)]
      (is (= :capability/lattice-invalid (:error/type error)))
      (is (= :requested-not-granted (:reason error)))
      (is (= 0 @executions)
          "the denied :net/fetch request never reached the provider")
      (is (= :created (:state (session/get-session db sid)))
          "the static gate rejects before the session leaves :created")
      (is (= [:session/created] (mapv :event/type events)))
      (is (:valid? (event/verify-event-chain db sid)))))
  (testing "filesystem scope broader than the host grant -> :intent/denied
            :capability/scope-denied (decided on the NORMALIZED resource)"
    (let [executions (atom 0)
          compiled (compiled-fixture "filesystem-escalation")
          pid (:compiled/phenotype-id compiled)
          reg (registry/create-registry)
          _ (registry/register! reg
                                (fixture/path-resolve-provider
                                 {:root "/protected/work"
                                  :execution-count executions}))
          usage (atom {})
          {:keys [executor db]} (make-executor compiled
                                               (loaded-genome "filesystem-escalation"
                                                              (route-descriptor))
                                               reg
                                               [(tool-lease (random-uuid)
                                                            pid
                                                            :fixture/path-resolve)
                                                (fs-lease child-cap-id
                                                          pid "/protected/work")]
                                               usage)
          sid (create-pinned-session executor compiled)
          result (scheduler/run-session! executor sid {})
          events (event/events-for-session db sid)
          denied (denied-event events)]
      (is (= :completed (:status result)))
      (is (= 0 @executions)
          "the path-resolve provider never ran for the over-broad request")
      (is (= :capability/scope-denied (get-in denied [:metadata :reason]))
          "scope-denial means the decision was made against the canonical
          resource (the root \"/\" normalizes outside /protected/work)")
      (is (= :capability/denied (get-in denied [:metadata :error/type])))
      (is (:valid? (event/verify-event-chain db sid)))
      (is (= {} @usage))))
  (testing "child reusing the parent's capability ID -> :intent/denied
            :capability/principal-mismatch (EXACT subject matching)"
    (let [executions (atom 0)
          parent (core/compile-genome (seed-loaded) (fixture-catalog))
          child (compiled-fixture "child-extension")
          reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider
                                     {:execution-count executions}))
          usage (atom {})
          {:keys [executor db]} (make-executor child
                                               (loaded-genome "child-extension"
                                                              (route-descriptor))
                                               reg
                                               [(tool-lease parent-cap-id
                                                            (:compiled/phenotype-id parent)
                                                            :fixture/echo)]
                                               usage)
          sid (create-pinned-session executor child)
          result (scheduler/run-session! executor sid {})
          events (event/events-for-session db sid)
          denied (denied-event events)]
      (is (= :completed (:status result)))
      (is (= 0 @executions)
          "the child's reuse attempt never ran the echo provider")
      (is (= :capability/principal-mismatch (get-in denied [:metadata :reason]))
          "the audit names the exact-match failure — the grant binds the parent only")
      (is (= :capability/denied (get-in denied [:metadata :error/type])))
      (is (:valid? (event/verify-event-chain db sid)))
      (is (= {} @usage)))))

;; ============================================================================
;; STEP 3 — a denial does NOT automatically grant a fallback broader tool
;; ============================================================================

(deftest denial-does-not-grant-a-fallback-broader-tool
  (testing "re-dispatching the same intent after a denial is denied again
            with the same reason, and no lease/budget appears"
    (let [executions (atom 0)
          reg (registry/create-registry)
          _ (registry/register! reg (net-fetch-provider {:execution-count executions}))
          _ (registry/register! reg (fixture/echo-provider
                                     {:execution-count executions}))
          usage (atom {})
          leases []                                  ; the host grants nothing
          broker-ctx (dispatch/make-broker-context
                      {:registry reg :leases leases :usage usage})
          net-intent (tool-call-intent (str "sha256:" (apply str (repeat 64 \a)))
                                       :net/fetch {:url "https://example.com"})]
      (let [r1 (dispatch/dispatch! broker-ctx net-intent)
            r2 (dispatch/dispatch! broker-ctx net-intent)]
        (is (= :capability/denied (:error/type r1)))
        (is (= :capability/denied (:error/type r2)))
        (is (= :capability/missing (get-in r1 [:error/data :reason])))
        (is (= (:error/data r1) (:error/data r2))
            "the second dispatch is denied with the SAME reason")
        (is (= 0 @executions)
            "no fallback provider ever ran")
        (testing "no new lease appeared and no call budget was consumed"
          (is (= [] leases)
              "the lease collection is byte-for-byte unchanged — the broker
              issued nothing")
          (is (= {} @usage)
              "a denied request never consumes a lease's call budget"))
        (testing "a request for a DIFFERENT tool after the denial is still
                  denied — no broader fallback was auto-granted"
          (let [r3 (dispatch/dispatch!
                    broker-ctx
                    (tool-call-intent (str "sha256:" (apply str (repeat 64 \a)))
                                      :fixture/echo {:text "fallback"}))]
            (is (= :capability/denied (:error/type r3)))
            (is (= :capability/missing (get-in r3 [:error/data :reason]))))))))
    (testing "at the pure broker level the decision is a pure function:
              the same leases yield the same deny, and removing leases can
              never turn it into an allow"
      (let [pid (str "sha256:" (apply str (repeat 64 \b)))
            other-pid (str "sha256:" (apply str (repeat 64 \c)))
            net-intent (tool-call-intent pid :net/fetch {:url "https://x"})
            request (normalized-tool-request :net/fetch {:url "https://x"})
            ;; grants bound to ANOTHER phenotype: each fails the EXACT
            ;; subject check first, so the deterministic reason is
            ;; :capability/principal-mismatch for every non-empty subset
            non-covering [(tool-lease child-cap-id other-pid :fixture/echo)
                          (fs-lease parent-cap-id other-pid "/protected/work")]
            d1 (broker/authorize {:intent net-intent :normalized-request request
                                  :leases non-covering :usage {}
                                  :now (java.util.Date.)})
            d2 (broker/authorize {:intent net-intent :normalized-request request
                                  :leases non-covering :usage {}
                                  :now (java.util.Date.)})]
        (is (= {:decision :deny :reason :capability/principal-mismatch} d1))
        (is (= d1 d2)
            "re-authorizing the same request against the same grants is
            identical — the decision never grows a grant")
        (testing "removing grants can never turn the deny into an allow"
          (doseq [s [[] [(tool-lease child-cap-id other-pid :fixture/echo)]
                   [(fs-lease parent-cap-id other-pid "/protected/work")]]]
            (let [ds (broker/authorize {:intent net-intent
                                        :normalized-request request
                                        :leases s :usage {}
                                        :now (java.util.Date.)})]
              (is (= :deny (:decision ds))
                  (str "subset must deny: " (pr-str s)))
              (is (contains? #{:capability/missing :capability/principal-mismatch}
                             (:reason ds))
                  (str "deny reason is deterministic: " (pr-str s)))))))))