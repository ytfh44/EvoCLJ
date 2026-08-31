(ns evoclj.kernel.evolution-llm-wiring-test
  "Feature 3 of 3 - the LLM-driven evolution adapters wired into the
  HOST (:evolution/system :model/registry :dispatch :model-lease)
  and driven end to end with a REAL system and a fake
  OpenAI-compatible endpoint.

  FULLY OFFLINE: a local com.sun.net.httpserver.HttpServer serves
  canned chat-completions JSON - a diagnosis payload for a diagnosis
  prompt and a mutation payload for a mutation prompt."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.cli.session :as cli-session]
            [evoclj.evolution.core :as evolution-core]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.kernel.system :as kernel]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [integrant.core :as ig])
  (:import (com.sun.net.httpserver HttpServer HttpHandler)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util Date UUID)))

;; --- fixture identity ---

(def ^:private generation-id "generation-1")

(defn- route-a-root []
  (str (io/file "test" "fixtures" "evolution-e2e" "route-a")))

(defn- route-descriptor []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- evolution-phenotype-id
  "The kernel-deterministic phenotype id the host's :model-call closure
  attributes every evolution model call to (the lease subject must
  match it exactly)."
  []
  (hash/text-digest "evoclj/evolution"))

(defn- model-lease
  "A valid capability lease granting the deterministic evolution
  phenotype the 'lmstudio/*' model resource for the next hour."
  []
  (let [now (Date.)]
    {:cap/id (UUID/randomUUID)
     :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id (evolution-phenotype-id)}
     :resource {:kind :model :id "lmstudio/*"}
     :actions #{:invoke}
     :constraints {:max-calls 100}
     :issued-at now
     :expires-at (Date. (+ (.getTime now) 3600000))}))

;; --- the fake OpenAI-compatible endpoint ---

(defn- diagnosis-response
  "Canned chat-completions body for a diagnosis prompt: one hypothesis
  that validates against the diagnosis schema (a support ref citing the
  seeded episode id)."
  [episode-id]
  (json/generate-string
   {:id "diag-1"
    :choices [{:index 0
               :message {:role "assistant"
                         :content (json/generate-string
                                   {:hypotheses
                                    [{:pattern "task/success"
                                      :claim "task success rate is below threshold"
                                      :support [{:episode-id episode-id :event-ids [2]}]
                                      :counterevidence []
                                      :target-kind "workflow"
                                      :target-id "task"
                                      :effect-metric "task/success"
                                      :effect-direction "increase"
                                      :confidence-band "low"}]})}
                         :finish_reason "stop"}]
    :usage {:prompt_tokens 100 :completion_tokens 50}}))

(defn- mutation-response
  "Canned chat-completions body for a mutation prompt: one
  :replace-text mutation against the mutable programs/route.clj with NO
  :expect/hash (the kernel computes and attaches it)."
  []
  (json/generate-string
   {:id "mut-1"
    :choices [{:index 0
               :message {:role "assistant"
                         :content (json/generate-string
                                   {:mutations
                                    [{:ops
                                      [{:op "replace-text"
                                        :file "programs/route.clj"
                                        :anchor "Class-A requests ({:op :echo-a :text t}) are served by tool A"
                                        :text "Class-A requests ({:op :echo-a :text t}) are served by EVOLVED tool A"}]
                                      :risk "behavioral"
                                      :expected-effect {:primary-metric "task/success"
                                                        :direction "increase"}}]})}
                         :finish_reason "stop"}]
    :usage {:prompt_tokens 200 :completion_tokens 60}}))

(defn- start-fake-endpoint
  "Serve canned chat-completions: a diagnosis payload when the request
  asks about an evidence pack, else a mutation payload. Returns
  {:server :base-url :requests}."
  [episode-id]
  (let [server (HttpServer/create (InetSocketAddress. 0) 0)
        requests (atom [])]
    (.createContext server "/chat/completions"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (let [body (slurp (.getRequestBody exchange))
                              _ (swap! requests conj body)
                              diag? (str/includes? body "evidence pack")
                              resp (if diag?
                                     (diagnosis-response episode-id)
                                     (mutation-response))
                              bytes (.getBytes resp "UTF-8")]
                          (.sendResponseHeaders exchange 200 (count bytes))
                          (with-open [os (.getResponseBody exchange)]
                            (.write os bytes))))))
    (.start server)
    {:server server
     :base-url (str "http://127.0.0.1:" (.getPort (.getAddress server)))
     :requests requests}))

;; --- temp plumbing ---

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
  (doseq [p @temp-paths] (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(def ^:private servers (atom []))
(use-fixtures :each
  (fn [f]
    (reset! servers [])
    (f)
    (doseq [{:keys [server]} @servers] (when server (.stop server 0)))
    (cleanup!)))

;; --- store provisioning ---

(defn- seed-store!
  "Migrate the db and seed a generation-1 row whose genome_id is the
  real route-a fixture address, plus one completed session + episode so
  the evidence pack is non-empty. Returns {:db :genome-id}."
  [db episode-id]
  (migrate/migrate! db)
  (let [loaded (load/load-genome (route-a-root))
        genome-id (:genome/id loaded)
        resolution-id (str "sha256:" (apply str (repeat 64 "f")))]
    (artifact/ensure-artifact! db genome-id "application/octet-stream" 0)
    (artifact/ensure-artifact! db resolution-id "application/edn" 0)
    (artifact/ensure-artifact! db (evolution-phenotype-id) "application/octet-stream" 0)
    (artifact/ensure-genome! db genome-id)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :sessions
                    {:id (str episode-id)
                     :generation_id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :phenotype_id (evolution-phenotype-id)
                     :state "completed"
                     :created_at "2025-01-02T00:00:00Z"})
      (doseq [[id type cause]
              [[1 ":session/created" nil]
               [2 ":session/completed" 1]]]
        (jdbc/insert! conn :events
                      {:id id
                       :session_id (str episode-id)
                       :event_seq id
                       :generation_id generation-id
                       :phenotype_id (evolution-phenotype-id)
                       :event_type type
                       :cause_event_id cause
                       :payload_ref nil
                       :payload "{}"
                       :prev_hash "fixture"
                       :event_hash (str "fixture-" id)
                       :created_at "2025-01-02T00:00:00Z"}))
      (jdbc/insert! conn :episodes
                    {:id (str (random-uuid))
                     :session_id (str episode-id)
                     :generation_id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :task_ref (str "sha256:" (apply str (repeat 64 "0")))
                     :first_event_id 1
                     :last_event_id 2
                     :outcome (pr-str {:status :completed})
                     :usage (pr-str {})
                     :created_at "2025-01-02T00:00:00Z"}))
    {:db db :genome-id genome-id}))

(defn- model-idx-entry
  "The models.dev catalog index entry for the fake lmstudio model."
  [base-url]
  {"lmstudio/fake"
   {:model/id "lmstudio/fake"
    :model/provider :lmstudio
    :model/style :openai-compatible
    :model/status :supported
    :model/base-url base-url
    :model/api-key-env "LMSTUDIO_API_KEY"
    :model/dialect {:interleaved :none :reasoning-options []
                    :server-side-search :off :extra-params {}}
    :model/cost {:input 0 :output 0}}})

(defn- offline-model-registry
  "Build the top-level :model/registry component config from injected
  catalog data (no models.dev fetch): replaces the base config's
  #ig/ref to :modelsdev/catalog with a plain catalog-data map."
  [base-url]
  {:catalog {:catalog/data {:catalog/models (model-idx-entry base-url)}}
   :registry/api-keys {:lmstudio "lm-studio"}})

(defn- offline-config
  "Take the CLI-style config and remove the network-facing models.dev
  catalog: dissoc :modelsdev/catalog, and fully replace the top-level
  :model/registry component with `model-registry` (the injected
  catalog data + api keys), or a bare empty registry when nil."
  [cfg model-registry]
  (-> cfg
      (dissoc :modelsdev/catalog)
      (assoc :model/registry (or model-registry {:registry/api-keys {}}))))

(defn- host-config
  "Build the CLI-style host config with the LLM evolution wiring
  fully offline. The :diagnostician / :mutator :llm maps are assoc'd in
  AFTER build-config so they REPLACE the base pattern/:none configs
  (build-config's deep-merge would otherwise merge the pattern keys
  into an :llm map, which the strict LLM-config gate rejects)."
  [state-dir base-url lease]
  (let [model-registry (offline-model-registry base-url)
        cfg (cli-session/build-config
             {:state-dir state-dir
              :overrides
              {:capability/broker
               {:registry (ig/ref :provider/registry) :leases []}}})]
    (-> cfg
        (offline-config model-registry)
        (assoc-in [:evolution/system :genome-root] (route-a-root))
        (assoc-in [:evolution/system :model-lease] lease)
        (assoc-in [:evolution/system :model/registry] (ig/ref :model/registry))
        (assoc-in [:evolution/system :dispatch] (ig/ref :capability/broker))
        (assoc-in [:evolution/system :diagnostician]
                  {:type :llm :model/id "lmstudio/fake" :max-hypotheses 2})
        (assoc-in [:evolution/system :mutator]
                  {:type :llm :model/id "lmstudio/fake" :max-mutations 1}))))

(defn- ensure-dirs! [state-dir]
  (doseq [d [(str state-dir "/db")
             (str state-dir "/candidates")]]
    (Files/createDirectories (Paths/get d (make-array String 0))
                             (make-array FileAttribute 0)))
  state-dir)

(defn- evolution-error
  "Walk a thrown Throwable chain — Integrant wraps build failures in
  :integrant.core/build-threw-exception — and return the first ex-data
  map carrying :error/type :evolution/system-invalid, or nil."
  [t]
  (loop [t t]
    (when t
      (let [d (ex-data t)]
        (if (= :evolution/system-invalid (:error/type d))
          d
          (let [c (.getCause ^Throwable t)]
            (recur c)))))))

;; --- the actual tests ---

(deftest llm-evolution-wires-through-the-real-host
  (let [episode-id (str (UUID/randomUUID))
        {:keys [server base-url requests]} (start-fake-endpoint episode-id)
        _ (swap! servers conj server)
        state-dir (ensure-dirs! (temp-dir "evoclj-llm-evo-"))
        db-path (str state-dir "/db/evoclj.db")
        db (sqlite/spec db-path)
        {:keys [genome-id]} (seed-store! db episode-id)
        lease (model-lease)
        system (kernel/init (host-config state-dir base-url lease))
        result (evolution-core/propose-candidates!
                (:evolution/system system)
                {:generation/id generation-id
                 :evidence-selector {:recent 1 :include-successes 1
                                     :include-failures 1 :include-high-cost 1}
                 :max-candidates 3})]
    (testing "the fake endpoint was hit at least twice"
      (is (>= (count @requests) 2)
          (str "expected >=2 model calls, got " (count @requests)))
      (let [messages (mapv #(get-in (json/parse-string % true) [:messages]) @requests)
            diag? (boolean (some (fn [ms] (some #(str/includes? (or (:content %) "") "evidence pack") ms)) messages))
            mut?  (boolean (some (fn [ms] (some #(str/includes? (or (:content %) "") "mutations") ms)) messages))]
        (is diag? "a diagnosis prompt reached the endpoint")
        (is mut? "a mutation prompt reached the endpoint")))
    (testing "at least one Candidate row was persisted"
      (is (seq result))
      (is (= :evaluation-pending (:state (first result))))
      (is (= genome-id (:parent/genome-id (first result))))
      (is (= 1 (count (sqlite/query db ["SELECT * FROM candidates"])))))
    (testing "the kernel-computes-hash security property"
      (let [parent (load/load-genome (route-a-root))
            expected (get-in parent [:files "programs/route.clj" :digest])
            mut-row (first (sqlite/query db ["SELECT * FROM mutations"]))
            ops (clojure.edn/read-string (:ops mut-row))
            op (first ops)]
        (is (some? (get @requests 0)) "requests were recorded")
        (is (= "programs/route.clj" (:file op)))
        (is (= expected (:expect/hash op))))
      (let [c (first result)]
        (is (not= (:parent/genome-id c) (:candidate/genome-id c)))))))

(deftest llm-adapters-fail-closed-without-model-registry
  (let [state-dir (ensure-dirs! (temp-dir "evoclj-llm-fail-"))
        cfg (-> (cli-session/build-config
                 {:state-dir state-dir
                  :overrides
                  {:evolution/system
                   {:diagnostician {:type :llm :model/id "lmstudio/fake"}
                    :mutator {:type :llm :model/id "lmstudio/fake"}}}})
                (offline-config nil)
                (assoc-in [:evolution/system :diagnostician]
                          {:type :llm :model/id "lmstudio/fake"})
                (assoc-in [:evolution/system :mutator]
                          {:type :llm :model/id "lmstudio/fake"}))
        thrown (try (kernel/init cfg) nil
                    (catch Throwable t t))]
    (let [d (evolution-error thrown)]
      (is (some? d))
      (is (= :llm-needs-model-registry (:reason d))))))

(deftest llm-configuration-errors-fail-closed
  (testing "an :llm diagnostician with an unknown key is rejected"
    (let [state-dir (ensure-dirs! (temp-dir "evoclj-llm-bad-"))
          model-registry (offline-model-registry "http://127.0.0.1:1")
          cfg (-> (cli-session/build-config
                   {:state-dir state-dir
                    :overrides
                    {:evolution/system
                     {:model/registry (ig/ref :model/registry)
                      :dispatch (ig/ref :capability/broker)}}})
                  (offline-config model-registry)
                  (assoc-in [:evolution/system :diagnostician]
                            {:type :llm :model/id "lmstudio/fake"
                             :bogus :nope}))
          thrown (try (kernel/init cfg) nil
                      (catch Throwable t t))]
      (let [d (evolution-error thrown)]
        (is (some? d))
        (is (= :llm-config-invalid (:reason d))
            "an unknown key inside a :type :llm diagnostician map fails closed")))))
