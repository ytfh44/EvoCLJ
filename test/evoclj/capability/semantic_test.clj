(ns evoclj.capability.semantic-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.binding.call :as binding]
            [evoclj.capability.broker :as broker]
            [evoclj.capability.semantic :as semantic]
            [evoclj.intent.pipeline :as pipeline]
            [evoclj.runtime.scheduler :as scheduler]))

(def ^:private intent
  {:intent/id "intent-1"
   :metadata {:idempotency/key "idempotency-1"}})

(def ^:private gmail-descriptor
  {:tool/id :gmail.send
   :effect :remote
   :semantic/validation :required
   :semantic/mapping-version semantic/mapping-version
   :semantic/spec (semantic/mapping-for :gmail.send)
   :input-schema {}
   :output-schema {}
   :required-action :invoke})

(def ^:private session-id #uuid "00000000-0000-0000-0000-000000000010")
(def ^:private capability-id #uuid "00000000-0000-0000-0000-000000000011")
(def ^:private semantic-intent
  {:intent/id #uuid "00000000-0000-0000-0000-000000000012"
   :intent/type :intent/tool-call
   :session/id session-id
   :phenotype/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
   :node/id :node/tool
   :cause/event-id 1
   :payload {:tool/id :gmail.send :args {}}
   :budget {:wall-ms 1000}
   :metadata {}})

(defn- semantic-lease [spec]
  {:cap/id capability-id
   :principal {:principal/type :session :session/id session-id}
   :resource {:kind :tool :id :gmail.send
              :semantic/spec spec}
   :actions #{:invoke}
   :constraints {}
   :issued-at #inst "2025-01-01T00:00:00.000-00:00"
   :expires-at #inst "2025-01-02T00:00:00.000-00:00"})

(defn- semantic-request [spec]
  {:resource {:kind :tool :id :gmail.send
              :semantic/spec spec
              :semantic/digest (semantic/spec-digest spec)}})
(deftest semantic-broker-enforces-subsumption
  (let [external (semantic/mapping-for :gmail.send)
        public (semantic/mapping-for :twitter.post)
        request (semantic-request external)
        allow (broker/authorize {:intent semantic-intent
                                 :normalized-request request
                                 :leases [(semantic-lease public)]
                                 :usage {}
                                 :now #inst "2025-01-01T12:00:00.000-00:00"})
        deny (broker/authorize {:intent semantic-intent
                                :normalized-request (semantic-request public)
                                :leases [(semantic-lease external)]
                                :usage {}
                                :now #inst "2025-01-01T12:00:00.000-00:00"})]
    (is (= :allow (:decision allow)))
    (is (= capability-id (:lease-id allow)))
    (is (= {:decision :deny :reason :capability/semantic-denied} deny))))
(deftest canonical-mappings-are-closed-and-versioned
  (testing "known provider ids resolve only through the static registry"
    (is (= {:semantic/version 1
            :semantic/effect :communication/send
            :semantic/risk :external}
           (semantic/semantic-spec-for gmail-descriptor)))
    (is (= :public (:semantic/risk (semantic/semantic-spec-for
                                    (assoc gmail-descriptor :tool/id :twitter.post
                                           :semantic/spec (semantic/mapping-for :twitter.post)))))))
  (testing "unknown and stale semantic data fail closed"
    (is (thrown? clojure.lang.ExceptionInfo
                 (semantic/semantic-spec-for
                  (assoc gmail-descriptor :tool/id :unknown.send))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (semantic/semantic-spec-for
                  (assoc gmail-descriptor :semantic/mapping-version 0))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (semantic/normalize-spec
                  {:semantic/version 1
                   :semantic/effect :communication/send
                   :semantic/risk :external
                   :semantic/extra true})))))

(deftest semantic-subsumption-is-monotone
  (let [internal {:semantic/version 1 :semantic/effect :communication/send :semantic/risk :internal}
        external {:semantic/version 1 :semantic/effect :communication/send :semantic/risk :external}
        public {:semantic/version 1 :semantic/effect :communication/send :semantic/risk :public}]
    (is (semantic/subsumes? public external))
    (is (semantic/subsumes? external internal))
    (is (not (semantic/subsumes? internal external)))
    (is (not (semantic/subsumes? public
                                 (assoc external :semantic/effect :unknown/send))))
    (is (not (semantic/subsumes? public (assoc public :semantic/version 0))))))

(deftest semantic-binding-freezes-raw-descriptor-and-digest
  (let [captured (binding/capture-tool-binding
                  {:descriptor gmail-descriptor :provider :provider}
                  {:revision/id "sha256:revision" :revision/seq 7
                   :freshness :best-effort :binding/id #uuid "00000000-0000-0000-0000-000000000001"})
        spec (:binding/semantic-spec captured)
        journal (#'pipeline/effect-journal captured intent {:decision :allow}
                 :effect/committed)
        events-meta (#'scheduler/semantic-event-metadata
                     {:effect-journal (:effect-journal
                                       {:effect-journal journal})})]
    (is (= gmail-descriptor (:binding/descriptor captured)))
    (is (= (semantic/mapping-for :gmail.send) spec))
    (is (= (semantic/spec-digest spec) (:binding/semantic-digest captured)))
    (is (= spec (:semantic/spec (binding/binding->persisted captured))))
    (is (= spec (get-in journal [:effect/semantic :spec])))
    (is (= (:binding/semantic-digest captured)
           (get-in journal [:effect/semantic :digest])))
    (is (= {:semantic/spec spec
            :semantic/digest (:binding/semantic-digest captured)} events-meta))))

(deftest legacy-bindings-remain-untyped
  (let [legacy {:tool/id :fixture.echo
                :effect :pure
                :input-schema {}
                :output-schema {}
                :required-action :invoke}
        captured (binding/capture-tool-binding
                  legacy
                  {:revision/id "sha256:revision" :revision/seq 1
                   :freshness :best-effort
                   :binding/id #uuid "00000000-0000-0000-0000-000000000002"})]
    (is (= binding/canonical-binding-keys (set (keys captured))))
    (is (nil? (:binding/semantic-spec captured)))
    (is (not (contains? (binding/binding->persisted captured) :semantic/spec)))
    (is (nil? (semantic/semantic-spec-for legacy)))))
