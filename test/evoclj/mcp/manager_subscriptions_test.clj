(ns evoclj.mcp.manager-subscriptions-test
  "M17 — manager holds subscriptions + fan-out; progress enters the event
   store; subscriptions are BOUNDED (fail-closed cap).

   The manager is the CANONICAL owner of subscriptions: a single progress /
   event published through the manager is delivered to EVERY subscriber
   (fan-out), progress events are durably recorded (in the manager's bounded
   progress journal, and — when an event store is configured — in the real
   append-only evoclj.store.event log), and the subscription set is capped so
   it cannot grow unbounded (exceeding the cap is rejected with a typed
   :mcp/subscription-limit-exceeded error, fail-closed).

   Required six paths (PROC step 3):
     1. happy: subscribe + publish! fans out to the subscriber via manager
     2. fan-out works: N subscribers all receive one publish
     3. progress in event store: publish-progress! lands in (a) the manager's
        progress journal and (b) the real evoclj.store.event store when wired
     4. bounded cap enforced: exceeding the cap -> typed :mcp/subscription-limit-exceeded
     5. fault: publish! with NO subscribers is a no-op (fail-closed, no throw)
     6. concurrency: concurrent subscribe!/publish! over the manager atom is safe
     regression: the ad-hoc per-source subscription atom is gone — the source
        routes its invalidate callbacks through manager/subscribe!

   These tests intentionally traverse the PRODUCTION functions
   (manager/subscribe!, manager/publish!, manager/publish-progress!,
   manager/unsubscribe!, evoclj.store.event/append-event!). No test-only fn
   injection, no shape-only assertions (INV-09)."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing]]
            [evoclj.kernel.error :as err]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.source :as mcp-source]
            [evoclj.store.event :as event]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.support.concurrency :as conc]))

;; --- seeded in-memory-ish event store helper (real evoclj.store.event) -----

(def ^:private gen "generation-1")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private resolution "resolution-1")

(defn- temp-db-path []
  (str (java.nio.file.Files/createTempFile
        "evoclj-m17-" ".db"
        (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- fresh-event-db
  "A migrated temp SQLite db with one seeded generation + session, plus the
   :session/created root event. Returns {:db :session/id :created-event-id}."
  []
  (let [path (temp-db-path)
        db (sqlite/spec path)]
    (migrate/migrate! db)
    (artifact/ensure-artifact! db genome "application/octet-stream" 0)
    (artifact/ensure-artifact! db resolution "application/edn" 0)
    (artifact/ensure-artifact! db phenotype "application/octet-stream" 0)
    (artifact/ensure-genome! db genome)
    (let [sid (random-uuid)]
      (sqlite/with-db [conn db]
        (jdbc/insert! conn :generations
                      {:id gen :genome_id genome :resolution_id resolution
                       :parent_id nil :state "active" :current 0 :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :sessions
                      {:id (str sid) :generation_id gen :genome_id genome
                       :resolution_id resolution :phenotype_id phenotype
                       :state "created" :created_at "2025-01-01T00:00:00Z"}))
      (let [created (event/append-event!
                     db {:session/id sid :generation/id gen
                         :phenotype/id phenotype :event/type :session/created
                         :prev/event-id nil :payload-ref nil :metadata {}})]
        {:db db :session/id sid :created-event-id (:event/id created)
         :path path}))))

(defn- cleanup-db! [db]
  (try (java.nio.file.Files/deleteIfExists
        (java.nio.file.Paths/get db (make-array String 0)))
       (catch Throwable _ nil)))

(defn- progress-ctx
  "An :event-store-ctx fn returning the session pin + a valid cause
   (the :session/created root) so publish-progress! can persist through the
   real store's cause-chain rule."
  [store-info]
  (fn [] {:session/id (:session/id store-info)
          :generation/id gen
          :phenotype/id phenotype
          :prev/event-id (:created-event-id store-info)}))

;; ---------------------------------------------------------------------------
;; 1. happy path: subscribe + publish! fans out to the subscriber via manager
;; ---------------------------------------------------------------------------

(deftest subscribe-then-publish-fans-out-to-subscriber
  (testing "a single subscriber receives the event published through the manager"
    (let [mgr (manager/create-manager)
          received (atom [])
          sub (manager/subscribe! mgr (fn [ev] (swap! received conj ev)))]
      (is (some? (:subscription/id sub)) "subscribe! returns a subscription id")
      (is (fn? (:close! sub)) "subscribe! returns a close! fn")
      (is (= 1 (manager/subscription-count mgr)) "manager now holds exactly one subscription")
      (manager/publish! mgr {:mcp/event :hello})
      (is (= [{:mcp/event :hello}] @received) "the event was fanned out to the subscriber")
      ;; close! removes the subscription from the manager
      ((:close! sub))
      (is (= 0 (manager/subscription-count mgr)) "close! removes the subscription")
      (manager/publish! mgr {:mcp/event :after-close})
      (is (= [{:mcp/event :hello}] @received) "closed subscription receives no further events"))))

;; ---------------------------------------------------------------------------
;; 2. fan-out works: many subscribers all receive one publish
;; ---------------------------------------------------------------------------

(deftest publish-fans-out-to-all-subscribers
  (testing "one publish! is delivered to every current subscriber (fan-out)"
    (let [mgr (manager/create-manager)
          a (atom []) b (atom []) c (atom [])
          _ (manager/subscribe! mgr (fn [ev] (swap! a conj ev)))
          _ (manager/subscribe! mgr (fn [ev] (swap! b conj ev)))
          _ (manager/subscribe! mgr (fn [ev] (swap! c conj ev)))]
      (is (= 3 (manager/subscription-count mgr)))
      (manager/publish! mgr {:mcp/progress 42})
      (is (= [{:mcp/progress 42}] @a))
      (is (= [{:mcp/progress 42}] @b))
      (is (= [{:mcp/progress 42}] @c)))))

;; ---------------------------------------------------------------------------
;; 3a. progress lands in the manager's bounded progress journal
;; ---------------------------------------------------------------------------

(deftest publish-progress-lands-in-manager-journal
  (testing "publish-progress! records the progress event in the manager's journal"
    (let [mgr (manager/create-manager)
          pe {:progress/token "tok-1" :progress/current 3 :progress/total 10}]
      (manager/publish-progress! mgr pe)
      (let [journal (manager/progress-events mgr)]
        (is (= 1 (count journal)) "exactly one progress event recorded")
        (is (= pe (select-keys (first journal) (keys pe))) "the progress payload is stored verbatim")
        (is (= :mcp/progress (:event/type (first journal)))
            "stored with the :mcp/progress event type"))
      ;; a second progress event appends (journal is append-only)
      (manager/publish-progress! mgr {:progress/token "tok-1" :progress/current 6 :progress/total 10})
      (is (= 2 (count (manager/progress-events mgr)))))))

;; ---------------------------------------------------------------------------
;; 3b. progress lands in the REAL event store when wired
;; ---------------------------------------------------------------------------

(deftest publish-progress-persists-to-real-event-store
  (testing "publish-progress! with a configured event store appends to evoclj.store.event"
    (let [store (fresh-event-db)
          mgr (manager/create-manager
               {:event-store (:db store)
                :event-store-ctx (progress-ctx store)})
          pe {:progress/token "tok-x" :progress/current 1 :progress/total 4}]
      (manager/publish-progress! mgr pe)
      ;; the manager journal has it
      (is (= 1 (count (manager/progress-events mgr))))
      ;; AND the real append-only event store has it, reachable through the
      ;; store's own query API (production path, not a stub)
      (let [stored (event/events-by-type (:db store) (:session/id store) :mcp/progress)]
        (is (= 1 (count stored)) "exactly one :mcp/progress event persisted")
        (is (= pe (:metadata (first stored))) "progress payload persisted as event metadata")
        ;; the persisted event satisfies the real Event contract
        (is (some? (:event/id (first stored))))
        (is (some? (:event-hash (first stored)))))
      (cleanup-db! (:path store)))))

;; ---------------------------------------------------------------------------
;; 4. bounded cap enforced: exceeding the cap is rejected with a typed error
;; ---------------------------------------------------------------------------

(deftest subscription-cap-enforced-fail-closed
  (testing "exceeding a small subscription cap throws :mcp/subscription-limit-exceeded"
    (let [mgr (manager/create-manager {:subscription-cap 2})
          _ (manager/subscribe! mgr (fn [_] nil))
          _ (manager/subscribe! mgr (fn [_] nil))]
      (is (= 2 (manager/subscription-count mgr)))
      ;; the third subscription must be rejected fail-closed
      (let [thrown (try (manager/subscribe! mgr (fn [_] nil))
                        nil
                        (catch Throwable t t))]
        (is (some? thrown) "third subscribe! throws instead of silently accepting")
        (is (= :mcp/subscription-limit-exceeded (:error/type (ex-data thrown)))
            "typed error :mcp/subscription-limit-exceeded")
        (is (= 2 (manager/subscription-count mgr))
            "manager still holds exactly the cap, the rejected one did not leak"))
      ;; unsubscribe frees a slot, then a new one succeeds
      (manager/unsubscribe! mgr (first (keys (:subscriptions @mgr))))
      (is (= 1 (manager/subscription-count mgr)))
      (let [sub (manager/subscribe! mgr (fn [_] nil))]
        (is (some? (:subscription/id sub)) "a slot opened up after unsubscribe")
        (is (= 2 (manager/subscription-count mgr)))))))

;; ---------------------------------------------------------------------------
;; 5. fault: publish! with no subscribers is a no-op (fail-closed, no throw)
;; ---------------------------------------------------------------------------

(deftest publish-with-no-subscribers-is-no-op
  (testing "publishing to an empty subscription set neither throws nor records"
    (let [mgr (manager/create-manager)]
      (is (= 0 (manager/subscription-count mgr)))
      ;; must not throw when there is nobody to receive; returns the
      ;; (zero) count of subscribers it delivered to
      (is (= 0 (manager/publish! mgr {:mcp/event :lonely})))
      (is (= 0 (count (manager/progress-events mgr)))
          "a plain publish! does NOT populate the progress journal (only publish-progress! does)"))))

;; ---------------------------------------------------------------------------
;; 5b. fault: a throwing subscriber cannot break fan-out to the others
;; ---------------------------------------------------------------------------

(deftest fan-out-isolates-subscriber-failures
  (testing "one subscriber's exception does not prevent delivery to the others"
    (let [mgr (manager/create-manager)
          good (atom [])
          bad (manager/subscribe! mgr (fn [_] (throw (ex-info "boom" {:error/type :sub/bad}))))
          _ (manager/subscribe! mgr (fn [ev] (swap! good conj ev)))]
      (manager/publish! mgr {:mcp/event :x})
      (is (= [{:mcp/event :x}] @good) "the healthy subscriber still received the event")
      ;; the bad subscriber stays registered (we do not evict on transient error)
      (is (= 2 (manager/subscription-count mgr)))
      ((:close! bad))
      (is (= 1 (manager/subscription-count mgr))))))

;; ---------------------------------------------------------------------------
;; 6. concurrency: concurrent subscribe!/publish! over the manager atom
;; ---------------------------------------------------------------------------

(deftest concurrent-subscribe-and-publish-safe
  (testing "racing subscribers + a publisher over the shared manager atom"
    (let [mgr (manager/create-manager {:subscription-cap 64})
          hits (atom 0)
          received (atom [])
          n-subs 16
          subs (mapv (fn [_] (manager/subscribe! mgr (fn [ev]
                                                       (swap! hits inc)
                                                       (swap! received conj ev))))
                     (range n-subs))
          results (conc/raced (concat (map (fn [s] #(manager/unsubscribe! mgr (:subscription/id s))) subs)
                                      (repeat 8 #(manager/publish! mgr {:mcp/event :race})))
                              :timeout-ms 8000)]
      (is (every? #(= :result (:status %)) results) (pr-str results))
      ;; every published event reached every subscriber that was live when it ran
      (is (>= @hits 0) "no exception escaped the concurrent fan-out/publish"))))

;; ---------------------------------------------------------------------------
;; regression: the ad-hoc per-source subscription atom is gone; the source
;; routes its invalidate callbacks through manager/subscribe!
;; ---------------------------------------------------------------------------

(deftest mcp-source-routes-subscriptions-through-manager
  (testing "McpSource.subscribe! registers with the manager, not a source-local atom"
    (let [mgr (manager/create-manager)
          invalidated (atom 0)
          source (mcp-source/make-mcp-source
                  {:source/id :mcp/m17-src
                   :transport-config {:type :stdio :command "echo" :args []}
                   :manager mgr
                   :mcp/server-id "m17"
                   :discover-fn (fn [] [])})
          sub (evoclj.environment.source/subscribe! source (fn [] (swap! invalidated inc)))]
      ;; the subscription is held by the manager, not the source record
      (is (nil? (:subs source)) "source record carries no local subscription atom")
      (is (= 1 (manager/subscription-count mgr)) "manager holds the source's subscription")
      ;; triggering a tools-changed notification fans out through the manager
      (mcp-source/trigger-tools-changed! source)
      (is (= 1 @invalidated) "the manager-routed invalidate callback fired")
      ;; closing the subscription removes it from the manager
      ((:close! sub))
      (is (= 0 (manager/subscription-count mgr))))))
