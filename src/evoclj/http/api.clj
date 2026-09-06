(ns evoclj.http.api
  "HTTP API shell (component): Ring routes for health, session,
  evolution status, and deployment polling.

  The handler expects the kernel system map on the request as `:system`
  (injected by the server wrapper). Routes return plain Clojure maps
  with an EDN content type.

  GET /api/deployment/current is the host polling read path
  (docs/host-polling-protocol.md): the CURRENT generation row
  (generations.current = 1) as {:generation/id :genome/id :canary
  :timestamp}. :canary is nil — rollout allocations live in the
  deployment-state envelopes returned by `evoclj deploy`, not in the
  generations table, and nil means no rollout is configured (the
  canary contract treats that as route-everything-to-current, never
  an error). No CURRENT row is a 404 the host answers by falling back
  to the seed generation. The poll is read-only: one SELECT, no
  writes, no locks, no promotion side effects."
  (:require [evoclj.promotion.current :as current]
            [ring.util.response :as response]))

;; --- handlers ----------------------------------------------------------------

(defn- health-handler
  "GET /health — liveness check."
  [_request]
  (-> (response/response {:status "ok"})
      (response/content-type "application/edn")))

(defn- session-status-handler
  "GET /sessions/:id — minimal session status placeholder."
  [request]
  (let [id (get-in request [:path-params :id])]
    (-> (response/response {:session/id id :status "unknown"})
        (response/content-type "application/edn"))))

(defn- evolution-status-handler
  "GET /evolution/status — minimal evolution status placeholder
  derived from the injected system map."
  [request]
  (-> (response/response {:status "ok"
                          :evolution/system (select-keys (:system request)
                                                        [:runtime/executor
                                                         :evolution/system])})
      (response/content-type "application/edn")))

(defn- deployment-current-handler
  "GET /api/deployment/current — read-only CURRENT generation poll.
  200 with {:generation/id :genome/id :canary nil :timestamp} when a
  CURRENT row exists; 404 with :deployment/no-current-generation when
  none does; 500 with :http/store-unavailable when the request carries
  no sqlite store (host wiring bug, never a quiet empty poll)."
  [request]
  (let [store (get-in request [:system :store/sqlite])]
    (if (nil? store)
      (-> (response/response {:error/type :http/store-unavailable
                              :message "no sqlite store on :system; the server wrapper must inject the kernel system map"})
          (response/content-type "application/edn")
          (response/status 500))
      (let [row (try (current/current-generation store) (catch Exception _ ::error))]
        (cond
          (= ::error row)
          (-> (response/response {:error/type :deployment/poll-failed
                                  :message "CURRENT generation read failed"})
              (response/content-type "application/edn")
              (response/status 500))
          (nil? row)
          (-> (response/response {:error/type :deployment/no-current-generation
                                  :message "no CURRENT generation; host falls back to the seed generation"})
              (response/content-type "application/edn")
              (response/status 404))
          :else
          (-> (response/response {:generation/id (:id row)
                                  :genome/id (:genome_id row)
                                  :canary nil
                                  :timestamp (str (java.time.Instant/now))})
              (response/content-type "application/edn")))))))

;; --- dispatch ----------------------------------------------------------------

(defn handler
  "Ring handler for the EvoCLJ HTTP API shell.

  Supports:
    GET /health
    GET /sessions/:id
    GET /evolution/status
    GET /api/deployment/current

  Returns 404 for unmatched routes."
  [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (and (= method :get) (= uri "/health"))
      (health-handler request)

      (and (= method :get) (.startsWith uri "/sessions/"))
      (let [id (subs uri (count "/sessions/"))]
        (session-status-handler (assoc request :path-params {:id id})))

      (and (= method :get) (= uri "/evolution/status"))
      (evolution-status-handler request)

      (and (= method :get) (= uri "/api/deployment/current"))
      (deployment-current-handler request)

      :else
      (-> (response/response {:error/type :http/not-found
                              :message (str "No route for " uri)})
          (response/content-type "application/edn")
          (response/status 404)))))
