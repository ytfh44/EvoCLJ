(ns evoclj.http.api
  "HTTP API shell (Task S3-3): Ring routes for health, session, and
  evolution status.

  The handler expects the kernel system map on the request as `:system`
  (injected by the server wrapper). Routes return plain Clojure maps
  with an EDN content type."
  (:require [ring.util.response :as response]))

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

;; --- dispatch ----------------------------------------------------------------

(defn handler
  "Ring handler for the EvoCLJ HTTP API shell.

  Supports:
    GET /health
    GET /sessions/:id
    GET /evolution/status

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

      :else
      (-> (response/response {:error/type :http/not-found
                              :message (str "No route for " uri)})
          (response/content-type "application/edn")
          (response/status 404)))))
