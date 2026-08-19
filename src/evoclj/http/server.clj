(ns evoclj.http.server
  "HTTP API shell (Task S3-3): Jetty server lifecycle.

  start-server! returns the Jetty Server instance so the caller can
  stop it later with stop-server!. The kernel system map is injected
  into each request as `:system`."
  (:require [evoclj.http.api :as api]
            [ring.adapter.jetty :as jetty]))

;; --- public API --------------------------------------------------------------

(defn start-server!
  "Start a Jetty server on `port` (default 3000) with the API routes.

  `system` is the kernel system map; it is attached to each request as
  `:system` so handlers can inspect it. Returns the Jetty Server
  instance, or nil when `port` is nil."
  ([] (start-server! 3000 nil))
  ([port] (start-server! port nil))
  ([port system]
   (when port
     (let [wrapped (fn [request]
                     ((api/handler) (assoc request :system system)))]
       (jetty/run-jetty wrapped {:port (int port) :join? false})))))

(defn stop-server!
  "Stop the Jetty `server` instance returned by `start-server!`. Safe to
  call with nil (no-op)."
  [server]
  (when server
    (.stop server)))
