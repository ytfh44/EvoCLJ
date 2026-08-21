(ns evoclj.provider.fixture
  "Deterministic fixture providers for the seed Genome and the broker
  tests (component).

  Two providers live here:

  - :fixture/echo — the pure echo tool declared by the seed Genome
    (the normative descriptor example):

        {:tool/id :fixture/echo
         :effect :pure
         :input-schema [:map [:text :string]]
         :output-schema [:map [:text :string]]
         :required-action :invoke
         :retry {:safe? true}}

  - :fixture/path-resolve — a traversal-style fixture proving that
    normalize-request resolves the REAL target BEFORE authorization
    sees it (Global Constraint 9: a visible action/tool never grants
    resource authority). The provider is constructed with a protected
    root; raw user-facing input such as \"a/../secret\" canonicalizes
    to the protected path under that root, and the canonical resource
    descriptor carries {:kind :filesystem :path ...} in exactly the
    form the capability lease matcher consumes (component), so the
    broker checks the canonical protected path — never the raw string.
    Backslash and Windows drive forms normalize to the same canonical
    \"/\"-separated form; an absolute user path stays absolute and is
    never rebased under the protected root; a \"..\" that would climb
    above the filesystem root is clamped at \"/\". The canonical
    segment resolution reuses
    evoclj.capability.lease/canonicalize-path, the single source of
    truth for canonical path forms in the v0 capability model.

  Both providers follow the secrets rule: constructor config (roots,
  keys, tokens) is closed over and NEVER appears in describe output,
  normalized requests, or results. normalize-request validates the
  user-facing args against the descriptor's :input-schema first
  (:provider/input-invalid on failure) and returns a canonical
  resource descriptor. execute-request! consumes the normalized
  request (:provider/request-invalid on an unnormalized target) and
  returns a plain result VALUE, which the broker validates against
  :output-schema (component)."
  (:require [clojure.string :as str]
            [evoclj.capability.lease :as lease]
            [evoclj.kernel.error :as err]
            [evoclj.provider.protocol :as proto]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m]))

;; --- shared provider helpers -----------------------------------------------

(defn- intent-args
  "Extract the :args map from a tool-call intent payload. A payload
  that is not a map or carries no :args is a malformed request and is
  rejected with :provider/input-invalid before anything is
  normalized."
  [intent]
  (let [payload (:payload intent)]
    (when-not (and (map? payload) (contains? payload :args))
      (throw (err/error :provider/input-invalid
                        "tool-call payload must carry an :args map"
                        {:value (err/sanitize payload)})))
    (:args payload)))

(defn- validate-args!
  "Validate the user-facing args against the descriptor's
  :input-schema: EDN-safety first (Global Constraint 22), then the
  schema. Throws :provider/input-invalid on any failure, carrying a
  fully serializable Malli explanation."
  [descriptor args]
  (when-not (boundary/edn-safe? args)
    (throw (err/error :provider/input-invalid
                      "provider input must be plain EDN-safe data (Global Constraint 22)"
                      {:value (err/sanitize args)})))
  (when-not (m/validate (:input-schema descriptor) args)
    (throw (err/error :provider/input-invalid
                      "provider input failed input-schema validation"
                      {:value (err/sanitize args)
                       :explanation (err/sanitize (m/explain (:input-schema descriptor) args))}))))

(defn- expect-normalized!
  "Guard execute-request!: the authorized-request must be a canonical
  resource descriptor carrying the given key. A request that did not
  come through normalize-request is a kernel-side bug and fails
  closed with :provider/request-invalid rather than executing against
  an unnormalized target."
  [authorized-request key]
  (when-not (and (map? authorized-request)
                 (contains? authorized-request key))
    (throw (err/error :provider/request-invalid
                      "execute-request! requires a normalized request"
                      {:value (err/sanitize authorized-request)}))))

;; --- :fixture/echo ---------------------------------------------------------

(def ^:private echo-descriptor
  {:tool/id :fixture/echo
   :effect :pure
   :input-schema [:map [:text :string]]
   :output-schema [:map [:text :string]]
   :required-action :invoke
   :retry {:safe? true}})

(defn echo-provider
  "Build the pure :fixture/echo provider used by the seed Genome.

  Optional opts:

  - :secret — constructor-private config (closed over, never exposed
    in describe / normalize-request / execute-request! output),
    mirroring how a real adapter closes over its API key.
  - :execution-count — an atom bumped once per execute-request! call,
    so tests can assert when the provider REALLY ran: a denied request
    must never bump it (component Step 2).
  - :fail-count — a non-negative integer: the first N execute-request!
    calls throw a TRANSIENT provider error (:provider/transient-error)
    before succeeding, letting tests simulate a flaky upstream (component Step 3). The descriptor still declares :retry {:safe? true}
    because the echo effect is pure and idempotent, so the dispatcher
    MAY retry it.

  normalize-request validates the args ({:text \"hello\"}) against the
  input-schema and returns the canonical resource descriptor
  {:tool/id :fixture/echo :resource {:kind :tool :id :fixture/echo}
   :args {...}}. execute-request! returns {:text ...}, the value the
  broker validates against :output-schema."
  ([] (echo-provider {}))
  ([{:keys [secret execution-count fail-count]}]
   (when (and (some? secret) (not (string? secret)))
     (throw (err/error :provider/config-invalid
                       "provider secret must be a string"
                       {:value (err/sanitize secret)})))
   (when (and (some? fail-count)
              (not (and (int? fail-count) (not (neg? fail-count)))))
     (throw (err/error :provider/config-invalid
                       "provider fail-count must be a non-negative integer"
                       {:value (err/sanitize fail-count)})))
   (let [count (or execution-count (atom 0))]
     (reify proto/Provider
       (describe [_] echo-descriptor)
       (normalize-request [_ intent]
         (let [args (intent-args intent)]
           (validate-args! echo-descriptor args)
           {:tool/id :fixture/echo
            :resource {:kind :tool :id :fixture/echo}
            :args args}))
       (execute-request! [_ authorized-request]
         (expect-normalized! authorized-request :args)
         (let [n (swap! count inc)]
           (when (and fail-count (<= n fail-count))
             (throw (err/error :provider/transient-error
                               "transient fixture failure"
                               {:attempt n})))
           {:text (get-in authorized-request [:args :text])}))))))

;; --- :fixture/non-idempotent --------------------------------------------

(def ^:private non-idempotent-descriptor
  ;; Deliberately NO :retry block: automatic retries are allowed only
  ;; when a provider declares :retry {:safe? true} (component), so the
  ;; dispatcher must NEVER auto-retry this provider, even when it
  ;; reports a transient failure.
  {:tool/id :fixture/non-idempotent
   :effect :pure
   :input-schema [:map [:text :string]]
   :output-schema [:map [:text :string]]
   :required-action :invoke})

(defn non-idempotent-provider
  "Build the :fixture/non-idempotent provider (component Step 3).

  The descriptor deliberately carries NO :retry block, so the
  dispatcher must never retry this provider, even when it reports a
  transient failure (:provider/transient-error): a non-idempotent
  action must not be blindly retried (Transaction Boundaries
  protocol).

  Optional opts: :secret (constructor-private, closed over),
  :execution-count (atom bumped once per execute-request! call), and
  :fail-count (the first N execute-request! calls throw a transient
  error before succeeding — simulating an upstream that would have
  recovered had it been retried).

  normalize-request and execute-request! mirror :fixture/echo's shape
  under the distinct tool id :fixture/non-idempotent."
  ([] (non-idempotent-provider {}))
  ([{:keys [secret execution-count fail-count]}]
   (when (and (some? secret) (not (string? secret)))
     (throw (err/error :provider/config-invalid
                       "provider secret must be a string"
                       {:value (err/sanitize secret)})))
   (when (and (some? fail-count)
              (not (and (int? fail-count) (not (neg? fail-count)))))
     (throw (err/error :provider/config-invalid
                       "provider fail-count must be a non-negative integer"
                       {:value (err/sanitize fail-count)})))
   (let [count (or execution-count (atom 0))]
     (reify proto/Provider
       (describe [_] non-idempotent-descriptor)
       (normalize-request [_ intent]
         (let [args (intent-args intent)]
           (validate-args! non-idempotent-descriptor args)
           {:tool/id :fixture/non-idempotent
            :resource {:kind :tool :id :fixture/non-idempotent}
            :args args}))
       (execute-request! [_ authorized-request]
         (expect-normalized! authorized-request :args)
         (let [n (swap! count inc)]
           (when (and fail-count (<= n fail-count))
             (throw (err/error :provider/transient-error
                               "transient fixture failure"
                               {:attempt n})))
           {:text (get-in authorized-request [:args :text])}))))))

;; --- :fixture/path-resolve -------------------------------------------------

(def ^:private default-root "/protected/work")

(defn- windows-drive?
  "True for a Windows drive-letter absolute path form (\"C:/x\")."
  [s]
  (boolean (re-matches #"[A-Za-z]:.*" s)))

(defn- resolve-path
  "Resolve a raw user-facing path string against the provider's
  protected root into the canonical \"/\"-separated absolute path.

  Backslashes normalize to \"/\" first. A relative path is joined
  under the root; an absolute path (including a Windows drive form)
  stays absolute and is never rebased under the root, so a
  user-supplied \"/etc/passwd\" or \"C:/x\" can never be re-rooted
  into the protected tree. The joined form is then canonicalized with
  the v0 segment resolver (evoclj.capability.lease/canonicalize-path):
  \".\" and \"..\" segments collapse (\"/protected/work/a/../secret\"
  -> \"/protected/work/secret\"), and a \"..\" that would climb above
  the filesystem root is clamped at \"/\"."
  [root raw]
  (let [raw (str/replace raw "\\" "/")
        absolute? (or (.startsWith raw "/") (windows-drive? raw))
        joined (if absolute? raw (str root "/" raw))
        joined (if (windows-drive? joined) (str "/" joined) joined)]
    (lease/canonicalize-path joined)))

(def ^:private path-resolve-descriptor
  {:tool/id :fixture/path-resolve
   :effect :pure
   :input-schema [:map [:path :string]]
   :output-schema [:map [:path :string]]
   :required-action :invoke
   :retry {:safe? true}})

(defn path-resolve-provider
  "Build the traversal-style :fixture/path-resolve provider.

  Optional opts: :root — the protected root relative paths resolve
  against (default \"/protected/work\"); :secret — constructor-private
  config, closed over and never exposed.

  normalize-request validates the args ({:path \"a/../secret\"}),
  resolves the raw path to its canonical form, and returns the
  canonical resource descriptor {:tool/id :fixture/path-resolve
   :resource {:kind :filesystem :path \"/protected/work/secret\"}
   :args {...}} — the REAL target, in exactly the canonical form the
  capability lease matcher checks (component), so authorization sees
  the protected canonical path and never the raw traversal string.
  execute-request! returns {:path <canonical>}, the value the broker
  validates against :output-schema."
  ([] (path-resolve-provider {}))
  ([{:keys [root secret]}]
   (when (and (some? secret) (not (string? secret)))
     (throw (err/error :provider/config-invalid
                       "provider secret must be a string"
                       {:value (err/sanitize secret)})))
   (let [root (or root default-root)]
     (reify proto/Provider
       (describe [_] path-resolve-descriptor)
       (normalize-request [_ intent]
         (let [args (intent-args intent)]
           (validate-args! path-resolve-descriptor args)
           {:tool/id :fixture/path-resolve
            :resource {:kind :filesystem
                       :path (resolve-path root (:path args))}
            :args args}))
       (execute-request! [_ authorized-request]
         (expect-normalized! authorized-request :resource)
         {:path (get-in authorized-request [:resource :path])})))))
