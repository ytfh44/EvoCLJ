(ns evoclj.promotion.canary
  "Task 9.3 — deterministic canary routing for NEW sessions.

  Route the creation of a NEW session to the current generation or to a
  canary generation from a stable hash of the session's routing key —
  NEVER a mutable global random source (Step 1). Sessions already
  created stay pinned to the generation they were created under: the
  pin lives in the sessions row (Task 5.4, Global Constraint 2), and
  this namespace only ever DECIDES which generation a NEW session is
  created against; it writes nothing and holds no state.

  DEPLOYMENT-STATE SHAPE (normative, designed here and documented for
  Tasks 9.4-9.5):

      {:current-generation \"G42\"          ; the fallback (CURRENT pointer)
       :canary {:generation \"G43\"          ; the candidate under rollout
                :allocation 0.10             ; fraction of NEW sessions
                                             ; routed to the canary
                :ladder [0.10 0.25 0.50 1.0] ; the declared rollout path
                                             ; (normative ladder default)
                :version \"v1\"}              ; allocation version, persisted
                                             ; with every session decision
                                             ; so routing can be audited
       :active? true}                        ; canary traffic enabled?

  :allocation is the CURRENT step of the ladder actually in effect;
  :ladder records the declared path it walks. Routing reads :allocation
  only (Step 3 tests each ladder step's measured share). :version is the
  immutable allocation version recorded with each session's :routing
  (Step 4).

  ROUTING (Step 1, the plan's formula): the routing key is hashed with
  evoclj.genome.hash/text-digest (the repo's sha256 conventions —
  CRLF/CR normalized to LF, so the same key always hashes the same);
  the first 16 hex chars of the digest are read as an unsigned integer,
  reduced mod 10000, and divided by 10000.0:

      (mod (bigint (subs digest 0 16) 16) 10000) / 10000.0 < :allocation
      → canary

  bucket < :allocation selects the canary generation; everything else
  selects :current-generation. The same (deployment-state, key) pair
  therefore always yields the same decision; there is no random state
  anywhere in the path.

  ERROR CONTRACT (Global Constraint 22 — plain serializable data):
  :promotion/routing-invalid — a malformed deployment state, an
  out-of-range allocation, or a non-string routing key. Missing or
  inactive canary configuration is NOT an error: it simply routes every
  key to the current generation. A nil deployment state returns nil —
  there is no canary information to decide with, so the caller falls
  back to its own notion of CURRENT."
  (:require [clojure.string :as str]
            [evoclj.genome.hash :as genome-hash]
            [evoclj.kernel.error :as err]
            [malli.core :as m]
            [malli.error :as me]))

;; --- the normative ladder ----------------------------------------------------

(def ladder-default
  "The NORMATIVE canary ladder (Task 9.3): 10% → 25% → 50% → 100%."
  [0.10 0.25 0.50 1.0])

;; --- boundary validation -----------------------------------------------------

(def ^:private fraction?
  "A fraction in [0, 1]. (Raw function children inside :and do not
  compile in this malli version, so the predicate is wrapped in an
  explicit [:fn ...].)"
  [:and number? [:fn (fn [x] (<= 0.0 x 1.0))]])

(def CanaryConfig
  "The :canary map of the deployment-state shape."
  [:map {:closed true}
   [:generation string?]
   [:allocation fraction?]
   [:ladder [:sequential fraction?]]
   [:version string?]])

(def DeploymentState
  "The deployment-state shape (designed and documented above). :canary
  is optional — nil means no rollout is configured and every new
  session goes to :current-generation."
  [:map {:closed true}
   [:current-generation string?]
   [:canary {:optional true} [:maybe CanaryConfig]]
   [:active? boolean?]])

(defn- deployment-error!
  "Throw :promotion/routing-invalid with a humanized Malli explanation."
  [expl]
  (throw (err/error :promotion/routing-invalid
                    "deployment state does not satisfy the routing contract"
                    {:errors (me/humanize expl)})))

(defn- validate-deployment-state
  "Validate `deployment-state` against DeploymentState and return it.
  nil is legal (no canary information); anything else must satisfy the
  closed schema or fail closed with :promotion/routing-invalid."
  [deployment-state]
  (if (nil? deployment-state)
    nil
    (do (when-let [expl (m/explain DeploymentState deployment-state)]
          (deployment-error! expl))
        deployment-state)))

(defn- validate-routing-key
  "A routing key must be a string: the stable hash is computed over its
  UTF-8 bytes (evoclj.genome.hash/text-digest)."
  [routing-key]
  (when-not (string? routing-key)
    (throw (err/error :promotion/routing-invalid
                      "the session routing key must be a string"
                      {:routing-key routing-key})))
  routing-key)

;; --- the stable bucket --------------------------------------------------------

(def ^:private bucket-scale
  "The modulus of the plan's bucket formula (10000)."
  10000)

(defn- digest-hex
  "The 64-hex-char digest of `routing-key` (the repo's sha256
  conventions, with the \"sha256:\" prefix stripped)."
  [routing-key]
  (subs (genome-hash/text-digest routing-key) (count "sha256:")))

(defn- bucket-int
  "The integer bucket of `routing-key`: (mod (bigint (subs digest 0 16)
  16) 10000), coerced to a Long so it satisfies the Session contract's
  :bucket int?."
  [routing-key]
  (long (mod (biginteger (java.math.BigInteger. (subs (digest-hex routing-key) 0 16) 16))
             bucket-scale)))

(defn routing-bucket
  "The deterministic allocation bucket of `routing-key`: a Double in
  [0, 1), computed from the first 16 hex chars of the key's sha256
  digest (evoclj.genome.hash conventions) reduced mod 10000 (the plan's
  formula). A pure function of the key — never a mutable global random
  source — so the same key always lands in the same bucket."
  [routing-key]
  (validate-routing-key routing-key)
  (/ (double (bucket-int routing-key)) (double bucket-scale)))

;; --- the routing decision -------------------------------------------------------

(defn- canary?
  "True when the deployment state actively routes `routing-key` to the
  canary generation: :active? is true, a canary is configured, and the
  key's bucket falls strictly below the configured allocation."
  [deployment-state routing-key]
  (let [canary (:canary deployment-state)]
    (boolean (and (:active? deployment-state)
                  canary
                  (< (routing-bucket routing-key) (:allocation canary))))))

(defn select-generation-for-new-session
  "The deterministic generation choice for a NEW session
  (Task 9.3 interface): the canary generation when
  `deployment-state` actively routes `stable-routing-key` into the
  canary allocation, else :current-generation. nil deployment state
  returns nil (no canary information — the caller falls back to its own
  CURRENT pointer). Purely a function of its arguments; it never
  consults or mutates any global state, so the same key and deployment
  state always choose the same generation.

  Throws :promotion/routing-invalid for a malformed deployment state,
  an out-of-range allocation, or a non-string routing key."
  [deployment-state stable-routing-key]
  (let [ds (validate-deployment-state deployment-state)]
    (when ds
      (validate-routing-key stable-routing-key)
      (if (canary? ds stable-routing-key)
        (:generation (:canary ds))
        (:current-generation ds)))))

(defn routing-decision
  "The full routing decision for a NEW session (Step 4 wiring): the
  chosen generation id plus the :routing map to persist with the
  session so routing can be audited later:

      {:generation/id \"G43\"
       :routing {:deployment-version \"v1\"   ; allocation version at
                                             ; decision time
                 :bucket 7441}}              ; the key's stable bucket

  :routing is nil when no canary is configured at all (there is no
  allocation version to record); the caller then creates the session
  without routing. When a canary is configured the version and bucket
  are always recorded — even while :active? is false — because the
  record is an audit of the deployment state the session was created
  under, and it never changes afterwards (the pin lives in the store)."
  [deployment-state stable-routing-key]
  (let [ds (validate-deployment-state deployment-state)]
    (if (nil? ds)
      {:generation/id nil :routing nil}
      (do (validate-routing-key stable-routing-key)
          (let [canary (:canary ds)
                bucket (bucket-int stable-routing-key)
                generation (if (canary? ds stable-routing-key)
                             (:generation canary)
                             (:current-generation ds))
                routing (when canary
                          {:deployment-version (:version canary)
                           :bucket bucket})]
            {:generation/id generation
             :routing routing})))))
