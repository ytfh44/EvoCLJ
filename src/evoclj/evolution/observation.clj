(ns evoclj.evolution.observation
  "Freshness and late-result guards for diagnostic observations.

  This namespace compares immutable subject identities. It does not own
  a surface registry, scheduler, or control authority."
  (:require [evoclj.evolution.diagnostic-schema :as schema]
            [evoclj.kernel.error :as err]
            [malli.core :as m]
            [malli.error :as me]))

(defn- validate-subject!
  [subject]
  (if-let [explanation (m/explain schema/SubjectSchema subject)]
    (throw (err/error :observation/subject-invalid
                      "observation subject is not a valid immutable identity"
                      {:errors (me/humanize explanation)}))
    subject))

(defn capture-token
  "Capture the immutable identity that an asynchronous observation may
  later use for admission. The token is bound to one diagnostic bundle."
  [bundle]
  (schema/validate-bundle bundle)
  {:observation/id (:diagnostic/id bundle)
   :subject (:subject bundle)})

(defn freshness
  "Compare a bundle's captured subject with the current subject.

  A different workspace is a scope mismatch, not merely stale data. A
  same-workspace revision or snapshot change is stale."
  [bundle current-subject]
  (schema/validate-bundle bundle)
  (validate-subject! current-subject)
  (let [captured (:subject bundle)
        _ (validate-subject! captured)]
    (cond
      (not= (:workspace/id captured) (:workspace/id current-subject))
      {:status :scope-mismatch
       :captured captured
       :current current-subject}

      (or (not= (:artifact/revision captured)
                (:artifact/revision current-subject))
          (and (contains? captured :snapshot/id)
               (not= (:snapshot/id captured)
                     (:snapshot/id current-subject))))
      {:status :stale
       :captured captured
       :current current-subject}

      :else
      {:status :fresh
       :captured captured
       :current current-subject})))

(defn guard-async-result
  "Return an explicit accept/reject decision for a late async result.

  The result is accepted only when the caller presents the token created
  from this exact bundle and the current subject still has the captured
  identity. Stale or cross-workspace results are returned as rejected
  decisions and must not mutate current state."
  [token bundle current-subject]
  (schema/validate-bundle bundle)
  (validate-subject! current-subject)
  (when-not (and (= (:observation/id token) (:diagnostic/id bundle))
                 (= (:subject token) (:subject bundle)))
    (throw (err/error :observation/token-mismatch
                      "asynchronous observation token does not match its bundle"
                      {:observation/id (:observation/id token)
                       :diagnostic/id (:diagnostic/id bundle)})))
  (let [comparison (freshness bundle current-subject)]
    (if (= :fresh (:status comparison))
      {:decision :accept
       :reason :fresh
       :diagnostic/id (:diagnostic/id bundle)
       :subject current-subject}
      {:decision :reject
       :reason (:status comparison)
       :diagnostic/id (:diagnostic/id bundle)
       :captured (:captured comparison)
       :current (:current comparison)})))
