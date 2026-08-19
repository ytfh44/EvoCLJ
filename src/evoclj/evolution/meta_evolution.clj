(ns evoclj.evolution.meta-evolution
  "Meta-evolution: evolve prompts, weights, and policies themselves (S3-2).

  A MetaGenome is a vector of parameter maps. The MetaEvolution protocol
  mutates one meta-genome into a new candidate. v0 ships three concrete
  mutators — prompt, weight, policy — and a convenience constructor that
  extracts the current parameters from a host system config."
  (:require [evoclj.evolution.meta-schema :as ms]
            [evoclj.kernel.error :as err]))

;; --- the protocol ------------------------------------------------------------

(defprotocol MetaEvolution
  (mutate-meta [this meta-genome]
    "Return a new meta-genome with mutated parameters, or nil when
    this adapter has nothing to propose."))

;; --- helpers -----------------------------------------------------------------

(defn- gaussian-noise
  "Simple Box-Muller-ish noise (mean 0, std 1), scaled."
  [scale]
  (* scale (- (rand) 0.5)))

(defn- clamp
  [x lo hi]
  (min hi (max lo x)))

(def ^:private prompt-synonyms
  "A tiny built-in synonym map for prompt mutation."
  {:improve ["enhance" "refine" "optimize" "polish"]
   :fix ["repair" "correct" "resolve" "amend"]
   :check ["verify" "validate" "inspect" "review"]
   :add ["append" "insert" "include" "attach"]
   :remove ["delete" "drop" "omit" "strip"]})

(defn- synonym-swap
  "Replace one word in text with a synonym when available."
  [text]
  (if (empty? text)
    text
    (let [words (clojure.string/split text #"\s+")
          idx (rand-int (count words))
          word (nth words idx)
          syms (get prompt-synonyms (keyword (clojure.string/lower-case word)))]
      (if (seq syms)
        (clojure.string/join " " (assoc words idx (rand-nth syms)))
        text))))

;; --- implementations ---------------------------------------------------------

(defn- mutate-prompt-param
  [param]
  (let [text (:prompt/text param)]
    (cond
      (< (rand) 0.5)
      (assoc param :prompt/text (synonym-swap text))
      :else
      (assoc param :prompt/text (str text " (refined)")))))

(defn- mutate-weight-param
  [param]
  (let [v (:weight/value param)
        lo (or (:weight/min param) (- v 1.0))
        hi (or (:weight/max param) (+ v 1.0))
        nv (clamp (+ v (gaussian-noise 0.1)) lo hi)]
    (assoc param :weight/value (double nv))))

(defn- mutate-policy-param
  [param]
  (let [v (:policy/value param)]
    (cond
      (number? v)
      (assoc param :policy/value (double (clamp (+ v (gaussian-noise 0.1))
                                                (- (abs v) 1.0)
                                                (+ (abs v) 1.0))))
      (boolean? v)
      (assoc param :policy/value (not v))
      :else
      param)))

(defn- mutate-one
  [param]
  (cond
    (:prompt/type param)
    (mutate-prompt-param param)
    (:weight/value param)
    (mutate-weight-param param)
    (:policy/value param)
    (mutate-policy-param param)
    :else
    param))

;; --- public constructors -----------------------------------------------------

(defn prompt-mutator
  "A MetaEvolution that mutates prompt parameters."
  []
  (reify MetaEvolution
    (mutate-meta [_ meta-genome]
      (let [params (vec (mapv mutate-one (:meta/params meta-genome)))]
        (ms/validate-meta-genome
          (assoc meta-genome :meta/params params))))))

(defn weight-mutator
  "A MetaEvolution that mutates weight parameters."
  []
  (reify MetaEvolution
    (mutate-meta [_ meta-genome]
      (let [params (vec (mapv mutate-one (:meta/params meta-genome)))]
        (ms/validate-meta-genome
          (assoc meta-genome :meta/params params))))))

(defn policy-mutator
  "A MetaEvolution that mutates policy parameters."
  []
  (reify MetaEvolution
    (mutate-meta [_ meta-genome]
      (let [params (vec (mapv mutate-one (:meta/params meta-genome)))]
        (ms/validate-meta-genome
          (assoc meta-genome :meta/params params))))))

;; --- meta-genome construction ------------------------------------------------

(defn create-meta-genome
  "Build a new meta-genome from an optional system config map. Extracts
  known prompts/weights/policies when present; otherwise returns an empty
  meta-genome."
  ([]
   (create-meta-genome {}))
  ([config]
   (let [params (vec (concat
                       (when-let [p (get-in config [:evolution/system :diagnostician :system-prompt])]
                         [{:prompt/type :diagnostician
                           :prompt/text p
                           :prompt/version 1}])
                       (when-let [p (get-in config [:evolution/system :mutator :system-prompt])]
                         [{:prompt/type :mutator
                           :prompt/text p
                           :prompt/version 1}])
                       (when-let [p (get-in config [:eval/system :judge :system-prompt])]
                         [{:prompt/type :judge
                           :prompt/text p
                           :prompt/version 1}])))]
     (ms/validate-meta-genome
       {:meta/params params
        :meta/fitness 0.0
        :meta/generation-id "meta-1"}))))

(defn evaluate-meta-genome
  "Stub fitness evaluator for a meta-genome. v0 returns a configurable
  fitness (default 0.0). A real implementation would run one generation
  with the mutated parameters and return the observed utility."
  ([meta-genome]
   (evaluate-meta-genome meta-genome {}))
  ([meta-genome opts]
   (let [fitness (or (:meta/fitness meta-genome)
                     (:fitness opts)
                     0.0)]
     (assoc meta-genome :meta/fitness (double fitness)))))
