(ns evoclj.capability.budget
  "Pure finite-resource accounting for capability budgets.

  A budget is a closed map of canonical dimensions. Values are non-negative
  long integers; money is represented in fixed-point micro-USD, never as
  floating point. Durable reservation/settlement lives in
  evoclj.store.budget-store; this namespace has no persistence side effects."
  (:require [evoclj.kernel.error :as err]))

(def budget-version 1)

;; The market is deliberately closed. Aliases keep the public vocabulary
;; compatible with provider-specific callers while every persisted value has
;; one canonical shape.
(def canonical-dimensions
  #{:calls :tokens :bytes :money-usd-micros})
(def dimensions canonical-dimensions)

(def ^:private aliases
  {:model/calls :calls
   :model-calls :calls
   :tool-calls :calls
   :tool/calls :calls
   :model/tokens :tokens
   :model-tokens :tokens
   :tokens :tokens
   :filesystem/bytes :bytes
   :filesystem-bytes :bytes
   :fs-bytes :bytes
   :bytes :bytes
   :money :money-usd-micros
   :money/micros :money-usd-micros
   :micro-usd :money-usd-micros
   :micro-usd/micros :money-usd-micros
   :usd-micros :money-usd-micros
   :money-usd-micros :money-usd-micros})

(defn- fail! [type message data]
  (throw (err/error type message data)))

(defn- safe-long [dimension value]
  (when-not (integer? value)
    (fail! :capability/budget-invalid
           "budget amounts must be integer values"
           {:dimension dimension :value (err/sanitize value)}))
  (when (neg? value)
    (fail! :capability/budget-invalid
           "budget amounts must be non-negative"
           {:dimension dimension :value value}))
  (try
    (long value)
    (catch ArithmeticException _
      (fail! :capability/budget-overflow
             "budget amount exceeds the supported integer range"
             {:dimension dimension :value (err/sanitize value)}))))

(defn canonical-dimension [dimension]
  (let [d (get aliases dimension dimension)]
    (when-not (contains? canonical-dimensions d)
      (fail! :capability/budget-invalid
             "unknown budget dimension"
             {:dimension (err/sanitize dimension)
              :known (vec (sort canonical-dimensions))}))
    d))

(defn canonicalize
  "Return a canonical closed budget map. Unknown keys and invalid amounts fail."
  [budget]
  (when-not (map? budget)
    (fail! :capability/budget-invalid
           "budget must be a map"
           {:value (err/sanitize budget)}))
  (reduce-kv (fn [out k v]
               (let [d (canonical-dimension k)
                     n (safe-long d v)]
                 (if (contains? out d)
                   (fail! :capability/budget-invalid
                          "budget contains duplicate canonical dimensions"
                          {:dimension d})
                   (assoc out d n))))
             {}
             budget))

(defn validate-budget! [budget]
  (canonicalize budget)
  budget)

(defn zero-budget [] {})

(defn- value [budget dimension]
  (long (get budget dimension 0)))

(defn- safe-add [a b data]
  (try
    (Math/addExact (long a) (long b))
    (catch ArithmeticException _
      (fail! :capability/budget-overflow
             "budget arithmetic overflow"
             (merge {:left a :right b} data)))))

(defn- safe-sub [a b data]
  (let [r (- (long a) (long b))]
    (when (neg? r)
      (fail! :capability/budget-exceeded
             "budget balance cannot become negative"
             (merge {:available a :requested b} data)))
    r))

(defn le?
  "True when every child dimension is <= parent. Missing dimensions are zero."
  [parent child]
  (let [p (canonicalize parent)
        c (canonicalize child)]
    (every? (fn [d] (<= (value c d) (value p d))) canonical-dimensions)))

(defn within? [budget amount]
  (let [b (canonicalize budget)
        a (canonicalize amount)]
    (every? (fn [d] (<= (value a d) (value b d))) canonical-dimensions)))

(defn meet
  "Greatest lower bound/attenuation of two finite budgets."
  [parent child]
  (let [p (canonicalize parent)
        c (canonicalize child)]
    (into {}
          (keep (fn [d]
                  (let [n (min (value p d) (value c d))]
                    (when (pos? n) [d n])))
                canonical-dimensions))))

(def attenuate meet)
(def derive-child meet)

(defn remaining [budget reserved consumed]
  (let [b (canonicalize budget)
        r (canonicalize reserved)
        c (canonicalize consumed)]
    (into {}
          (keep (fn [d]
                  (let [n (- (value b d) (value r d) (value c d))]
                    (when (neg? n)
                      (fail! :capability/budget-invalid
                             "reserved and consumed amounts exceed budget"
                             {:dimension d
                              :budget (value b d)
                              :reserved (value r d)
                              :consumed (value c d)}))
                    (when (pos? n) [d n])))
                canonical-dimensions))))

(defn can-reserve? [budget reserved consumed amount]
  (try
    (let [a (canonicalize amount)
          available (remaining budget reserved consumed)]
      (every? (fn [d] (<= (value a d) (value available d)))
              canonical-dimensions))
    (catch clojure.lang.ExceptionInfo e
      (if (= :capability/budget-exceeded (:error/type (ex-data e)))
        false
        (throw e)))))

(defn reserve [budget reserved consumed amount]
  (let [b (canonicalize budget)
        r (canonicalize reserved)
        c (canonicalize consumed)
        a (canonicalize amount)]
    (when-not (can-reserve? b r c a)
      (fail! :capability/budget-exceeded
             "budget reservation exceeds available balance"
             {:budget b :reserved r :consumed c :requested a}))
    (reduce (fn [m d]
              (let [n (safe-add (value r d) (value a d) {:dimension d})]
                (if (pos? n) (assoc m d n) m)))
            {}
            canonical-dimensions)))

(defn settle [reserved consumed reservation actual]
  (let [r (canonicalize reserved)
        c (canonicalize consumed)
        q (canonicalize reservation)
        a (canonicalize actual)]
    (when-not (within? q a)
      (fail! :capability/budget-invalid
             "settlement cannot consume more than reserved"
             {:reserved q :actual a}))
    {:reserved (reduce (fn [m d]
                         (let [n (safe-sub (value r d) (value q d) {:dimension d})]
                           (if (pos? n) (assoc m d n) m)))
                       {}
                       canonical-dimensions)
     :consumed (reduce (fn [m d]
                         (let [n (safe-add (value c d) (value a d) {:dimension d})]
                           (if (pos? n) (assoc m d n) m)))
                       {}
                       canonical-dimensions)}))

(defn release [reserved amount]
  (let [r (canonicalize reserved)
        a (canonicalize amount)]
    (when-not (within? r a)
      (fail! :capability/budget-invalid
             "release cannot exceed reserved balance"
             {:reserved r :release a}))
    (reduce (fn [m d]
              (let [n (safe-sub (value r d) (value a d) {:dimension d})]
                (if (pos? n) (assoc m d n) m)))
            {}
            canonical-dimensions)))

(defn reallocate
  "Move only currently unreserved/unconsumed amounts from one allocation to another."
  [from-state to-state amount]
  (let [a (canonicalize amount)
        available (remaining (:budget from-state)
                             (:reserved from-state)
                             (:consumed from-state))]
    (when-not (within? available a)
      (fail! :capability/budget-reallocation-invalid
             "reallocation exceeds unreserved, unconsumed balance"
             {:available available :requested a}))
    {:from (update from-state :budget #(reduce (fn [m d]
                                                 (let [n (safe-sub (value % d) (value a d)
                                                                   {:dimension d})]
                                                   (if (pos? n) (assoc m d n) m)))
                                               {}
                                               canonical-dimensions))
     :to (update to-state :budget #(reduce (fn [m d]
                                             (let [n (safe-add (value % d) (value a d)
                                                               {:dimension d})]
                                               (if (pos? n) (assoc m d n) m)))
                                           {}
                                           canonical-dimensions))}))

(defn usage-for
  "Map one provider operation to budget dimensions. Call count is charged
  separately by the intent pipeline; nil dimensions are omitted."
  [{:keys [model? tokens money-usd-micros calls bytes filesystem-bytes]}]
  (cond-> {}
    (or model? (some? calls)) (assoc :calls (or calls 1))
    (some? tokens) (assoc :tokens tokens)
    (some? money-usd-micros) (assoc :money-usd-micros money-usd-micros)
    (or (some? bytes) (some? filesystem-bytes))
    (assoc :bytes (or bytes filesystem-bytes))))
