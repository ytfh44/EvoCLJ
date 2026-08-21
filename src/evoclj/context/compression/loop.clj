(ns evoclj.context.compression.loop
  (:require [evoclj.context.compression.error :as err]
            [evoclj.context.compression.envelope :as envelope]
            [evoclj.context.compression.apply :as apply]
            [evoclj.context.compression.idempotency :as idempotency]
            [evoclj.context.compression.compacter :as compacter]
            [evoclj.context.compression.registry :as registry]
            [evoclj.context.compression.footer :as footer]
            [clojure.string :as str]))

(defn read-envelope-prefix
  [s]
  (when-not (string? s)
    (throw (err/error :context/compression-invalid
                      "input must be a string"
                      {:value (err/sanitize s)})))
  (let [[_ end-idx]
        (loop [depth 0 i 0 in-string false]
          (if (>= i (count s))
            [nil i]
            (let [ch (nth s i)]
              (cond
                in-string
                (cond
                  (= ch \\) (recur depth (+ i 2) in-string)
                  (= ch \") (recur depth (inc i) false)
                  :else (recur depth (inc i) in-string))

                (= ch \") (recur depth (inc i) true)
                (= ch \() (recur (inc depth) (inc i) in-string)
                (= ch \)) (if (zero? depth)
                            [nil i]
                            (recur (dec depth) (inc i) in-string))
                (= ch \{) (recur (inc depth) (inc i) in-string)
                (= ch \}) (if (= 1 depth)
                            [nil (inc i)]
                            (recur (dec depth) (inc i) in-string))
                :else (recur depth (inc i) in-string)))))
        prefix (subs s 0 end-idx)
        rest-str (subs s end-idx)]
    (try
      (let [parsed (clojure.edn/read-string prefix)]
        (if (map? parsed)
          {:envelope parsed
           :rest rest-str}
          (throw (err/error :context/compression-invalid
                            "envelope prefix did not parse to a map"
                            {:type (type parsed)
                             :raw (err/sanitize (subs s 0 (min 200 (count s))))}))))
      (catch Exception e
        (throw (err/error :context/compression-invalid
                          (str "failed to read envelope prefix: "
                               (.getMessage e))
                          {:raw (err/sanitize (subs s 0 (min 200 (count s))))}))))))

(defn- extract-fresh-tail-from-rest
  [rest-str]
  (let [trimmed (str/triml rest-str)]
    (if (str/starts-with? trimmed "[CONTEXT COMPRESSION]")
      (let [parts (str/split trimmed #"\n\n" 3)]
        (cond
          (>= (count parts) 3) (nth parts 2)
          (>= (count parts) 2) (nth parts 1)
          :else ""))
      rest-str)))

(defn extract-envelope
  [context-str]
  (when-not (string? context-str)
    (throw (err/error :context/compression-invalid
                      "context-str must be a string"
                      {:value (err/sanitize context-str)})))
  (if (str/blank? context-str)
    {:envelope nil :fresh-tail ""}
    (try
      (let [{:keys [envelope rest]} (read-envelope-prefix context-str)]
        (if (and (map? envelope) (:envelope/version envelope))
          (let [fresh-tail (extract-fresh-tail-from-rest rest)]
            {:envelope envelope
             :fresh-tail fresh-tail})
          {:envelope nil :fresh-tail context-str}))
      (catch Exception _
        {:envelope nil :fresh-tail context-str}))))

(defn extract-fresh-tail
  [context-str]
  (:fresh-tail (extract-envelope context-str)))

(defn- task->section
  [task]
  (when task
    (cond-> {:task/id (:task/id task)
             :task/status (:task/status task)
             :task/description (:task/description task)}
      (some? (:task/owner task)) (assoc :task/owner (:task/owner task)))))

(defn- subgoal->section
  [sg]
  (cond-> {:subgoal/id (:subgoal/id sg)
           :subgoal/status (:subgoal/status sg)
           :subgoal/description (:subgoal/description sg)}
    (some? (:subgoal/parent sg)) (assoc :subgoal/parent (:subgoal/parent sg))))

(defn- noop-envelope?
  "True when `envelope` represents a noop compression (no actual
  compression happened). Detected by the task id being \"noop\" or
  tokens-before equaling tokens-after."
  [envelope]
  (or (= (:envelope/task envelope) "noop")
      (= (:envelope/tokens-before envelope) (:envelope/tokens-after envelope))))

(defn- compute-savings
  "Compute token savings from an envelope: tokens-before minus tokens-after."
  [envelope]
  (max 0 (- (:envelope/tokens-before envelope 0)
            (:envelope/tokens-after envelope 0)))) 

(defn recompress!
  ([context-str compacter]
   (recompress! context-str compacter {}))
  ([context-str compacter opts]
   {:pre [(string? context-str)
          (satisfies? compacter/Compacter compacter)]}
   (let [opts (or opts {})
         extracted (extract-envelope context-str)
         old-envelope (:envelope extracted)
         fresh-tail (:fresh-tail extracted)]
     (if old-envelope
       (let [structured-sections {:tasks [(task->section (:envelope/task old-envelope))]
                                  :subgoals (map subgoal->section (:envelope/subgoals old-envelope []))}
             compacter-opts (cond-> opts
                               true (assoc :previous-envelope old-envelope)
                               true (assoc :structured-sections structured-sections))
             result (compacter/compress compacter fresh-tail compacter-opts)
             new-envelope (:envelope result)
             merged-envelope (idempotency/idempotent-merge old-envelope new-envelope)
             footer-opts (assoc opts :archiver-reports (registry/archiver-reports))
             f (footer/build-footer merged-envelope footer-opts)
             trigger-result (:trigger result)
             last-savings (or (when trigger-result
                                (:trigger/last-savings trigger-result))
                              (compute-savings merged-envelope))
             new-context (if (noop-envelope? merged-envelope)
                           context-str
                           (apply/apply-envelope merged-envelope fresh-tail))]
         {:envelope merged-envelope
          :footer f
          :context new-context
          :trigger/last-savings last-savings})
       (let [result (compacter/compress compacter context-str opts)
             env (:envelope result)
             f (:footer result)
             trigger-result (:trigger result)
             last-savings (or (when trigger-result
                                (:trigger/last-savings trigger-result))
                              (compute-savings env))
             new-context (if (noop-envelope? env)
                           context-str
                           (apply/apply-envelope env fresh-tail))]
         {:envelope env
          :footer f
          :context new-context
          :trigger/last-savings last-savings})))))

(defn compress-and-apply
  ([context-str compacter]
   (compress-and-apply context-str compacter {}))
  ([context-str compacter opts]
   (:context (recompress! context-str compacter opts))))
