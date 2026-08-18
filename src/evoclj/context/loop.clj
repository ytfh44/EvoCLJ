(ns evoclj.context.loop
  (:require [evoclj.context.error :as err]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.apply :as apply]
            [evoclj.context.idempotency :as idempotency]
            [evoclj.context.compacter :as compacter]
            [evoclj.context.registry :as registry]
            [evoclj.context.footer :as footer]
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
             new-context (apply/apply-envelope merged-envelope f)]
         {:envelope merged-envelope
          :footer f
          :context new-context})
       (let [result (compacter/compress compacter context-str opts)
             env (:envelope result)
             f (:footer result)
             new-context (apply/apply-envelope env f)]
         {:envelope env
          :footer f
          :context new-context})))))

(defn compress-and-apply
  ([context-str compacter]
   (compress-and-apply context-str compacter {}))
  ([context-str compacter opts]
   (:context (recompress! context-str compacter opts))))


