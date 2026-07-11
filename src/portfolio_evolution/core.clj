(ns portfolio-evolution.core
  (:require
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.metrics :as metrics]
   [portfolio-evolution.synthetic-data :as synthetic-data])
  (:gen-class))

(defn parse-long-argument
  [value]
  (try
    (Long/parseLong value)
    (catch NumberFormatException _
      (throw
       (ex-info "Expected an integer value."
                {:value value})))))

(defn parse-args
  [args]
  (loop [remaining args
         config {:seed 1}]

    (if (empty? remaining)
      config

      (let [[flag value & more] remaining]
        (case flag
          "--seed"
          (do
            (when-not value
              (throw
               (ex-info "--seed requires an integer value."
                        {:arguments args})))

            (recur more
                   (assoc config
                          :seed
                          (parse-long-argument value))))

          (throw
           (ex-info "Unknown command-line argument."
                    {:argument flag
                     :arguments args})))))))

(defn -main
  [& args]
  (let [{:keys [seed]}
        (parse-args args)

        dataset
        (synthetic-data/generate-dataset)

        first-training-instance
        (first (:train dataset))

        exact-result
        (knapsack/exact-solve first-training-instance)

        exact-metrics
        (metrics/score
         (:objective exact-result)
         (:optimal-objective exact-result))]

    (println "Evolving Portfolio DSL")
    (println "Clojure version:" (clojure-version))
    (println "Algorithm seed:" seed)
    (println)

    (println "Dataset")
    (println "  Training instances:" (count (:train dataset)))
    (println "  Test instances:" (count (:test dataset)))
    (println "  Assets per instance:"
             (count (:assets first-training-instance)))
    (println "  Train data seed:"
             (get-in dataset [:config :train-data-seed]))
    (println "  Test data seed:"
             (get-in dataset [:config :test-data-seed]))
    (println)

    (println "First training instance")
    (println "  ID:" (:id first-training-instance))
    (println "  Budget:" (:budget first-training-instance))
    (println "  Exact objective:" (:objective exact-result))
    (println "  Selected assets:"
             (:selected-asset-count exact-result))
    (println "  Used capital:" (:cost exact-result))
    (println "  Feasible:" (:feasible? exact-result))
    (println "  Normalized score:"
             (:normalized-score exact-metrics))
    (println "  Optimality gap:"
             (:optimality-gap exact-metrics))))
