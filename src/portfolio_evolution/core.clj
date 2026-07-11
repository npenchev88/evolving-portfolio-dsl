(ns portfolio-evolution.core
  (:require
   [portfolio-evolution.baseline-ga :as baseline-ga]
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

      (let [[flag value & more]
            remaining]

        (case flag
          "--seed"
          (do
            (when-not value
              (throw
               (ex-info "--seed requires an integer value."
                        {:arguments args})))

            (recur
             more
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

        test-instance
        (first (:test dataset))

        exact-result
        (knapsack/exact-solve
         test-instance)

        baseline-result
        (baseline-ga/run
         test-instance
         seed)

        result-metrics
        (metrics/score
         (:best-objective baseline-result)
         (:optimal-objective exact-result))]

    (println "Evolving Portfolio DSL")
    (println "Algorithm seed:" seed)
    (println)

    (println "Held-out test instance")
    (println "  ID:" (:id test-instance))
    (println "  Assets:" (count (:assets test-instance)))
    (println "  Budget:" (:budget test-instance))
    (println)

    (println "Exact DP oracle")
    (println "  Optimal objective:"
             (:optimal-objective exact-result))
    (println "  Selected assets:"
             (:selected-asset-count exact-result))
    (println)

    (println "Baseline GA")
    (println "  Best objective:"
             (:best-objective baseline-result))
    (println "  Used capital:"
             (:best-cost baseline-result))
    (println "  Selected assets:"
             (:selected-asset-count baseline-result))
    (println "  Feasible:"
             (:feasible? baseline-result))
    (println "  Evaluations:"
             (:fitness-evaluations-used
              baseline-result))
    (println "  Best first found at evaluation:"
             (:best-found-at-evaluation
              baseline-result))
    (println "  Runtime ms:"
             (format "%.3f"
                     (:runtime-ms baseline-result)))
    (println "  Raw feasibility rate:"
             (format "%.4f"
                     (:raw-feasibility-rate
                      baseline-result)))
    (println "  Post-repair feasibility rate:"
             (format "%.4f"
                     (:feasibility-rate
                      baseline-result)))
    (println "  Normalized score:"
             (format "%.6f"
                     (:normalized-score
                      result-metrics)))
    (println "  Optimality gap:"
             (format "%.6f"
                     (:optimality-gap
                      result-metrics)))))
