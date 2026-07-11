(ns portfolio-evolution.dsl-demo
  (:require
   [clojure.pprint :as pprint]
   [portfolio-evolution.core :as core]
   [portfolio-evolution.dsl :as dsl]
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.metrics :as metrics]
   [portfolio-evolution.optimizer-runtime :as runtime]
   [portfolio-evolution.synthetic-data :as synthetic-data])
  (:gen-class))

(defn -main
  [& args]
  (let [{:keys [seed]}
        (core/parse-args
         args)

        dataset
        (synthetic-data/generate-dataset)

        instance
        (first
         (:test dataset))

        optimizer
        dsl/example-optimizer

        exact-result
        (knapsack/exact-solve
         instance)

        optimizer-result
        (runtime/run
         instance
         optimizer
         seed)

        result-metrics
        (metrics/score
         (:best-objective
          optimizer-result)
         (:optimal-objective
          exact-result))]

    (println "Typed S-expression Portfolio Optimizer")
    (println)

    (println "Validated DSL type:"
             (dsl/infer-type
              optimizer))

    (println "Program nodes:"
             (dsl/program-node-count
              optimizer))

    (println "Program depth:"
             (dsl/program-depth
              optimizer))

    (println)
    (println "Optimizer program")
    (pprint/pprint
     optimizer)

    (println)
    (println "Held-out instance")
    (println "  ID:"
             (:id instance))
    (println "  Assets:"
             (count
              (:assets instance)))
    (println "  Budget:"
             (:budget instance))

    (println)
    (println "Exact DP oracle")
    (println "  Optimal objective:"
             (:optimal-objective
              exact-result))

    (println)
    (println "DSL optimizer")
    (println "  Seed:"
             seed)
    (println "  Best objective:"
             (:best-objective
              optimizer-result))
    (println "  Used capital:"
             (:best-cost
              optimizer-result))
    (println "  Selected assets:"
             (:selected-asset-count
              optimizer-result))
    (println "  Evaluations:"
             (:fitness-evaluations-used
              optimizer-result))
    (println "  Best found at evaluation:"
             (:best-found-at-evaluation
              optimizer-result))
    (println "  Final population size:"
             (:final-population-size
              optimizer-result))
    (println "  Feasible:"
             (:feasible?
              optimizer-result))
    (println "  Raw feasibility rate:"
             (format
              "%.4f"
              (:raw-feasibility-rate
               optimizer-result)))
    (println "  Post-repair feasibility rate:"
             (format
              "%.4f"
              (:feasibility-rate
               optimizer-result)))
    (println "  Runtime ms:"
             (format
              "%.3f"
              (:runtime-ms
               optimizer-result)))
    (println "  Normalized score:"
             (format
              "%.6f"
              (:normalized-score
               result-metrics)))
    (println "  Optimality gap:"
             (format
              "%.6f"
              (:optimality-gap
               result-metrics)))))
