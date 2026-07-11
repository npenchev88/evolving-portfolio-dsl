(ns portfolio-evolution.generation-demo
  (:require
   [clojure.pprint :as pprint]
   [portfolio-evolution.core :as core]
   [portfolio-evolution.dsl :as dsl]
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.metrics :as metrics]
   [portfolio-evolution.optimizer-runtime :as runtime]
   [portfolio-evolution.program-generation :as generation]
   [portfolio-evolution.rng :as rng]
   [portfolio-evolution.synthetic-data :as synthetic-data])
  (:gen-class))

(defn- evaluate-optimizer
  [instance
   optimal-objective
   master-seed
   optimizer-index
   optimizer]

  (let [algorithm-seed
        (rng/derive-seed
         master-seed
         optimizer-index)

        result
        (runtime/run
         instance
         optimizer
         algorithm-seed
         {:evaluation-budget 500})

        score
        (metrics/score
         (:best-objective result)
         optimal-objective)]

    {:result result
     :score score}))

(defn- print-optimizer
  [label optimizer evaluation]
  (let [result
        (:result evaluation)

        score
        (:score evaluation)]

    (println label)
    (println "  Type:"
             (dsl/infer-type optimizer))
    (println "  Nodes:"
             (dsl/program-node-count
              optimizer))
    (println "  Depth:"
             (dsl/program-depth
              optimizer))
    (println "  Objective:"
             (:best-objective result))
    (println "  Normalized score:"
             (format
              "%.6f"
              (:normalized-score score)))
    (println "  Optimality gap:"
             (format
              "%.6f"
              (:optimality-gap score)))
    (println "  Evaluations:"
             (:fitness-evaluations-used
              result))
    (println "  Valid:"
             (dsl/valid?
              optimizer))
    (println)
    (pprint/pprint
     optimizer)
    (println)))

(defn -main
  [& args]
  (let [{:keys [seed]}
        (core/parse-args args)

        generator-random
        (rng/create seed)

        original
        (generation/generate-optimizer
         generator-random)

        mutation-one
        (generation/mutate
         generator-random
         original)

        mutation-two
        (generation/mutate
         generator-random
         mutation-one)

        dataset
        (synthetic-data/generate-dataset)

        instance
        (first
         (:test dataset))

        exact-result
        (knapsack/exact-solve
         instance)

        optimal-objective
        (:optimal-objective
         exact-result)]

    (println "Random Typed Optimizer Generation")
    (println "Master seed:" seed)
    (println "Instance:" (:id instance))
    (println "Exact optimum:"
             optimal-objective)
    (println)

    (print-optimizer
     "Original generated optimizer"
     original
     (evaluate-optimizer
      instance
      optimal-objective
      seed
      0
      original))

    (print-optimizer
     "Mutation 1"
     mutation-one
     (evaluate-optimizer
      instance
      optimal-objective
      seed
      1
      mutation-one))

    (print-optimizer
     "Mutation 2"
     mutation-two
     (evaluate-optimizer
      instance
      optimal-objective
      seed
      2
      mutation-two))))
