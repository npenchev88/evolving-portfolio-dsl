(ns portfolio-evolution.test-runner
  (:require
   [clojure.test :as test]
   [portfolio-evolution.baseline-ga-test]
   [portfolio-evolution.core-test]
   [portfolio-evolution.dsl-test]
   [portfolio-evolution.experiment-test]
   [portfolio-evolution.final-experiment-test]
   [portfolio-evolution.held-out-evaluation-test]
   [portfolio-evolution.knapsack-test]
   [portfolio-evolution.metrics-test]
   [portfolio-evolution.optimizer-runtime-test]
   [portfolio-evolution.outer-evolution-test]
   [portfolio-evolution.outer-gp-test]
   [portfolio-evolution.program-generation-test]
   [portfolio-evolution.random-search-test]
   [portfolio-evolution.scale-evaluation-test]
   [portfolio-evolution.synthetic-data-test]))

(defn -main
  [& _]
  (let [{:keys [fail error]}
        (test/run-tests
         'portfolio-evolution.core-test
         'portfolio-evolution.knapsack-test
         'portfolio-evolution.metrics-test
         'portfolio-evolution.synthetic-data-test
         'portfolio-evolution.baseline-ga-test
         'portfolio-evolution.random-search-test
         'portfolio-evolution.experiment-test
         'portfolio-evolution.dsl-test
         'portfolio-evolution.optimizer-runtime-test
         'portfolio-evolution.program-generation-test
         'portfolio-evolution.outer-gp-test
         'portfolio-evolution.outer-evolution-test
         'portfolio-evolution.held-out-evaluation-test
         'portfolio-evolution.scale-evaluation-test
         'portfolio-evolution.final-experiment-test)]

    (shutdown-agents)

    (when (pos?
           (+ fail error))
      (System/exit 1))))
