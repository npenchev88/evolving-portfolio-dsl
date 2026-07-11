(ns portfolio-evolution.test-runner
  (:require
   [clojure.test :as test]
   [portfolio-evolution.baseline-ga-test]
   [portfolio-evolution.core-test]
   [portfolio-evolution.experiment-test]
   [portfolio-evolution.knapsack-test]
   [portfolio-evolution.metrics-test]
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
         'portfolio-evolution.experiment-test)]

    (shutdown-agents)

    (when (pos? (+ fail error))
      (System/exit 1))))
