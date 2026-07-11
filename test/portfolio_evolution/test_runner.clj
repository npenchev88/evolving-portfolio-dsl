(ns portfolio-evolution.test-runner
  (:require
   [clojure.test :as test]
   [portfolio-evolution.core-test]))

(defn -main
  [& _]
  (let [{:keys [fail error]}
        (test/run-tests
         'portfolio-evolution.core-test)]
    (shutdown-agents)
    (when (pos? (+ fail error))
      (System/exit 1))))
