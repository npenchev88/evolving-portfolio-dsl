(ns portfolio-evolution.knapsack-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.knapsack :as knapsack]))

(def known-instance
  {:id "known-instance"
   :assets
   [{:id "asset-0"
     :cost 10
     :expected-profit 60}

    {:id "asset-1"
     :cost 20
     :expected-profit 100}

    {:id "asset-2"
     :cost 30
     :expected-profit 120}]

   :budget 50})

(deftest evaluate-solution-test
  (testing "A feasible solution is evaluated correctly"
    (let [result
          (knapsack/evaluate-solution
           known-instance
           [1 1 0])]

      (is (= 30 (:cost result)))
      (is (= 160 (:objective result)))
      (is (= [0 1] (:selected-indices result)))
      (is (= 2 (:selected-asset-count result)))
      (is (true? (:feasible? result)))))

  (testing "An infeasible solution is identified"
    (let [result
          (knapsack/evaluate-solution
           known-instance
           [1 1 1])]

      (is (= 60 (:cost result)))
      (is (= 280 (:objective result)))
      (is (false? (:feasible? result))))))

(deftest exact-solver-test
  (testing "Dynamic programming finds the known optimum"
    (let [result
          (knapsack/exact-solve known-instance)]

      ;; Selecting assets 1 and 2:
      ;; cost = 20 + 30 = 50
      ;; profit = 100 + 120 = 220
      (is (= 220 (:objective result)))
      (is (= 220 (:optimal-objective result)))
      (is (= 50 (:cost result)))
      (is (= [1 2] (:selected-indices result)))
      (is (= [0 1 1] (:solution result)))
      (is (true? (:feasible? result)))
      (is (true? (:optimal? result))))))
