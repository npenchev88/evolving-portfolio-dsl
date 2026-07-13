(ns portfolio-evolution.greedy-baseline-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.greedy-baseline :as greedy]
   [portfolio-evolution.knapsack :as knapsack]))

(def counterexample-instance
  {:id "greedy-counterexample"
   :budget 50
   :assets
   [{:id "a" :cost 10 :expected-profit 60}
    {:id "b" :cost 20 :expected-profit 100}
    {:id "c" :cost 30 :expected-profit 120}]})

(deftest greedy-baseline-is-deterministic-and-feasible
  (let [first-run
        (greedy/run counterexample-instance)

        second-run
        (greedy/run counterexample-instance)]

    (testing "The same instance produces the same portfolio"
      (is (= (:best-solution first-run)
             (:best-solution second-run))))

    (testing "The constructed portfolio is feasible"
      (is (:feasible? first-run))
      (is (<= (:best-cost first-run)
              (:budget counterexample-instance))))

    (testing "Greedy only evaluates one constructed portfolio"
      (is (= 1
             (:fitness-evaluations-used first-run))))))

(deftest greedy-baseline-does-not-use-the-exact-oracle
  (let [greedy-result
        (greedy/run counterexample-instance)

        exact-result
        (knapsack/exact-solve counterexample-instance)]

    (testing "The standard ratio heuristic may be suboptimal for 0/1 knapsack"
      (is (= [1 1 0]
             (:best-solution greedy-result)))

      (is (= 160
             (:best-objective greedy-result)))

      (is (= 220
             (:optimal-objective exact-result)))

      (is (< (:best-objective greedy-result)
             (:optimal-objective exact-result))))))

(deftest ratio-order-matches-the-dsl-initializer-tie-break
  (let [instance
        {:id "ratio-tie"
         :budget 10
         :assets
         [{:id "first" :cost 10 :expected-profit 20}
          {:id "second" :cost 5 :expected-profit 10}]}

        result
        (greedy/run instance)]

    (testing "Equal ratios are resolved by original asset index"
      (is (= [1 0]
             (:best-solution result))))))
