(ns portfolio-evolution.random-search-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.random-search :as random-search]))

(def known-instance
  {:id
   "random-search-test"

   :assets
   [{:id "a0"
     :cost 4
     :expected-profit 8}

    {:id "a1"
     :cost 5
     :expected-profit 11}

    {:id "a2"
     :cost 6
     :expected-profit 13}

    {:id "a3"
     :cost 7
     :expected-profit 14}

    {:id "a4"
     :cost 8
     :expected-profit 15}]

   :budget
   15})

(def smoke-config
  {:selection-probability 0.30
   :evaluation-budget 40})

(def deterministic-keys
  [:algorithm
   :instance-id
   :seed
   :config
   :best-solution
   :best-objective
   :best-cost
   :selected-asset-count
   :feasible?
   :fitness-evaluations-used
   :best-found-at-evaluation
   :raw-feasibility-rate
   :feasibility-rate])

(deftest random-search-respects-the-evaluation-budget
  (let [result
        (random-search/run
         known-instance
         1
         smoke-config)]

    (is (= 40
           (:fitness-evaluations-used
            result)))

    (is (<= 1
            (:best-found-at-evaluation
             result)
            40))))

(deftest random-search-returns-a-feasible-solution
  (let [result
        (random-search/run
         known-instance
         2
         smoke-config)

        evaluation
        (knapsack/evaluate-solution
         known-instance
         (:best-solution
          result))]

    (is (true?
         (:feasible?
          result)))

    (is (true?
         (:feasible?
          evaluation)))

    (is (= 1.0
           (:feasibility-rate
            result)))))

(deftest random-search-is-reproducible
  (let [result-a
        (random-search/run
         known-instance
         42
         smoke-config)

        result-b
        (random-search/run
         known-instance
         42
         smoke-config)]

    (is (=
         (select-keys
          result-a
          deterministic-keys)

         (select-keys
          result-b
          deterministic-keys)))))
