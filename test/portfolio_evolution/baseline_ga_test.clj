(ns portfolio-evolution.baseline-ga-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.baseline-ga :as baseline-ga]))

(def known-instance
  {:id "baseline-known-instance"
   :assets
   [{:id "asset-0"
     :cost 6
     :expected-profit 6}

    {:id "asset-1"
     :cost 5
     :expected-profit 15}

    {:id "asset-2"
     :cost 4
     :expected-profit 8}]
   :budget 9})

(def smoke-config
  {:population-size 10
   :tournament-size 2
   :crossover-probability 0.7
   :elite-count 1
   :evaluation-budget 60})

(deftest ratio-repair-test
  (testing "The lowest-ratio asset is removed first"
    ;; Ratios:
    ;; asset 0 = 1.0
    ;; asset 1 = 3.0
    ;; asset 2 = 2.0
    ;;
    ;; All assets cost 15 in total, while the budget is 9.
    ;; Removing asset 0 produces the feasible solution [0 1 1].
    (is (= [0 1 1]
           (baseline-ga/ratio-repair
            known-instance
            [1 1 1])))))

(deftest evaluation-budget-test
  (testing "The GA uses exactly the configured fitness budget"
    (let [result
          (baseline-ga/run
           known-instance
           1
           smoke-config)]

      (is (= 60
             (:fitness-evaluations-used result)))

      (is (<=
           (:best-found-at-evaluation result)
           60))

      (is (true?
           (:feasible? result)))

      (is (= 1.0
             (:feasibility-rate result))))))

(deftest convergence-is-monotonic-test
  (testing "The best objective never decreases"
    (let [result
          (baseline-ga/run
           known-instance
           1
           smoke-config)

          objectives
          (map :best-objective
               (:convergence result))]

      (is (apply <= objectives)))))

(deftest reproducibility-test
  (testing "The same seed produces the same stochastic trajectory"
    (let [result-a
          (baseline-ga/run
           known-instance
           42
           smoke-config)

          result-b
          (baseline-ga/run
           known-instance
           42
           smoke-config)

          reproducible-keys
          [:best-solution
           :best-objective
           :best-cost
           :selected-asset-count
           :fitness-evaluations-used
           :best-found-at-evaluation
           :raw-feasibility-rate
           :feasibility-rate
           :convergence]]

      ;; Runtime is intentionally excluded.
      (is (=
           (select-keys result-a
                        reproducible-keys)
           (select-keys result-b
                        reproducible-keys))))))
