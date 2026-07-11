(ns portfolio-evolution.optimizer-runtime-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.optimizer-runtime :as runtime]))

(def known-instance
  {:id "dsl-known-instance"

   :assets
   [{:id "asset-0"
     :cost 2
     :expected-profit 6}

    {:id "asset-1"
     :cost 3
     :expected-profit 7}

    {:id "asset-2"
     :cost 4
     :expected-profit 9}

    {:id "asset-3"
     :cost 5
     :expected-profit 10}

    {:id "asset-4"
     :cost 6
     :expected-profit 11}]

   :budget 10})

(def test-optimizer
  '(optimizer
     (mixed-population
       6
       4
       return-per-cost)

     ratio-repair

     (repeat-until-budget
       (sequence
         (select-best 5)

         (local-search
           (drop-add
             return-per-cost))

         (transform
           (swap-assets))))))

(def smoke-config
  {:evaluation-budget 60})

(def deterministic-keys
  [:algorithm
   :instance-id
   :seed
   :program
   :program-node-count
   :program-depth
   :config
   :best-solution
   :best-objective
   :best-cost
   :selected-asset-count
   :feasible?
   :fitness-evaluations-used
   :best-found-at-evaluation
   :program-steps
   :raw-feasibility-rate
   :feasibility-rate
   :final-population-size
   :convergence])

(deftest interpreter-respects-evaluation-budget
  (let [result
        (runtime/run
         known-instance
         test-optimizer
         1
         smoke-config)]

    (testing "Exactly the configured number of evaluations is used"
      (is (= 60
             (:fitness-evaluations-used
              result))))

    (testing "The final solution is feasible"
      (is (true?
           (:feasible?
            result)))

      (is (= 1.0
             (:feasibility-rate
              result))))

    (testing "The best solution was found inside the evaluation budget"
      (is (<= 1
              (:best-found-at-evaluation
               result)
              60)))))

(deftest interpreter-never-exceeds-the-exact-optimum
  (let [exact-result
        (knapsack/exact-solve
         known-instance)

        runtime-result
        (runtime/run
         known-instance
         test-optimizer
         1
         smoke-config)]

    (is (<= (:best-objective
             runtime-result)
            (:optimal-objective
             exact-result)))))

(deftest same-seed-produces-the-same-program-trajectory
  (let [result-a
        (runtime/run
         known-instance
         test-optimizer
         42
         smoke-config)

        result-b
        (runtime/run
         known-instance
         test-optimizer
         42
         smoke-config)]

    ;; Runtime is intentionally excluded.
    (is (=
         (select-keys
          result-a
          deterministic-keys)

         (select-keys
          result-b
          deterministic-keys)))))

(deftest invalid-programs-are-not-executed
  (is (thrown?
       clojure.lang.ExceptionInfo

       (runtime/run
        known-instance

        '(optimizer
           (random-population 10)
           ratio-repair
           (System/exit 0))

        1
        smoke-config))))

(deftest non-progressing-repeat-is-rejected-at-runtime
  (is (thrown?
       clojure.lang.ExceptionInfo

       (runtime/run
        known-instance

        '(optimizer
           (random-population 10)
           ratio-repair

           (repeat-until-budget
             (select-best 5)))

        1
        smoke-config))))
