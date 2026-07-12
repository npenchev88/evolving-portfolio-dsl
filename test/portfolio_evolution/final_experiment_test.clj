(ns portfolio-evolution.final-experiment-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.final-experiment :as experiment]))

(def synthetic-seed-summaries
  [{:seed 1

    :outer
    {:fitness-improvement 0.02
     :training-score-improvement 0.01
     :node-reduction 5
     :depth-reduction 1}

    :sizes
    [{:asset-count 50
      :random-mean-score 0.70
      :ga-mean-score 0.85
      :initial-dsl-mean-score 0.95
      :final-dsl-mean-score 0.97
      :final-minus-random 0.27
      :final-minus-ga 0.12
      :final-minus-initial 0.02}]}

   {:seed 2

    :outer
    {:fitness-improvement 0.04
     :training-score-improvement 0.03
     :node-reduction 7
     :depth-reduction 3}

    :sizes
    [{:asset-count 50
      :random-mean-score 0.72
      :ga-mean-score 0.87
      :initial-dsl-mean-score 0.96
      :final-dsl-mean-score 0.99
      :final-minus-random 0.27
      :final-minus-ga 0.12
      :final-minus-initial 0.03}]}])

(deftest seed-level-results-are-aggregated
  (let [aggregate
        (experiment/aggregate-seed-summaries
         synthetic-seed-summaries)

        size-result
        (first
         (:by-size aggregate))]

    (testing "Seed count is preserved"
      (is (= 2
             (:seed-count
              aggregate))))

    (testing "Means are calculated across seeds"
      (is (= 0.71
             (:random-score-mean
              size-result)))

      (is (= 0.86
             (:ga-score-mean
              size-result)))

      (is (= 0.98
             (:final-dsl-score-mean
              size-result))))

    (testing "Paired differences are aggregated directly"
      (is (= 0.27
             (:final-minus-random-mean
              size-result)))

      (is (= 0.12
             (:final-minus-ga-mean
              size-result))))

    (testing "Program simplification is summarized"
      (is (= 6.0
             (get-in aggregate
                     [:outer
                      :node-reduction
                      :mean]))))))

(def tiny-config
  (merge
   experiment/default-config

   {:outer-config
    {:outer-population-size 4
     :outer-generations 1
     :outer-tournament-size 2
     :elite-count 1
     :max-offspring-attempts 1000

     :training-instance-count 2
     :inner-evaluation-budget 20
     :parsimony-penalty-coefficient 0.001

     :dataset-config
     {:train-instance-count 2
      :test-instance-count 1
      :asset-count 8}

     :generator-config
     {:max-depth 7
      :population-size 10
      :greedy-counts [2 4]
      :selection-counts [3 5 8]
      :injection-counts [2 3 5]
      :stagnation-thresholds [2 3]
      :diversity-thresholds [0.2 0.4]
      :budget-utilization-thresholds [0.7 0.9]
      :scorer-scalars [0.2 0.5 1.0]}}

    :scale-config
    {:asset-counts [8 12]
     :test-instance-count 1
     :evaluation-budget 20
     :base-test-data-seed 123

     :random-search-config
     {:selection-probability 0.30}

     :baseline-ga-config
     {:population-size 10
      :tournament-size 2
      :elite-count 1}}}))

(deftest one-seed-pipeline-runs-end-to-end
  (let [result
        (experiment/run-one-seed
         1
         tiny-config)]

    (is (= 1
           (:seed result)))

    (is (= [8 12]
           (mapv :asset-count
                 (:sizes result))))

    (is
     (every?
      number?
      (map :final-dsl-mean-score
           (:sizes result))))

    (is
     (seq?
      (get-in result
              [:outer
               :final
               :program])))))
