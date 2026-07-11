(ns portfolio-evolution.outer-gp-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.dsl :as dsl]
   [portfolio-evolution.outer-gp :as outer-gp]))

(def smoke-config
  {:outer-population-size 6
   :training-instance-count 3
   :inner-evaluation-budget 40
   :parsimony-penalty-coefficient 0.001

   :dataset-config
   {:train-instance-count 4
    :test-instance-count 2
    :asset-count 10}

   :generator-config
   {:max-depth 7
    :population-size 10
    :greedy-counts [2 4]
    :selection-counts [3 5 8]
    :injection-counts [2 3 5]
    :stagnation-thresholds [2 3 4]
    :diversity-thresholds [0.20 0.40]
    :budget-utilization-thresholds [0.70 0.90]
    :scorer-scalars [0.2 0.5 1.0]}})

(defn- remove-runtime-values
  [result]
  (-> result
      (dissoc
       :outer-runtime-ms)

      (update
       :candidates
       (fn [candidates]
         (mapv
          (fn [candidate]
            (-> candidate
                (dissoc
                 :mean-runtime-ms)

                (update
                 :instance-results
                 (fn [instance-results]
                   (mapv
                    #(dissoc %
                             :runtime-ms)
                    instance-results)))))
          candidates)))

      (update
       :best-candidate
       (fn [candidate]
         (-> candidate
             (dissoc
              :mean-runtime-ms)

             (update
              :instance-results
              (fn [instance-results]
                (mapv
                 #(dissoc %
                          :runtime-ms)
                 instance-results))))))))

(deftest initial-population-has-the-requested-shape
  (let [result
        (outer-gp/evaluate-initial-population
         1
         smoke-config)

        candidates
        (:candidates
         result)]

    (testing "The requested number of candidates is generated"
      (is (= 6
             (count candidates))))

    (testing "All programs are unique"
      (is (= 6
             (count
              (distinct
               (map :program
                    candidates))))))

    (testing "All generated individuals remain valid DSL optimizers"
      (is
       (every?
        #(dsl/valid?
          (:program %))
        candidates)))

    (testing "Rank values are one-based and consecutive"
      (is (= [1 2 3 4 5 6]
             (mapv :rank
                   candidates))))))

(deftest outer-fitness-uses-training-instances-only
  (let [result
        (outer-gp/evaluate-initial-population
         2
         smoke-config)]

    (is (= 3
           (:training-instance-count-used
            result)))

    (is (= 0
           (:test-instances-used
            result)))

    (is
     (every?
      #(str/starts-with?
        %
        "train-")
      (:training-instance-ids
       result)))

    (is
     (every?
      (fn [candidate]
        (every?
         #(str/starts-with?
           (:instance-id %)
           "train-")
         (:instance-results
          candidate)))
      (:candidates
       result)))))

(deftest all-candidates-receive-common-inner-seeds
  (let [result
        (outer-gp/evaluate-initial-population
         3
         smoke-config)

        expected-seeds
        (:common-inner-seeds
         result)

        candidate-seed-vectors
        (mapv
         (fn [candidate]
           (mapv :inner-seed
                 (:instance-results
                  candidate)))
         (:candidates
          result))]

    (is
     (every?
      #(= expected-seeds %)
      candidate-seed-vectors))))

(deftest candidates-are-ranked-by-fitness
  (let [result
        (outer-gp/evaluate-initial-population
         4
         smoke-config)

        candidates
        (:candidates
         result)

        fitness-values
        (mapv :fitness
              candidates)]

    (testing "Fitness values are in descending order"
      (is (apply >=
                 fitness-values)))

    (testing "Fitness equals mean score minus parsimony penalty"
      (is
       (every?
        (fn [candidate]
          (<
           (Math/abs
            (- (:fitness candidate)
               (- (:mean-normalized-score
                   candidate)
                  (:parsimony-penalty
                   candidate))))
           1.0e-12))
        candidates)))))

(deftest every-candidate-respects-the-inner-budget
  (let [result
        (outer-gp/evaluate-initial-population
         5
         smoke-config)]

    (is
     (every?
      (fn [candidate]
        (every?
         #(= 40
             (:fitness-evaluations-used
              %))
         (:instance-results
          candidate)))
      (:candidates
       result)))))

(deftest initial-population-evaluation-is-reproducible
  (let [result-a
        (outer-gp/evaluate-initial-population
         42
         smoke-config)

        result-b
        (outer-gp/evaluate-initial-population
         42
         smoke-config)]

    ;; Wall-clock runtime is deliberately excluded.
    (is (=
         (remove-runtime-values
          result-a)

         (remove-runtime-values
          result-b)))))
