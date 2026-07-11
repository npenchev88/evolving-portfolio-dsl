(ns portfolio-evolution.outer-evolution-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]
   [portfolio-evolution.dsl :as dsl]
   [portfolio-evolution.outer-gp :as outer-gp]))

(def smoke-config
  {:outer-population-size 6
   :outer-generations 3
   :outer-tournament-size 2
   :elite-count 1
   :max-offspring-attempts 2000

   :training-instance-count 2
   :inner-evaluation-budget 30
   :parsimony-penalty-coefficient 0.001

   :dataset-config
   {:train-instance-count 3
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

(def runtime-keys
  #{:runtime-ms
    :mean-runtime-ms
    :generation-runtime-ms
    :outer-runtime-ms})

(defn- remove-runtime-values
  [value]
  (walk/postwalk
   (fn [node]
     (if (map? node)
       (apply dissoc
              node
              runtime-keys)
       node))
   value))

(deftest evolution-produces-all-requested-generations
  (let [result
        (outer-gp/evolve
         1
         smoke-config)]

    (testing "Generation zero plus three evolved generations are recorded"
      (is (= 4
             (count
              (:generations result))))

      (is (= [0 1 2 3]
             (mapv :generation
                   (:generations result)))))

    (testing "Every generation has the configured population size"
      (is
       (every?
        #(= 6
            (count
             (:candidates %)))
        (:generations result))))))

(deftest all-evolved-programs-remain-valid-and-unique
  (let [result
        (outer-gp/evolve
         2
         smoke-config)]

    (doseq [generation
            (:generations result)]

      (let [programs
            (mapv :program
                  (:candidates generation))]

        (is
         (every?
          dsl/valid?
          programs))

        (is (= (count programs)
               (count
                (distinct programs))))))))

(deftest elitism-makes-best-fitness-monotonic
  (let [result
        (outer-gp/evolve
         3
         smoke-config)

        best-fitness-values
        (mapv :best-fitness
              (:history result))]

    (is
     (apply <=
            best-fitness-values))))

(deftest previous-best-program-is-preserved-as-an-elite
  (let [result
        (outer-gp/evolve
         4
         smoke-config)

        generations
        (:generations result)]

    (doseq [[previous-generation
             next-generation]
            (map vector
                 generations
                 (rest generations))]

      (let [previous-best-program
            (get-in previous-generation
                    [:candidates
                     0
                     :program])

            elite-programs
            (->> (:candidates
                  next-generation)

                 (filter
                  #(= :elite
                      (:origin %)))

                 (map :program)
                 set)]

        (is
         (contains?
          elite-programs
          previous-best-program))))))

(deftest every-candidate-respects-the-inner-evaluation-budget
  (let [result
        (outer-gp/evolve
         5
         smoke-config)]

    (is
     (every?
      true?

      (for [generation
            (:generations result)

            candidate
            (:candidates generation)

            instance-result
            (:instance-results candidate)]

        (= 30
           (:fitness-evaluations-used
            instance-result)))))))

(deftest test-instances-are-never-used
  (let [result
        (outer-gp/evolve
         6
         smoke-config)]

    (is (= 0
           (:test-instances-used
            result)))

    (is
     (every?
      #(str/starts-with?
        %
        "train-")
      (:training-instance-ids
       result)))))

(deftest complete-evolution-is-reproducible
  (let [result-a
        (outer-gp/evolve
         42
         smoke-config)

        result-b
        (outer-gp/evolve
         42
         smoke-config)]

    (is
     (=
      (remove-runtime-values
       result-a)

      (remove-runtime-values
       result-b)))))
