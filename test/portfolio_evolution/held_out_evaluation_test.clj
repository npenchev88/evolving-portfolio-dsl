(ns portfolio-evolution.held-out-evaluation-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]
   [portfolio-evolution.held-out-evaluation :as evaluation]))

(def test-optimizer
  '(optimizer
     (mixed-population
       8
       2
       return-per-cost)

     ratio-repair

     (repeat-until-budget
       (transform
         (drop-add
           expected-profit)))))

(def smoke-config
  {:test-instance-count 3
   :evaluation-budget 40

   :dataset-config
   {:train-instance-count 2
    :test-instance-count 3
    :asset-count 10}

   :baseline-config
   {:population-size 10
    :tournament-size 2
    :elite-count 1}})

(def runtime-keys
  #{:runtime-ms
    :mean-runtime-ms
    :total-runtime-ms})

(defn- remove-runtimes
  [value]
  (walk/postwalk
   (fn [node]
     (if (map? node)
       (apply dissoc
              node
              runtime-keys)
       node))
   value))

(deftest evaluation-uses-only-held-out-test-instances
  (let [result
        (evaluation/evaluate
         1
         test-optimizer
         smoke-config)]

    (is (= 0
           (:training-instances-used
            result)))

    (is (= 3
           (:test-instance-count-used
            result)))

    (is
     (every?
      #(str/starts-with?
        %
        "test-")
      (:test-instance-ids
       result)))))

(deftest both-methods-use-the-same-budget-and-seed
  (let [result
        (evaluation/evaluate
         2
         test-optimizer
         smoke-config)]

    (is
     (every?
      (fn [comparison]
        (and
         (= 40
            (get-in comparison
                    [:baseline
                     :fitness-evaluations-used]))

         (= 40
            (get-in comparison
                    [:evolved
                     :fitness-evaluations-used]))))
      (:instances result)))))

(deftest held-out-evaluation-is-reproducible
  (let [result-a
        (evaluation/evaluate
         42
         test-optimizer
         smoke-config)

        result-b
        (evaluation/evaluate
         42
         test-optimizer
         smoke-config)]

    (is (=
         (remove-runtimes
          result-a)

         (remove-runtimes
          result-b)))))
