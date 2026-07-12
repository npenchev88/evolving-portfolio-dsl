(ns portfolio-evolution.scale-evaluation-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]
   [portfolio-evolution.scale-evaluation :as evaluation]))

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
  {:asset-counts
   [8 12]

   :test-instance-count
   2

   :evaluation-budget
   20

   :base-test-data-seed
   555

   :random-search-config
   {:selection-probability 0.30}

   :baseline-ga-config
   {:population-size 10
    :tournament-size 2
    :elite-count 1}})

(def runtime-keys
  #{:runtime-ms
    :mean-runtime-ms
    :total-runtime-ms
    :total-size-runtime-ms})

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

(deftest requested-problem-sizes-are-evaluated
  (let [result
        (evaluation/evaluate
         1
         test-optimizer
         smoke-config)]

    (is (= [8 12]
           (mapv :asset-count
                 (:sizes result))))

    (is
     (every?
      #(= 2
          (:instance-count %))
      (:sizes result)))))

(deftest all-methods-use-the-same-evaluation-budget
  (let [result
        (evaluation/evaluate
         2
         test-optimizer
         smoke-config)]

    (is
     (every?
      true?

      (for [size-result
            (:sizes result)

            comparison
            (:instances size-result)

            method-key
            [:random :ga :evolved]]

        (= 20
           (get-in comparison
                   [method-key
                    :fitness-evaluations-used])))))))

(deftest all-methods-remain-bounded-by-the-exact-optimum
  (let [result
        (evaluation/evaluate
         3
         test-optimizer
         smoke-config)]

    (is
     (every?
      true?

      (for [size-result
            (:sizes result)

            comparison
            (:instances size-result)

            method-key
            [:random :ga :evolved]]

        (<=
         (get-in comparison
                 [method-key
                  :best-objective])

         (:optimal-objective
          comparison))))))

(deftest scale-evaluation-is-reproducible
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

    (is
     (=
      (remove-runtimes
       result-a)

      (remove-runtimes
       result-b))))))
