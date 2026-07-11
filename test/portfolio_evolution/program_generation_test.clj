(ns portfolio-evolution.program-generation-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.dsl :as dsl]
   [portfolio-evolution.optimizer-runtime :as runtime]
   [portfolio-evolution.program-generation :as generation]
   [portfolio-evolution.rng :as rng]))

(def test-generator-config
  {:max-depth 8
   :population-size 10
   :greedy-counts [2 4]
   :selection-counts [3 5 8]
   :injection-counts [2 3 5]
   :stagnation-thresholds [2 3 4]
   :diversity-thresholds [0.20 0.40]
   :budget-utilization-thresholds [0.70 0.90]
   :scorer-scalars [0.2 0.5 1.0]})

(def known-instance
  {:id "generated-program-instance"

   :assets
   [{:id "asset-0"
     :cost 4
     :expected-profit 8}

    {:id "asset-1"
     :cost 5
     :expected-profit 11}

    {:id "asset-2"
     :cost 6
     :expected-profit 13}

    {:id "asset-3"
     :cost 7
     :expected-profit 14}

    {:id "asset-4"
     :cost 8
     :expected-profit 15}

    {:id "asset-5"
     :cost 9
     :expected-profit 16}

    {:id "asset-6"
     :cost 10
     :expected-profit 17}

    {:id "asset-7"
     :cost 11
     :expected-profit 18}]

   :budget 30})

(defn- generate-for-seed
  [seed]
  (generation/generate-optimizer
   (rng/create seed)
   test-generator-config))

(deftest generated-optimizers-are-valid-by-construction
  (let [optimizers
        (mapv
         generate-for-seed
         (range 1 101))]

    (testing "Every generated individual has optimizer type"
      (is
       (every?
        #(= :optimizer
            (dsl/infer-type %))
        optimizers)))

    (testing "Every generated individual passes static validation"
      (is
       (every?
        dsl/valid?
        optimizers)))

    (testing "Every optimizer respects the configured maximum depth"
      (is
       (every?
        #(<= (dsl/program-depth %)
             (:max-depth
              test-generator-config))
        optimizers)))

    (testing "Generated optimizers contain structural diversity"
      (is (> (count
              (distinct optimizers))
             20)))))

(deftest generation-is-reproducible
  (testing "The same generator seed produces the same optimizer"
    (is (=
         (generate-for-seed 42)
         (generate-for-seed 42)))))

(deftest different-seeds-can-produce-different-optimizers
  (testing "Different generator seeds explore different program trees"
    (is (not=
         (generate-for-seed 1)
         (generate-for-seed 2)))))

(deftest subtree-access-and-replacement-work
  (let [optimizer
        '(optimizer
           (random-population 10)
           ratio-repair
           (repeat-until-budget
             (inject-random 2)))

        replacement
        '(local-search
           (swap-assets))

        replaced
        (generation/replace-subtree
         optimizer
         [2 0]
         replacement)]

    (is (=
         '(inject-random 2)
         (generation/subtree-at
          optimizer
          [2 0])))

    (is (=
         replacement
         (generation/subtree-at
          replaced
          [2 0])))

    (is (true?
         (dsl/valid?
          replaced)))))

(deftest mutation-preserves-type-and-validity
  (let [random
        (rng/create 700)

        initial
        (generation/generate-optimizer
         random
         test-generator-config)

        mutations
        (loop [current
               initial

               remaining
               25

               accumulated
               []]

          (if (zero? remaining)
            accumulated

            (let [mutated
                  (generation/mutate
                   random
                   current
                   test-generator-config)]

              (recur
               mutated
               (dec remaining)
               (conj accumulated
                     mutated)))))]

    (testing "Every mutation differs from its immediate parent"
      (is
       (every?
        true?
        (map not=
             (cons initial
                   (butlast mutations))
             mutations))))

    (testing "Every mutation remains a valid optimizer"
      (is
       (every?
        dsl/valid?
        mutations)))

    (testing "Mutation never exceeds the configured maximum depth"
      (is
       (every?
        #(<= (dsl/program-depth %)
             (:max-depth
              test-generator-config))
        mutations)))

    (testing "The fixed evaluation-budget wrapper is preserved"
      (is
       (every?
        (fn [optimizer]
          (= 'repeat-until-budget
             (first
              (nth optimizer 3))))
        mutations)))))

(deftest generated-optimizers-are-executable
  (doseq [generator-seed
          (range 1 11)]

    (let [optimizer
          (generate-for-seed
           generator-seed)

          result
          (runtime/run
           known-instance
           optimizer
           (rng/derive-seed
            99
            generator-seed)
           {:evaluation-budget 60})]

      (testing
       (str
        "Generated optimizer "
        generator-seed
        " consumes the complete budget")

        (is (= 60
               (:fitness-evaluations-used
                result))))

      (testing
       (str
        "Generated optimizer "
        generator-seed
        " returns a feasible solution")

        (is (true?
             (:feasible?
              result)))

        (is (= 1.0
               (:feasibility-rate
                result)))))))
