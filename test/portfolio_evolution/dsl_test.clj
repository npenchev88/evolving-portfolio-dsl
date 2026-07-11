(ns portfolio-evolution.dsl-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.dsl :as dsl]))

(deftest example-program-is-valid
  (testing "The example optimizer has optimizer type"
    (is (= :optimizer
           (dsl/infer-type
            dsl/example-optimizer)))

    (is (true?
         (dsl/valid?
          dsl/example-optimizer)))

    (is (= dsl/example-optimizer
           (dsl/validate!
            dsl/example-optimizer)))))

(deftest arbitrary-clojure-code-is-rejected
  (testing "Unknown operators cannot enter the language"
    (is (false?
         (dsl/valid?
          '(optimizer
             (random-population 10)
             ratio-repair
             (eval
               (println "unsafe"))))))

    (is (thrown?
         clojure.lang.ExceptionInfo
         (dsl/validate!
          '(optimizer
             (random-population 10)
             ratio-repair
             (eval
               (println "unsafe"))))))))

(deftest type-mismatches-are-rejected
  (testing "An asset scorer cannot be used as a predicate"
    (is (false?
         (dsl/valid?
          '(optimizer
             (random-population 10)
             ratio-repair

             (if return-per-cost
               (no-op)
               (no-op)))))))

  (testing "A predicate cannot be used as a portfolio operation"
    (is (false?
         (dsl/valid?
          '(optimizer
             (random-population 10)
             ratio-repair

             (transform
               (stagnant-for? 4))))))))

(deftest invalid-literals-are-rejected
  (testing "Population size must be positive"
    (is (false?
         (dsl/valid?
          '(optimizer
             (random-population 0)
             ratio-repair
             (no-op))))))

  (testing "Diversity threshold must be between zero and one"
    (is (false?
         (dsl/valid?
          '(optimizer
             (random-population 10)
             ratio-repair

             (if (diversity-below? 2.0)
               (no-op)
               (no-op))))))))

(deftest scoring-expressions-are-typed
  (testing "Scoring expressions can be composed structurally"
    (is (= :asset-scorer
           (dsl/infer-type
            '(-
               expected-profit
               (* 0.2
                  cost)))))))

(deftest structural-metrics-are-available
  (testing "Program size and depth are measurable"
    (is (pos?
         (dsl/program-node-count
          dsl/example-optimizer)))

    (is (> (dsl/program-depth
            dsl/example-optimizer)
           1))))
