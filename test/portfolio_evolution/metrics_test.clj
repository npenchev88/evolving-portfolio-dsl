(ns portfolio-evolution.metrics-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.metrics :as metrics]))

(defn approximately=
  [expected actual]
  (< (Math/abs
      (- (double expected)
         (double actual)))
     1.0e-12))

(deftest exact-solution-metrics-test
  (testing "An exact solution receives score 1 and gap 0"
    (is
     (approximately=
      1.0
      (metrics/normalized-score 220 220)))

    (is
     (approximately=
      0.0
      (metrics/optimality-gap 220 220)))))

(deftest partial-solution-metrics-test
  (testing "A suboptimal solution receives the expected relative metrics"
    (is
     (approximately=
      (/ 160.0 220.0)
      (metrics/normalized-score 160 220)))

    (is
     (approximately=
      (/ 60.0 220.0)
      (metrics/optimality-gap 160 220)))))
