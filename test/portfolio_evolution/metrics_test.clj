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

(deftest confidence-interval-with-one-observation-test
  (testing "A confidence interval is not fabricated for n=1"
    (let [result
          (metrics/confidence-interval-95
           [0.95])]

      (is (= 1
             (:n result)))

      (is
       (approximately=
        0.95
        (:mean result)))

      (is (nil?
           (:ci95-lower
            result)))

      (is (nil?
           (:ci95-upper
            result))))))

(deftest confidence-interval-for-constant-values-test
  (testing "A constant sample has zero-width confidence interval"
    (let [result
          (metrics/confidence-interval-95
           [0.9 0.9 0.9 0.9])]

      (is
       (approximately=
        0.9
        (:mean result)))

      (is
       (approximately=
        0.0
        (:sample-standard-deviation
         result)))

      (is
       (approximately=
        0.9
        (:ci95-lower
         result)))

      (is
       (approximately=
        0.9
        (:ci95-upper
         result))))))
