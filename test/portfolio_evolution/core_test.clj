(ns portfolio-evolution.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.core :as core]))

(deftest parse-args-test
  (testing "Seed defaults to one and results directory defaults to results"
    (is (= {:seed 1
            :results-dir "results"}
           (core/parse-args []))))

  (testing "Explicit seed is parsed while the default results directory remains"
    (is (= {:seed 42
            :results-dir "results"}
           (core/parse-args
            ["--seed" "42"]))))

  (testing "Explicit results directory is parsed"
    (is (= {:seed 1
            :results-dir "/tmp/portfolio-results"}
           (core/parse-args
            ["--results-dir"
             "/tmp/portfolio-results"]))))

  (testing "Seed and results directory can both be provided"
    (is (= {:seed 42
            :results-dir "/tmp/portfolio-results"}
           (core/parse-args
            ["--seed" "42"
             "--results-dir" "/tmp/portfolio-results"]))))

  (testing "Argument order does not matter"
    (is (= {:seed 42
            :results-dir "/tmp/portfolio-results"}
           (core/parse-args
            ["--results-dir" "/tmp/portfolio-results"
             "--seed" "42"]))))

  (testing "Invalid seed throws an exception"
    (is (thrown?
         clojure.lang.ExceptionInfo
         (core/parse-args
          ["--seed" "abc"]))))

  (testing "Missing seed value throws an exception"
    (is (thrown?
         clojure.lang.ExceptionInfo
         (core/parse-args
          ["--seed"]))))

  (testing "Missing results directory value throws an exception"
    (is (thrown?
         clojure.lang.ExceptionInfo
         (core/parse-args
          ["--results-dir"]))))

  (testing "Unknown arguments are rejected"
    (is (thrown?
         clojure.lang.ExceptionInfo
         (core/parse-args
          ["--unknown" "value"])))))
