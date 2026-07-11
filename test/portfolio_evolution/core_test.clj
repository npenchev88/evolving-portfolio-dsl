(ns portfolio-evolution.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.core :as core]))

(deftest parse-args-test
  (testing "Seed defaults to one"
    (is (= {:seed 1}
           (core/parse-args []))))

  (testing "Explicit seed is parsed"
    (is (= {:seed 42}
           (core/parse-args ["--seed" "42"]))))

  (testing "Invalid seed throws an exception"
    (is (thrown? clojure.lang.ExceptionInfo
                 (core/parse-args ["--seed" "abc"]))))

  (testing "Unknown arguments are rejected"
    (is (thrown? clojure.lang.ExceptionInfo
                 (core/parse-args ["--unknown" "value"])))))
