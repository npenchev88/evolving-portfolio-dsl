(ns portfolio-evolution.synthetic-data-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.synthetic-data :as synthetic-data]))

(deftest dataset-is-deterministic-test
  (testing "The same data seeds produce identical datasets"
    (let [dataset-a
          (synthetic-data/generate-dataset)

          dataset-b
          (synthetic-data/generate-dataset)]

      (is (= dataset-a dataset-b)))))

(deftest dataset-shape-test
  (let [{:keys [train test config]}
        (synthetic-data/generate-dataset)]

    (testing "The configured number of instances is generated"
      (is (= 10 (count train)))
      (is (= 10 (count test))))

    (testing "Every instance contains the configured number of assets"
      (is
       (every?
        #(= (:asset-count config)
            (count (:assets %)))
        (concat train test))))

    (testing "Training and test instances are distinct"
      (is (not= train test)))))

(deftest generated-values-respect-config-test
  (let [{:keys [train config]}
        (synthetic-data/generate-dataset)

        assets
        (mapcat :assets train)]

    (testing "Generated costs remain inside the configured range"
      (is
       (every?
        #(<= (:minimum-cost config)
             (:cost %)
             (:maximum-cost config))
        assets)))

    (testing "Generated expected profits remain inside the configured range"
      (is
       (every?
        #(<= (:minimum-expected-profit config)
             (:expected-profit %)
             (:maximum-expected-profit config))
        assets)))

    (testing "Budget is derived from the configured budget ratio"
      (is
       (every?
        (fn [{:keys [budget total-available-cost]}]
          (= budget
             (max 1
                  (long
                   (* (double (:budget-ratio config))
                      (double total-available-cost))))))
        train)))))
