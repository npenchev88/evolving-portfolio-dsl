(ns portfolio-evolution.experiment-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [portfolio-evolution.experiment :as experiment]))

(def smoke-options
  {:master-seed 7

   :dataset-config
   {:train-instance-count 2
    :test-instance-count 3
    :asset-count 10}

   :ga-config
   {:population-size 10
    :tournament-size 2
    :elite-count 1
    :evaluation-budget 50}

   :write-results? false})

(defn- deterministic-view
  [result]
  (-> result
      (dissoc :output-files)

      (update
       :instances
       (fn [instance-results]
         (mapv
          #(dissoc % :runtime-ms)
          instance-results)))

      (update
       :summary
       dissoc
       :mean-runtime-ms
       :total-runtime-ms)))

(deftest experiment-uses-only-held-out-test-instances
  (let [result
        (experiment/run-baseline-experiment
         smoke-options)]

    (testing "Training instances are generated but not evaluated"
      (is (= 2
             (:training-instance-count-generated
              result)))
      (is (= 0
             (:training-instances-used
              result))))

    (testing "Every configured test instance is evaluated"
      (is (= 3
             (:test-instance-count
              result)))
      (is (= 3
             (count (:instances result)))))

    (testing "All evaluated rows belong to the test split"
      (is
       (every?
        #(= :test (:split %))
        (:instances result)))

      (is
       (every?
        #(str/starts-with?
          (:instance-id %)
          "test-")
        (:instances result))))))

(deftest instance-seeds-are-distinct
  (let [result
        (experiment/run-baseline-experiment
         smoke-options)

        instance-seeds
        (mapv :instance-seed
              (:instances result))]

    (is (= (count instance-seeds)
           (count (distinct instance-seeds))))))

(deftest same-master-seed-is-reproducible
  (let [result-a
        (experiment/run-baseline-experiment
         smoke-options)

        result-b
        (experiment/run-baseline-experiment
         smoke-options)]

    ;; Wall-clock runtime is intentionally excluded.
    (is (= (deterministic-view result-a)
           (deterministic-view result-b)))))

(deftest expected-output-files-are-written
  (let [temporary-directory
        (io/file
         (System/getProperty "java.io.tmpdir")
         (str
          "portfolio-evolution-"
          (System/nanoTime)))]

    (.mkdirs temporary-directory)

    (try
      (let [result
            (experiment/run-baseline-experiment
             (assoc smoke-options
                    :write-results? true
                    :results-dir
                    (.getPath temporary-directory)))

            output-files
            (vals (:output-files result))]

        (is (= 3
               (count output-files)))

        (is
         (every?
          #(.isFile (io/file %))
          output-files)))

      (finally
        (doseq [file
                (reverse
                 (file-seq temporary-directory))]
          (io/delete-file file true))))))
