(ns portfolio-evolution.experiment
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [portfolio-evolution.baseline-ga :as baseline-ga]
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.metrics :as metrics]
   [portfolio-evolution.rng :as rng]
   [portfolio-evolution.synthetic-data :as synthetic-data]))

(def default-options
  {:master-seed 1
   :dataset-config {}
   :ga-config {}
   :results-dir "results"
   :write-results? true})

(def result-csv-headers
  [:master-seed
   :instance-seed
   :split
   :instance-index
   :instance-id
   :asset-count
   :budget
   :best-objective
   :optimal-objective
   :normalized-score
   :optimality-gap
   :best-cost
   :selected-asset-count
   :feasible?
   :fitness-evaluations-used
   :best-found-at-evaluation
   :best-found-fraction
   :raw-feasibility-rate
   :feasibility-rate
   :runtime-ms])

(def convergence-csv-headers
  [:master-seed
   :instance-seed
   :instance-index
   :instance-id
   :generation-step
   :complete-generation?
   :evaluations-used
   :best-objective
   :best-found-at-evaluation])

(defn- csv-cell
  [value]
  (let [text
        (cond
          (nil? value)
          ""

          (keyword? value)
          (name value)

          :else
          (str value))]

    (if (re-find #"[\",\n\r]" text)
      (str "\""
           (str/replace text
                        "\""
                        "\"\"")
           "\"")
      text)))

(defn- write-csv!
  [file headers rows]
  (with-open [writer
              (io/writer file)]

    (.write writer
            (str
             (str/join
              ","
              (map csv-cell headers))
             "\n"))

    (doseq [row rows]
      (.write writer
              (str
               (str/join
                ","
                (map
                 (fn [header]
                   (csv-cell
                    (get row header)))
                 headers))
               "\n")))))

(defn- run-test-instance
  [master-seed
   instance-index
   instance
   ga-config]

  (let [instance-seed
        (rng/derive-seed
         master-seed
         instance-index)

        exact-result
        (knapsack/exact-solve
         instance)

        baseline-result
        (baseline-ga/run
         instance
         instance-seed
         ga-config)

        score-result
        (metrics/score
         (:best-objective baseline-result)
         (:optimal-objective exact-result))

        normalized-score
        (:normalized-score score-result)]

    (when (> normalized-score
             (+ 1.0 1.0e-12))
      (throw
       (ex-info
        "Baseline objective exceeded the exact dynamic-programming optimum."
        {:instance-id (:id instance)
         :baseline-objective
         (:best-objective baseline-result)
         :optimal-objective
         (:optimal-objective exact-result)})))

    {:master-seed
     master-seed

     :instance-seed
     instance-seed

     :split
     :test

     :instance-index
     instance-index

     :instance-id
     (:id instance)

     :asset-count
     (count (:assets instance))

     :budget
     (:budget instance)

     :best-objective
     (:best-objective baseline-result)

     :optimal-objective
     (:optimal-objective exact-result)

     :normalized-score
     normalized-score

     :optimality-gap
     (:optimality-gap score-result)

     :best-cost
     (:best-cost baseline-result)

     :selected-asset-count
     (:selected-asset-count baseline-result)

     :feasible?
     (:feasible? baseline-result)

     :fitness-evaluations-used
     (:fitness-evaluations-used
      baseline-result)

     :best-found-at-evaluation
     (:best-found-at-evaluation
      baseline-result)

     :best-found-fraction
     (/ (double
         (:best-found-at-evaluation
          baseline-result))
        (double
         (:fitness-evaluations-used
          baseline-result)))

     :raw-feasibility-rate
     (:raw-feasibility-rate
      baseline-result)

     :feasibility-rate
     (:feasibility-rate
      baseline-result)

     :runtime-ms
     (:runtime-ms baseline-result)

     :best-solution
     (:best-solution baseline-result)

     :resolved-ga-config
     (:config baseline-result)

     :convergence
     (:convergence baseline-result)}))

(defn- convergence-rows
  [instance-results]
  (mapcat
   (fn [{:keys [master-seed
                instance-seed
                instance-index
                instance-id
                convergence]}]

     (map
      (fn [point]
        {:master-seed
         master-seed

         :instance-seed
         instance-seed

         :instance-index
         instance-index

         :instance-id
         instance-id

         :generation-step
         (:generation-step point)

         :complete-generation?
         (:complete-generation? point)

         :evaluations-used
         (:evaluations-used point)

         :best-objective
         (:best-objective point)

         :best-found-at-evaluation
         (:best-found-at-evaluation
          point)})
      convergence))

   instance-results))

(defn- ensure-results-directory!
  [results-dir]
  (let [directory
        (io/file results-dir)]

    (when-not (.exists directory)
      (when-not (.mkdirs directory)
        (throw
         (ex-info "Could not create results directory."
                  {:results-dir results-dir}))))

    (when-not (.isDirectory directory)
      (throw
       (ex-info "Results path is not a directory."
                {:results-dir results-dir})))

    directory))

(defn- write-results!
  [result results-dir]
  (let [directory
        (ensure-results-directory!
         results-dir)

        master-seed
        (:master-seed result)

        edn-file
        (io/file
         directory
         (format "run-seed-%d.edn"
                 master-seed))

        result-csv-file
        (io/file
         directory
         (format "run-seed-%d.csv"
                 master-seed))

        convergence-csv-file
        (io/file
         directory
         (format "convergence-seed-%d.csv"
                 master-seed))]

    (spit
     edn-file
     (with-out-str
       (pprint/pprint result)))

    (write-csv!
     result-csv-file
     result-csv-headers
     (:instances result))

    (write-csv!
     convergence-csv-file
     convergence-csv-headers
     (convergence-rows
      (:instances result)))

    {:edn
     (.getPath edn-file)

     :instance-csv
     (.getPath result-csv-file)

     :convergence-csv
     (.getPath convergence-csv-file)}))

(defn run-baseline-experiment
  "Runs the fixed baseline GA over all held-out test instances.

  Training instances are generated as part of the fixed dataset, but are
  not used by this experiment."
  ([]
   (run-baseline-experiment {}))

  ([options]
   (let [{:keys [master-seed
                 dataset-config
                 ga-config
                 results-dir
                 write-results?]}
         (merge default-options
                options)

         dataset
         (synthetic-data/generate-dataset
          dataset-config)

         test-instances
         (:test dataset)

         instance-results
         (mapv
          (fn [instance-index instance]
            (run-test-instance
             master-seed
             instance-index
             instance
             ga-config))
          (range)
          test-instances)

         result
         {:experiment
          :baseline-ga-held-out-test

          :master-seed
          master-seed

          :data-config
          (:config dataset)

          :requested-ga-config
          ga-config

          :training-instance-count-generated
          (count (:train dataset))

          :training-instances-used
          0

          :test-instance-count
          (count test-instances)

          :instances
          instance-results

          :summary
          (metrics/summarize-instance-results
           instance-results)}]

     (if write-results?
       (assoc result
              :output-files
              (write-results!
               result
               results-dir))
       result))))
