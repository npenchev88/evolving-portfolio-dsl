(ns portfolio-evolution.final-experiment
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [portfolio-evolution.metrics :as metrics]
   [portfolio-evolution.outer-gp :as outer-gp]
   [portfolio-evolution.scale-evaluation :as scale-evaluation]))

(def default-config
  {:seed-start 1
   :seed-count 30

   :results-dir
   "results/final-experiment"

   :resume?
   true

   :outer-config
   {:outer-population-size 20
    :outer-generations 10
    :outer-tournament-size 3
    :elite-count 1
    :max-offspring-attempts 20000

    :training-instance-count 10
    :inner-evaluation-budget 150
    :parsimony-penalty-coefficient 0.0005

    :dataset-config
    {:train-instance-count 10
     :test-instance-count 10
     :asset-count 50}

    :generator-config
    {:max-depth 8
     :population-size 50
     :greedy-counts [5 10 15 20]
     :selection-counts [10 20 25 30]
     :injection-counts [5 10 15 20]
     :stagnation-thresholds [2 4 6 8]
     :diversity-thresholds [0.10 0.20 0.30 0.50]
     :budget-utilization-thresholds [0.70 0.80 0.90]
     :scorer-scalars [0.1 0.2 0.5 1.0 2.0]}}

   :scale-config
   {:asset-counts [50 100 250 500]
    :test-instance-count 10
    :evaluation-budget 150
    :base-test-data-seed 9898

    :random-search-config
    {:selection-probability 0.30}

    :baseline-ga-config
    {:population-size 50
     :tournament-size 2
     :crossover-probability 0.7
     :elite-count 1}}})

(def outer-csv-headers
  [:seed
   :initial-fitness
   :final-fitness
   :fitness-improvement
   :initial-training-score
   :final-training-score
   :training-score-improvement
   :initial-nodes
   :final-nodes
   :node-reduction
   :initial-depth
   :final-depth
   :depth-reduction
   :outer-runtime-ms])

(def size-csv-headers
  [:seed
   :asset-count
   :random-mean-score
   :ga-mean-score
   :initial-dsl-mean-score
   :final-dsl-mean-score
   :final-minus-random
   :final-minus-ga
   :final-minus-initial
   :random-mean-gap
   :ga-mean-gap
   :initial-dsl-mean-gap
   :final-dsl-mean-gap
   :random-mean-runtime-ms
   :ga-mean-runtime-ms
   :initial-dsl-mean-runtime-ms
   :final-dsl-mean-runtime-ms])

(def aggregate-size-csv-headers
  [:asset-count

   :random-score-mean
   :random-score-ci95-lower
   :random-score-ci95-upper

   :ga-score-mean
   :ga-score-ci95-lower
   :ga-score-ci95-upper

   :final-dsl-score-mean
   :final-dsl-score-ci95-lower
   :final-dsl-score-ci95-upper

   :final-minus-random-mean
   :final-minus-random-ci95-lower
   :final-minus-random-ci95-upper

   :final-minus-ga-mean
   :final-minus-ga-ci95-lower
   :final-minus-ga-ci95-upper

   :final-minus-initial-mean
   :final-minus-initial-ci95-lower
   :final-minus-initial-ci95-upper])

(def aggregate-outer-csv-headers
  [:seed-count

   :fitness-improvement-mean
   :fitness-improvement-ci95-lower
   :fitness-improvement-ci95-upper

   :training-score-improvement-mean
   :training-score-improvement-ci95-lower
   :training-score-improvement-ci95-upper

   :node-reduction-mean
   :node-reduction-ci95-lower
   :node-reduction-ci95-upper

   :depth-reduction-mean
   :depth-reduction-ci95-lower
   :depth-reduction-ci95-upper])

(defn- validate-config!
  [{:keys [seed-start
           seed-count
           results-dir]
    :as config}]

  (when-not (integer?
             seed-start)
    (throw
     (ex-info
      "Seed start must be an integer."
      {:seed-start seed-start})))

  (when-not (and (integer? seed-count)
                 (pos? seed-count))
    (throw
     (ex-info
      "Seed count must be a positive integer."
      {:seed-count seed-count})))

  (when-not (and (string? results-dir)
                 (not
                  (str/blank?
                   results-dir)))
    (throw
     (ex-info
      "Results directory must be a non-empty string."
      {:results-dir results-dir})))

  config)

(defn- candidate-summary
  [candidate]
  {:fitness
   (:fitness candidate)

   :mean-training-score
   (:mean-normalized-score
    candidate)

   :minimum-training-score
   (:minimum-normalized-score
    candidate)

   :mean-training-gap
   (:mean-optimality-gap
    candidate)

   :node-count
   (:program-node-count
    candidate)

   :depth
   (:program-depth
    candidate)

   :program
   (:program candidate)})

(defn- size-result-map
  [scale-result]
  (into
   {}
   (map
    (juxt :asset-count
          identity)
    (:sizes scale-result))))

(defn- size-summary
  [seed
   final-size-result
   initial-size-result]

  (let [asset-count
        (:asset-count
         final-size-result)

        random-score
        (get-in final-size-result
                [:random-summary
                 :mean-normalized-score])

        ga-score
        (get-in final-size-result
                [:ga-summary
                 :mean-normalized-score])

        initial-dsl-score
        (get-in initial-size-result
                [:evolved-summary
                 :mean-normalized-score])

        final-dsl-score
        (get-in final-size-result
                [:evolved-summary
                 :mean-normalized-score])]

    {:seed
     seed

     :asset-count
     asset-count

     :random-mean-score
     random-score

     :ga-mean-score
     ga-score

     :initial-dsl-mean-score
     initial-dsl-score

     :final-dsl-mean-score
     final-dsl-score

     :final-minus-random
     (- final-dsl-score
        random-score)

     :final-minus-ga
     (- final-dsl-score
        ga-score)

     :final-minus-initial
     (- final-dsl-score
        initial-dsl-score)

     :random-mean-gap
     (get-in final-size-result
             [:random-summary
              :mean-optimality-gap])

     :ga-mean-gap
     (get-in final-size-result
             [:ga-summary
              :mean-optimality-gap])

     :initial-dsl-mean-gap
     (get-in initial-size-result
             [:evolved-summary
              :mean-optimality-gap])

     :final-dsl-mean-gap
     (get-in final-size-result
             [:evolved-summary
              :mean-optimality-gap])

     :random-mean-runtime-ms
     (get-in final-size-result
             [:random-summary
              :mean-runtime-ms])

     :ga-mean-runtime-ms
     (get-in final-size-result
             [:ga-summary
              :mean-runtime-ms])

     :initial-dsl-mean-runtime-ms
     (get-in initial-size-result
             [:evolved-summary
              :mean-runtime-ms])

     :final-dsl-mean-runtime-ms
     (get-in final-size-result
             [:evolved-summary
              :mean-runtime-ms])}))

(defn run-one-seed
  "Runs the complete experiment for one master seed."
  [seed config]
  (let [outer-config
        (:outer-config config)

        scale-config
        (:scale-config config)

        evolution
        (outer-gp/evolve
         seed
         outer-config)

        initial-candidate
        (get-in evolution
                [:generations
                 0
                 :candidates
                 0])

        final-candidate
        (:best-candidate
         evolution)

        initial-program
        (:program
         initial-candidate)

        final-program
        (:program
         final-candidate)

        ;; Both programs are evaluated on exactly the same scale suites.
        initial-scale-result
        (scale-evaluation/evaluate
         seed
         initial-program
         scale-config)

        final-scale-result
        (scale-evaluation/evaluate
         seed
         final-program
         scale-config)

        initial-sizes
        (size-result-map
         initial-scale-result)

        final-sizes
        (:sizes
         final-scale-result)

        size-summaries
        (mapv
         (fn [final-size-result]
           (let [asset-count
                 (:asset-count
                  final-size-result)

                 initial-size-result
                 (get initial-sizes
                      asset-count)]

             (when-not initial-size-result
               (throw
                (ex-info
                 "Initial optimizer is missing a scale result."
                 {:seed seed
                  :asset-count
                  asset-count})))

             (size-summary
              seed
              final-size-result
              initial-size-result)))
         final-sizes)

        initial-summary
        (candidate-summary
         initial-candidate)

        final-summary
        (candidate-summary
         final-candidate)]

    {:seed
     seed

     :protocol
     {:outer-config
      outer-config

      :scale-config
      scale-config}

     :outer
     {:initial
      initial-summary

      :final
      final-summary

      :fitness-improvement
      (- (:fitness final-summary)
         (:fitness initial-summary))

      :training-score-improvement
      (- (:mean-training-score
          final-summary)
         (:mean-training-score
          initial-summary))

      ;; Positive values indicate simplification.
      :node-reduction
      (- (:node-count initial-summary)
         (:node-count final-summary))

      :depth-reduction
      (- (:depth initial-summary)
         (:depth final-summary))

      :history
      (:history evolution)

      :outer-runtime-ms
      (:outer-runtime-ms
       evolution)}

     :sizes
     size-summaries}))

(defn- statistic
  [rows field]
  (metrics/confidence-interval-95
   (mapv field
         rows)))

(defn- statistic-fields
  [statistics prefix]
  {(keyword
    (str prefix "-mean"))
   (:mean statistics)

   (keyword
    (str prefix "-ci95-lower"))
   (:ci95-lower
    statistics)

   (keyword
    (str prefix "-ci95-upper"))
   (:ci95-upper
    statistics)})

(defn aggregate-seed-summaries
  "Aggregates independent seed-level results.

  Confidence intervals are calculated across master seeds, not across
  individual test instances."
  [seed-summaries]
  (when (empty?
         seed-summaries)
    (throw
     (ex-info
      "Cannot aggregate an empty seed collection."
      {})))

  (let [outer-rows
        (mapv
         (fn [seed-summary]
           (let [outer
                 (:outer
                  seed-summary)]

             {:fitness-improvement
              (:fitness-improvement
               outer)

              :training-score-improvement
              (:training-score-improvement
               outer)

              :node-reduction
              (:node-reduction
               outer)

              :depth-reduction
              (:depth-reduction
               outer)}))
         seed-summaries)

        asset-counts
        (->> seed-summaries
             (mapcat :sizes)
             (map :asset-count)
             distinct
             sort
             vec)

        by-size
        (mapv
         (fn [asset-count]
           (let [rows
                 (->> seed-summaries
                      (mapcat :sizes)
                      (filter
                       #(= asset-count
                           (:asset-count %)))
                      vec)]

             (merge
              {:asset-count
               asset-count

               :seed-count
               (count rows)}

              (statistic-fields
               (statistic
                rows
                :random-mean-score)
               "random-score")

              (statistic-fields
               (statistic
                rows
                :ga-mean-score)
               "ga-score")

              (statistic-fields
               (statistic
                rows
                :final-dsl-mean-score)
               "final-dsl-score")

              (statistic-fields
               (statistic
                rows
                :final-minus-random)
               "final-minus-random")

              (statistic-fields
               (statistic
                rows
                :final-minus-ga)
               "final-minus-ga")

              (statistic-fields
               (statistic
                rows
                :final-minus-initial)
               "final-minus-initial"))))
         asset-counts)]

    {:seed-count
     (count seed-summaries)

     :outer
     {:fitness-improvement
      (statistic
       outer-rows
       :fitness-improvement)

      :training-score-improvement
      (statistic
       outer-rows
       :training-score-improvement)

      :node-reduction
      (statistic
       outer-rows
       :node-reduction)

      :depth-reduction
      (statistic
       outer-rows
       :depth-reduction)}

     :by-size
     by-size}))

(defn- ensure-directory!
  [path]
  (let [directory
        (io/file path)]

    (when-not (.exists directory)
      (when-not (.mkdirs directory)
        (throw
         (ex-info
          "Could not create results directory."
          {:path path}))))

    (when-not (.isDirectory directory)
      (throw
       (ex-info
        "Results path is not a directory."
        {:path path})))

    directory))

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

    (if (re-find #"[\",\n\r]"
                 text)
      (str
       "\""
       (str/replace
        text
        "\""
        "\"\"")
       "\"")
      text)))

(defn- write-csv!
  [file headers rows]
  (with-open [writer
              (io/writer file)]

    (.write
     writer
     (str
      (str/join
       ","
       (map csv-cell
            headers))
      "\n"))

    (doseq [row rows]
      (.write
       writer
       (str
        (str/join
         ","
         (map
          (fn [header]
            (csv-cell
             (get row header)))
          headers))
        "\n")))))

(defn- outer-row
  [seed-summary]
  (let [seed
        (:seed seed-summary)

        outer
        (:outer seed-summary)

        initial
        (:initial outer)

        final
        (:final outer)]

    {:seed
     seed

     :initial-fitness
     (:fitness initial)

     :final-fitness
     (:fitness final)

     :fitness-improvement
     (:fitness-improvement
      outer)

     :initial-training-score
     (:mean-training-score
      initial)

     :final-training-score
     (:mean-training-score
      final)

     :training-score-improvement
     (:training-score-improvement
      outer)

     :initial-nodes
     (:node-count initial)

     :final-nodes
     (:node-count final)

     :node-reduction
     (:node-reduction
      outer)

     :initial-depth
     (:depth initial)

     :final-depth
     (:depth final)

     :depth-reduction
     (:depth-reduction
      outer)

     :outer-runtime-ms
     (:outer-runtime-ms
      outer)}))

(defn- aggregate-size-row
  [row]
  (select-keys
   row
   aggregate-size-csv-headers))

(defn- aggregate-outer-row
  [aggregate]
  (let [outer
        (:outer aggregate)]

    {:seed-count
     (:seed-count aggregate)

     :fitness-improvement-mean
     (get-in outer
             [:fitness-improvement
              :mean])

     :fitness-improvement-ci95-lower
     (get-in outer
             [:fitness-improvement
              :ci95-lower])

     :fitness-improvement-ci95-upper
     (get-in outer
             [:fitness-improvement
              :ci95-upper])

     :training-score-improvement-mean
     (get-in outer
             [:training-score-improvement
              :mean])

     :training-score-improvement-ci95-lower
     (get-in outer
             [:training-score-improvement
              :ci95-lower])

     :training-score-improvement-ci95-upper
     (get-in outer
             [:training-score-improvement
              :ci95-upper])

     :node-reduction-mean
     (get-in outer
             [:node-reduction
              :mean])

     :node-reduction-ci95-lower
     (get-in outer
             [:node-reduction
              :ci95-lower])

     :node-reduction-ci95-upper
     (get-in outer
             [:node-reduction
              :ci95-upper])

     :depth-reduction-mean
     (get-in outer
             [:depth-reduction
              :mean])

     :depth-reduction-ci95-lower
     (get-in outer
             [:depth-reduction
              :ci95-lower])

     :depth-reduction-ci95-upper
     (get-in outer
             [:depth-reduction
              :ci95-upper])}))

(defn- seed-result-file
  [results-dir seed]
  (io/file
   results-dir
   (format
    "seed-%02d.edn"
    seed)))

(defn- write-seed-result!
  [results-dir seed-summary]
  (let [file
        (seed-result-file
         results-dir
         (:seed seed-summary))]

    (spit
     file
     (with-out-str
       (pprint/pprint
        seed-summary)))

    (.getPath file)))

(defn- read-seed-result!
  [file]
  (edn/read-string
   (slurp file)))

(defn- write-aggregate-results!
  [results-dir
   seed-summaries
   aggregate]

  (let [aggregate-edn
        (io/file
         results-dir
         "aggregate.edn")

        outer-seeds-csv
        (io/file
         results-dir
         "outer-seed-results.csv")

        size-seeds-csv
        (io/file
         results-dir
         "size-seed-results.csv")

        aggregate-size-csv
        (io/file
         results-dir
         "aggregate-by-size.csv")

        aggregate-outer-csv
        (io/file
         results-dir
         "aggregate-outer.csv")]

    (spit
     aggregate-edn
     (with-out-str
       (pprint/pprint
        aggregate)))

    (write-csv!
     outer-seeds-csv
     outer-csv-headers
     (mapv outer-row
           seed-summaries))

    (write-csv!
     size-seeds-csv
     size-csv-headers
     (mapv identity
           (mapcat :sizes
                   seed-summaries)))

    (write-csv!
     aggregate-size-csv
     aggregate-size-csv-headers
     (mapv aggregate-size-row
           (:by-size
            aggregate)))

    (write-csv!
     aggregate-outer-csv
     aggregate-outer-csv-headers
     [(aggregate-outer-row
       aggregate)])

    {:aggregate-edn
     (.getPath aggregate-edn)

     :outer-seed-results-csv
     (.getPath outer-seeds-csv)

     :size-seed-results-csv
     (.getPath size-seeds-csv)

     :aggregate-by-size-csv
     (.getPath aggregate-size-csv)

     :aggregate-outer-csv
     (.getPath aggregate-outer-csv)}))

(defn run-batch
  "Runs or resumes the complete multi-seed experiment."
  ([]
   (run-batch
    {}))

  ([config-overrides]
   (let [config
         (validate-config!
          (merge
           default-config
           config-overrides))

         {:keys [seed-start
                 seed-count
                 results-dir
                 resume?]}
         config

         directory
         (ensure-directory!
          results-dir)

         resolved-results-dir
         (.getPath directory)

         seeds
         (range seed-start
                (+ seed-start
                   seed-count))

         seed-summaries
         (mapv
          (fn [seed]
            (let [file
                  (seed-result-file
                   resolved-results-dir
                   seed)]

              (if (and resume?
                       (.isFile file))

                (do
                  (println
                   (format
                    "[seed %d] loading existing result"
                    seed))
                  (flush)
                  (read-seed-result!
                   file))

                (do
                  (println
                   (format
                    "[seed %d] running experiment"
                    seed))
                  (flush)

                  (let [started-at
                        (System/nanoTime)

                        result
                        (run-one-seed
                         seed
                         config)

                        elapsed-ms
                        (/ (double
                            (- (System/nanoTime)
                               started-at))
                           1000000.0)]

                    (write-seed-result!
                     resolved-results-dir
                     result)

                    (println
                     (format
                      "[seed %d] complete in %.2f seconds"
                      seed
                      (/ elapsed-ms
                         1000.0)))
                    (flush)

                    result)))))
          seeds)

         aggregate
         (aggregate-seed-summaries
          seed-summaries)

         output-files
         (write-aggregate-results!
          resolved-results-dir
          seed-summaries
          aggregate)]

     {:config
      config

      :seed-summaries
      seed-summaries

      :aggregate
      aggregate

      :output-files
      output-files})))
