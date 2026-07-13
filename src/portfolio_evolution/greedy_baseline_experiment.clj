(ns portfolio-evolution.greedy-baseline-experiment
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [portfolio-evolution.greedy-baseline :as greedy]
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.metrics :as metrics]
   [portfolio-evolution.rng :as rng]
   [portfolio-evolution.scale-evaluation :as scale-evaluation]
   [portfolio-evolution.synthetic-data :as synthetic-data])
  (:gen-class))

(def default-config
  {:seed-start 1
   :seed-count 30
   :input-dir "results/final-30-seeds"
   :output-dir "results/greedy-baseline-30-seeds"})

(def seed-csv-headers
  [:seed
   :asset-count
   :random-score
   :ga-score
   :greedy-score
   :evolved-score
   :greedy-minus-random
   :greedy-minus-ga
   :evolved-minus-greedy])

(def aggregate-csv-headers
  [:asset-count
   :seed-count

   :random-score-mean
   :random-score-ci95-lower
   :random-score-ci95-upper

   :ga-score-mean
   :ga-score-ci95-lower
   :ga-score-ci95-upper

   :greedy-score
   :greedy-instance-score-standard-deviation
   :greedy-mean-optimality-gap
   :greedy-mean-runtime-ms
   :greedy-fitness-evaluations-used

   :evolved-score-mean
   :evolved-score-ci95-lower
   :evolved-score-ci95-upper

   :greedy-minus-random-mean
   :greedy-minus-random-ci95-lower
   :greedy-minus-random-ci95-upper

   :greedy-minus-ga-mean
   :greedy-minus-ga-ci95-lower
   :greedy-minus-ga-ci95-upper

   :evolved-minus-greedy-mean
   :evolved-minus-greedy-ci95-lower
   :evolved-minus-greedy-ci95-upper])

(defn- parse-long!
  [value flag]
  (try
    (Long/parseLong value)
    (catch NumberFormatException _
      (throw
       (ex-info
        "Expected an integer command-line value."
        {:flag flag
         :value value})))))

(defn parse-args
  [args]
  (loop [remaining (seq args)
         config default-config]

    (if (nil? remaining)
      config

      (let [flag
            (first remaining)]

        (case flag
          "--seed-start"
          (let [value (second remaining)]
            (when-not value
              (throw
               (ex-info
                "--seed-start requires a value."
                {})))
            (recur
             (nnext remaining)
             (assoc config
                    :seed-start
                    (parse-long! value flag))))

          "--seed-count"
          (let [value (second remaining)]
            (when-not value
              (throw
               (ex-info
                "--seed-count requires a value."
                {})))
            (recur
             (nnext remaining)
             (assoc config
                    :seed-count
                    (parse-long! value flag))))

          "--input-dir"
          (let [value (second remaining)]
            (when-not value
              (throw
               (ex-info
                "--input-dir requires a value."
                {})))
            (recur
             (nnext remaining)
             (assoc config
                    :input-dir value)))

          "--output-dir"
          (let [value (second remaining)]
            (when-not value
              (throw
               (ex-info
                "--output-dir requires a value."
                {})))
            (recur
             (nnext remaining)
             (assoc config
                    :output-dir value)))

          (throw
           (ex-info
            "Unknown command-line argument."
            {:argument flag})))))))

(defn- validate-config!
  [{:keys [seed-start
           seed-count
           input-dir
           output-dir]
    :as config}]

  (when-not (integer? seed-start)
    (throw
     (ex-info
      "Seed start must be an integer."
      {:seed-start seed-start})))

  (when-not (and (integer? seed-count)
                 (pos? seed-count))
    (throw
     (ex-info
      "Seed count must be positive."
      {:seed-count seed-count})))

  (doseq [[field value]
          [[:input-dir input-dir]
           [:output-dir output-dir]]]
    (when-not (and (string? value)
                   (not (str/blank? value)))
      (throw
       (ex-info
        "Directory paths must be non-empty strings."
        {:field field
         :value value}))))

  config)

(defn- ensure-directory!
  [path]
  (let [directory
        (io/file path)]

    (when-not (.exists directory)
      (when-not (.mkdirs directory)
        (throw
         (ex-info
          "Could not create output directory."
          {:path path}))))

    (when-not (.isDirectory directory)
      (throw
       (ex-info
        "Output path is not a directory."
        {:path path})))

    directory))

(defn- seed-file
  [input-dir seed]
  (io/file
   input-dir
   (format "seed-%02d.edn"
           seed)))

(defn- read-edn!
  [file]
  (when-not (.isFile file)
    (throw
     (ex-info
      "Required EDN file was not found."
      {:file (.getPath file)})))

  (edn/read-string
   (slurp file)))

(defn- read-seed-result!
  [input-dir seed]
  (let [result
        (read-edn!
         (seed-file input-dir seed))]

    (when-not (= seed
                 (:seed result))
      (throw
       (ex-info
        "Seed file contains an unexpected seed."
        {:expected-seed seed
         :actual-seed (:seed result)})))

    result))

(defn- normalized-scale-config
  [seed-result]
  (merge
   scale-evaluation/default-config
   (get-in seed-result
           [:protocol
            :scale-config])))

(defn- validate-common-scale-config!
  [seed-results]
  (let [first-config
        (normalized-scale-config
         (first seed-results))]

    (doseq [seed-result
            (rest seed-results)]
      (let [current-config
            (normalized-scale-config
             seed-result)]

        (when-not (= first-config
                     current-config)
          (throw
           (ex-info
            "Seed files were produced with different scale configurations."
            {:first-config first-config
             :seed (:seed seed-result)
             :current-config current-config})))))

    first-config))

(defn- generate-size-suite
  [asset-count
   {:keys [test-instance-count
           base-test-data-seed
           dataset-config]}]

  (let [data-seed
        (rng/derive-seed
         base-test-data-seed
         asset-count)

        instance-config
        (-> synthetic-data/default-config
            (merge dataset-config)
            (assoc
             :asset-count asset-count
             :test-instance-count test-instance-count))

        family
        (keyword
         (str "test-"
              asset-count
              "-assets"))]

    {:asset-count asset-count
     :data-seed data-seed
     :instances
     (synthetic-data/generate-instances
      data-seed
      family
      test-instance-count
      instance-config)}))

(defn- evaluate-greedy-instance
  [instance-index instance]
  (let [exact-result
        (knapsack/exact-solve instance)

        optimal-objective
        (:optimal-objective exact-result)

        greedy-result
        (greedy/run instance)

        score-result
        (metrics/score
         (:best-objective greedy-result)
         optimal-objective)]

    (when (> (:best-objective greedy-result)
             optimal-objective)
      (throw
       (ex-info
        "Greedy baseline exceeded the exact optimum."
        {:instance-id (:id instance)
         :greedy-objective (:best-objective greedy-result)
         :optimal-objective optimal-objective})))

    {:instance-index instance-index
     :instance-id (:id instance)
     :asset-count (count (:assets instance))
     :budget (:budget instance)
     :best-objective (:best-objective greedy-result)
     :optimal-objective optimal-objective
     :normalized-score (:normalized-score score-result)
     :optimality-gap (:optimality-gap score-result)
     :best-cost (:best-cost greedy-result)
     :selected-asset-count (:selected-asset-count greedy-result)
     :feasible? (:feasible? greedy-result)
     :fitness-evaluations-used
     (:fitness-evaluations-used greedy-result)
     :best-found-at-evaluation 1
     :best-found-fraction 1.0
     :raw-feasibility-rate 1.0
     :feasibility-rate 1.0
     :runtime-ms (:runtime-ms greedy-result)}))

(defn- evaluate-greedy-size
  [asset-count scale-config]
  (let [suite
        (generate-size-suite
         asset-count
         scale-config)

        instance-results
        (mapv evaluate-greedy-instance
              (range)
              (:instances suite))]

    {:asset-count asset-count
     :data-seed (:data-seed suite)
     :instance-count (count instance-results)
     :instances instance-results
     :summary
     (assoc
      (metrics/summarize-instance-results
       instance-results)
      :fitness-evaluations-used 1)}))

(defn- seed-size-map
  [seed-result]
  (into
   {}
   (map
    (juxt :asset-count identity)
    (:sizes seed-result))))

(defn- build-seed-row
  [seed size-row greedy-size-result]
  (let [random-score
        (:random-mean-score size-row)

        ga-score
        (:ga-mean-score size-row)

        evolved-score
        (:final-dsl-mean-score size-row)

        greedy-score
        (get-in greedy-size-result
                [:summary
                 :mean-normalized-score])]

    {:seed seed
     :asset-count (:asset-count size-row)
     :random-score random-score
     :ga-score ga-score
     :greedy-score greedy-score
     :evolved-score evolved-score
     :greedy-minus-random
     (- greedy-score random-score)
     :greedy-minus-ga
     (- greedy-score ga-score)
     :evolved-minus-greedy
     (- evolved-score greedy-score)}))

(defn- statistic
  [rows field]
  (metrics/confidence-interval-95
   (mapv field rows)))

(defn- statistic-fields
  [prefix statistics]
  {(keyword (str prefix "-mean"))
   (:mean statistics)

   (keyword (str prefix "-ci95-lower"))
   (:ci95-lower statistics)

   (keyword (str prefix "-ci95-upper"))
   (:ci95-upper statistics)})

(defn aggregate-results
  [seed-rows greedy-results-by-size]
  (let [asset-counts
        (->> seed-rows
             (map :asset-count)
             distinct
             sort
             vec)]

    {:seed-count
     (count
      (distinct
       (map :seed seed-rows)))

     :by-size
     (mapv
      (fn [asset-count]
        (let [rows
              (filterv
               #(= asset-count
                   (:asset-count %))
               seed-rows)

              greedy-summary
              (get-in greedy-results-by-size
                      [asset-count
                       :summary])]

          (merge
           {:asset-count asset-count
            :seed-count (count rows)
            :greedy-score
            (:mean-normalized-score greedy-summary)
            :greedy-instance-score-standard-deviation
            (:instance-score-standard-deviation greedy-summary)
            :greedy-mean-optimality-gap
            (:mean-optimality-gap greedy-summary)
            :greedy-mean-runtime-ms
            (:mean-runtime-ms greedy-summary)
            :greedy-fitness-evaluations-used
            (:fitness-evaluations-used greedy-summary)}

           (statistic-fields
            "random-score"
            (statistic rows :random-score))

           (statistic-fields
            "ga-score"
            (statistic rows :ga-score))

           (statistic-fields
            "evolved-score"
            (statistic rows :evolved-score))

           (statistic-fields
            "greedy-minus-random"
            (statistic rows :greedy-minus-random))

           (statistic-fields
            "greedy-minus-ga"
            (statistic rows :greedy-minus-ga))

           (statistic-fields
            "evolved-minus-greedy"
            (statistic rows :evolved-minus-greedy)))))
      asset-counts)}))

(defn- csv-cell
  [value]
  (let [text
        (cond
          (nil? value) ""
          (keyword? value) (name value)
          :else (str value))]

    (if (re-find #"[\",\n\r]"
                 text)
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
             (str/join ","
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

(defn- write-results!
  [output-dir
   greedy-results-by-size
   seed-rows
   aggregate]
  (let [greedy-edn
        (io/file output-dir
                 "greedy-baseline-instances.edn")

        aggregate-edn
        (io/file output-dir
                 "greedy-baseline-aggregate.edn")

        seed-csv
        (io/file output-dir
                 "greedy-baseline-seed-results.csv")

        aggregate-csv
        (io/file output-dir
                 "greedy-baseline-aggregate-by-size.csv")]

    (spit greedy-edn
          (with-out-str
            (pprint/pprint
             greedy-results-by-size)))

    (spit aggregate-edn
          (with-out-str
            (pprint/pprint
             aggregate)))

    (write-csv!
     seed-csv
     seed-csv-headers
     seed-rows)

    (write-csv!
     aggregate-csv
     aggregate-csv-headers
     (:by-size aggregate))

    {:greedy-instances-edn
     (.getPath greedy-edn)

     :aggregate-edn
     (.getPath aggregate-edn)

     :seed-results-csv
     (.getPath seed-csv)

     :aggregate-by-size-csv
     (.getPath aggregate-csv)}))

(defn run-experiment
  [config-overrides]
  (let [{:keys [seed-start
                seed-count
                input-dir
                output-dir]
         :as config}
        (validate-config!
         (merge default-config
                config-overrides))

        input-directory
        (io/file input-dir)

        _input-check
        (when-not (.isDirectory input-directory)
          (throw
           (ex-info
            "Input result directory was not found."
            {:input-dir input-dir})))

        output-directory
        (ensure-directory!
         output-dir)

        seeds
        (range seed-start
               (+ seed-start seed-count))

        seed-results
        (mapv
         #(read-seed-result!
           input-dir
           %)
         seeds)

        scale-config
        (validate-common-scale-config!
         seed-results)

        asset-counts
        (:asset-counts scale-config)

        greedy-size-results
        (mapv
         #(evaluate-greedy-size
           %
           scale-config)
         asset-counts)

        greedy-results-by-size
        (into
         {}
         (map
          (juxt :asset-count identity)
          greedy-size-results))

        seed-rows
        (vec
         (mapcat
          (fn [seed-result]
            (let [size-map
                  (seed-size-map
                   seed-result)]

              (mapv
               (fn [asset-count]
                 (let [size-row
                       (get size-map
                            asset-count)

                       greedy-size-result
                       (get greedy-results-by-size
                            asset-count)]

                   (when-not size-row
                     (throw
                      (ex-info
                       "Seed result is missing an expected asset count."
                       {:seed (:seed seed-result)
                        :asset-count asset-count})))

                   (build-seed-row
                    (:seed seed-result)
                    size-row
                    greedy-size-result)))
               asset-counts)))
          seed-results))

        aggregate
        (aggregate-results
         seed-rows
         greedy-results-by-size)

        output-files
        (write-results!
         (.getPath output-directory)
         greedy-results-by-size
         seed-rows
         aggregate)]

    {:config config
     :scale-config scale-config
     :greedy-results-by-size greedy-results-by-size
     :seed-rows seed-rows
     :aggregate aggregate
     :output-files output-files}))

(defn- format-ci
  [row prefix]
  (format
   "%.6f [%.6f, %.6f]"
   (get row
        (keyword (str prefix "-mean")))
   (get row
        (keyword (str prefix "-ci95-lower")))
   (get row
        (keyword (str prefix "-ci95-upper")))))

(defn -main
  [& args]
  (let [config
        (parse-args args)

        result
        (run-experiment config)

        aggregate
        (:aggregate result)]

    (println)
    (println "Return-per-cost Greedy Baseline")
    (println "Algorithm seeds:"
             (:seed-count aggregate))
    (println "Greedy evaluations per instance: 1")
    (println)

    (doseq [row
            (:by-size aggregate)]
      (println "Assets:"
               (:asset-count row))

      (println "  Random:"
               (format-ci row
                          "random-score"))

      (println "  Standard GA:"
               (format-ci row
                          "ga-score"))

      (printf "  Greedy only: %.6f (instance SD %.6f)%n"
              (:greedy-score row)
              (:greedy-instance-score-standard-deviation row))

      (println "  Evolved DSL:"
               (format-ci row
                          "evolved-score"))

      (println "  Greedy - Random:"
               (format-ci row
                          "greedy-minus-random"))

      (println "  Greedy - GA:"
               (format-ci row
                          "greedy-minus-ga"))

      (println "  Evolved - Greedy:"
               (format-ci row
                          "evolved-minus-greedy"))

      (println))

    (println "Output files")
    (doseq [[file-type path]
            (:output-files result)]
      (println " "
               (name file-type)
               "->"
               path))))
