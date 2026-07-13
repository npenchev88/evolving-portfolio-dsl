(ns portfolio-evolution.ablation-experiment
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [portfolio-evolution.dsl :as dsl]
   [portfolio-evolution.final-experiment :as final-experiment]
   [portfolio-evolution.metrics :as metrics]
   [portfolio-evolution.scale-evaluation :as scale-evaluation])
  (:gen-class))

(def default-config
  {:seed-start 1
   :seed-count 30
   :input-dir "results/final-30-seeds"
   :output-dir "results/ablation-30-seeds"
   :resume? true})

(def seed-csv-headers
  [:seed
   :asset-count

   :full-score
   :no-greedy-score
   :simple-body-score

   :full-minus-no-greedy
   :full-minus-simple-body

   :full-gap
   :no-greedy-gap
   :simple-body-gap

   :full-runtime-ms
   :no-greedy-runtime-ms
   :simple-body-runtime-ms])

(def aggregate-csv-headers
  [:asset-count
   :seed-count

   :full-score-mean
   :full-score-ci95-lower
   :full-score-ci95-upper

   :no-greedy-score-mean
   :no-greedy-score-ci95-lower
   :no-greedy-score-ci95-upper

   :simple-body-score-mean
   :simple-body-score-ci95-lower
   :simple-body-score-ci95-upper

   :full-minus-no-greedy-mean
   :full-minus-no-greedy-ci95-lower
   :full-minus-no-greedy-ci95-upper

   :full-minus-simple-body-mean
   :full-minus-simple-body-ci95-lower
   :full-minus-simple-body-ci95-upper])

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
          (let [value
                (second remaining)]

            (when-not value
              (throw
               (ex-info
                "--seed-start requires a value."
                {})))

            (recur
             (nnext remaining)
             (assoc
              config
              :seed-start
              (parse-long! value flag))))

          "--seed-count"
          (let [value
                (second remaining)]

            (when-not value
              (throw
               (ex-info
                "--seed-count requires a value."
                {})))

            (recur
             (nnext remaining)
             (assoc
              config
              :seed-count
              (parse-long! value flag))))

          "--input-dir"
          (let [value
                (second remaining)]

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
          (let [value
                (second remaining)]

            (when-not value
              (throw
               (ex-info
                "--output-dir requires a value."
                {})))

            (recur
             (nnext remaining)
             (assoc config
                    :output-dir value)))

          "--resume"
          (recur
           (next remaining)
           (assoc config
                  :resume? true))

          "--no-resume"
          (recur
           (next remaining)
           (assoc config
                  :resume? false))

          (throw
           (ex-info
            "Unknown command-line argument."
            {:argument flag})))))))

(defn- ensure-directory!
  [path]
  (let [directory
        (io/file path)]

    (when-not (.exists directory)
      (when-not (.mkdirs directory)
        (throw
         (ex-info
          "Could not create directory."
          {:path path}))))

    (when-not (.isDirectory directory)
      (throw
       (ex-info
        "Path is not a directory."
        {:path path})))

    directory))

(defn- input-seed-file
  [input-dir seed]
  (io/file
   input-dir
   (format "seed-%02d.edn"
           seed)))

(defn- output-seed-file
  [output-dir seed]
  (io/file
   output-dir
   (format "ablation-seed-%02d.edn"
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

(defn- read-final-program!
  [input-dir seed]
  (let [result
        (read-edn!
         (input-seed-file
          input-dir
          seed))

        program
        (get-in result
                [:outer
                 :final
                 :program])]

    (when-not program
      (throw
       (ex-info
        "Seed result does not contain a final optimizer."
        {:seed seed})))

    (dsl/validate! program)
    program))

(defn- initializer-population-size
  [initializer]
  (let [[operator & arguments]
        initializer]

    (case operator
      random-population
      (first arguments)

      mixed-population
      (+ (nth arguments 0)
         (nth arguments 1))

      (throw
       (ex-info
        "Unsupported initializer in ablation."
        {:initializer initializer})))))

(defn- without-greedy-initialization
  "Replaces the evolved initializer with an equally sized random population."
  [program]
  (let [[_ initializer repair-strategy body]
        program

        population-size
        (initializer-population-size
         initializer)

        ablated
        (list
         'optimizer
         (list
          'random-population
          population-size)
         repair-strategy
         body)]

    (dsl/validate! ablated)
    ablated))

(defn- with-simple-body
  "Preserves the evolved initializer and repair strategy, but replaces the
  evolved search body with simple random injection."
  [program]
  (let [[_ initializer repair-strategy _]
        program

        ablated
        (list
         'optimizer
         initializer
         repair-strategy
         (list
          'repeat-until-budget
          (list
           'inject-random
           5)))]

    (dsl/validate! ablated)
    ablated))

(defn- size-map
  [evaluation-result]
  (into
   {}
   (map
    (juxt :asset-count
          identity)
    (:sizes evaluation-result))))

(defn- evolved-value
  [size-result key]
  (get-in size-result
          [:evolved-summary key]))

(defn- build-size-row
  [seed
   full-size
   no-greedy-size
   simple-body-size]

  (let [asset-count
        (:asset-count full-size)

        full-score
        (evolved-value
         full-size
         :mean-normalized-score)

        no-greedy-score
        (evolved-value
         no-greedy-size
         :mean-normalized-score)

        simple-body-score
        (evolved-value
         simple-body-size
         :mean-normalized-score)]

    {:seed
     seed

     :asset-count
     asset-count

     :full-score
     full-score

     :no-greedy-score
     no-greedy-score

     :simple-body-score
     simple-body-score

     :full-minus-no-greedy
     (- full-score
        no-greedy-score)

     :full-minus-simple-body
     (- full-score
        simple-body-score)

     :full-gap
     (evolved-value
      full-size
      :mean-optimality-gap)

     :no-greedy-gap
     (evolved-value
      no-greedy-size
      :mean-optimality-gap)

     :simple-body-gap
     (evolved-value
      simple-body-size
      :mean-optimality-gap)

     :full-runtime-ms
     (evolved-value
      full-size
      :mean-runtime-ms)

     :no-greedy-runtime-ms
     (evolved-value
      no-greedy-size
      :mean-runtime-ms)

     :simple-body-runtime-ms
     (evolved-value
      simple-body-size
      :mean-runtime-ms)}))

(defn- run-one-seed
  [seed input-dir scale-config]
  (let [full-program
        (read-final-program!
         input-dir
         seed)

        no-greedy-program
        (without-greedy-initialization
         full-program)

        simple-body-program
        (with-simple-body
         full-program)

        full-evaluation
        (scale-evaluation/evaluate
         seed
         full-program
         scale-config)

        no-greedy-evaluation
        (scale-evaluation/evaluate
         seed
         no-greedy-program
         scale-config)

        simple-body-evaluation
        (scale-evaluation/evaluate
         seed
         simple-body-program
         scale-config)

        full-sizes
        (size-map full-evaluation)

        no-greedy-sizes
        (size-map no-greedy-evaluation)

        simple-body-sizes
        (size-map simple-body-evaluation)

        asset-counts
        (sort
         (keys full-sizes))

        rows
        (mapv
         (fn [asset-count]
           (build-size-row
            seed
            (get full-sizes
                 asset-count)
            (get no-greedy-sizes
                 asset-count)
            (get simple-body-sizes
                 asset-count)))
         asset-counts)]

    {:seed
     seed

     :programs
     {:full
      full-program

      :no-greedy
      no-greedy-program

      :simple-body
      simple-body-program}

     :sizes
     rows}))

(defn- statistic
  [rows field]
  (metrics/confidence-interval-95
   (mapv field rows)))

(defn- statistic-fields
  [prefix statistics]
  {(keyword
    (str prefix "-mean"))
   (:mean statistics)

   (keyword
    (str prefix "-ci95-lower"))
   (:ci95-lower statistics)

   (keyword
    (str prefix "-ci95-upper"))
   (:ci95-upper statistics)})

(defn- aggregate-results
  [seed-results]
  (let [all-size-rows
        (vec
         (mapcat :sizes
                 seed-results))

        asset-counts
        (->> all-size-rows
             (map :asset-count)
             distinct
             sort
             vec)

        by-size
        (mapv
         (fn [asset-count]
           (let [rows
                 (filterv
                  #(= asset-count
                      (:asset-count %))
                  all-size-rows)]

             (merge
              {:asset-count
               asset-count

               :seed-count
               (count rows)}

              (statistic-fields
               "full-score"
               (statistic rows
                          :full-score))

              (statistic-fields
               "no-greedy-score"
               (statistic rows
                          :no-greedy-score))

              (statistic-fields
               "simple-body-score"
               (statistic rows
                          :simple-body-score))

              (statistic-fields
               "full-minus-no-greedy"
               (statistic rows
                          :full-minus-no-greedy))

              (statistic-fields
               "full-minus-simple-body"
               (statistic rows
                          :full-minus-simple-body)))))
         asset-counts)]

    {:seed-count
     (count seed-results)

     :by-size
     by-size}))

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
       (map csv-cell headers))
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

(defn- write-seed-result!
  [output-dir result]
  (let [file
        (output-seed-file
         output-dir
         (:seed result))]

    (spit
     file
     (with-out-str
       (pprint/pprint result)))

    result))

(defn- write-aggregate-results!
  [output-dir seed-results aggregate]
  (let [aggregate-edn
        (io/file output-dir
                 "ablation-aggregate.edn")

        seed-results-csv
        (io/file output-dir
                 "ablation-seed-results.csv")

        aggregate-csv
        (io/file output-dir
                 "ablation-aggregate-by-size.csv")]

    (spit
     aggregate-edn
     (with-out-str
       (pprint/pprint aggregate)))

    (write-csv!
     seed-results-csv
     seed-csv-headers
     (mapcat :sizes
             seed-results))

    (write-csv!
     aggregate-csv
     aggregate-csv-headers
     (:by-size aggregate))

    {:aggregate-edn
     (.getPath aggregate-edn)

     :seed-results-csv
     (.getPath seed-results-csv)

     :aggregate-csv
     (.getPath aggregate-csv)}))

(defn- format-ci
  [row prefix]
  (let [mean-value
        (get row
             (keyword
              (str prefix "-mean")))

        lower
        (get row
             (keyword
              (str prefix "-ci95-lower")))

        upper
        (get row
             (keyword
              (str prefix "-ci95-upper")))]

    (if (nil? lower)
      (format "%.6f"
              mean-value)

      (format
       "%.6f [%.6f, %.6f]"
       mean-value
       lower
       upper))))

(defn run-batch
  [config]
  (let [{:keys [seed-start
                seed-count
                input-dir
                output-dir
                resume?]}
        config

        _input-check
        (when-not (.isDirectory
                   (io/file input-dir))
          (throw
           (ex-info
            "Input result directory was not found."
            {:input-dir input-dir})))

        output-directory
        (ensure-directory!
         output-dir)

        resolved-output-dir
        (.getPath output-directory)

        scale-config
        (:scale-config
         final-experiment/default-config)

        seeds
        (range seed-start
               (+ seed-start
                  seed-count))

        seed-results
        (mapv
         (fn [seed]
           (let [result-file
                 (output-seed-file
                  resolved-output-dir
                  seed)]

             (if (and resume?
                      (.isFile result-file))

               (do
                 (println
                  (format
                   "[seed %d] loading existing ablation"
                   seed))
                 (flush)
                 (read-edn!
                  result-file))

               (do
                 (println
                  (format
                   "[seed %d] running ablation"
                   seed))
                 (flush)

                 (let [started-at
                       (System/nanoTime)

                       result
                       (run-one-seed
                        seed
                        input-dir
                        scale-config)

                       elapsed-ms
                       (/ (double
                           (- (System/nanoTime)
                              started-at))
                          1000000.0)]

                   (write-seed-result!
                    resolved-output-dir
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
        (aggregate-results
         seed-results)

        output-files
        (write-aggregate-results!
         resolved-output-dir
         seed-results
         aggregate)]

    {:seed-results
     seed-results

     :aggregate
     aggregate

     :output-files
     output-files}))

(defn -main
  [& args]
  (let [config
        (parse-args args)

        result
        (run-batch config)

        aggregate
        (:aggregate result)]

    (println)
    (println "DSL Ablation Experiment")
    (println "Seeds:"
             (:seed-count aggregate))
    (println)

    (doseq [row
            (:by-size aggregate)]

      (println "Assets:"
               (:asset-count row))

      (println "  Full optimizer:"
               (format-ci
                row
                "full-score"))

      (println "  Without greedy initialization:"
               (format-ci
                row
                "no-greedy-score"))

      (println "  Simple body:"
               (format-ci
                row
                "simple-body-score"))

      (println "  Full - no greedy:"
               (format-ci
                row
                "full-minus-no-greedy"))

      (println "  Full - simple body:"
               (format-ci
                row
                "full-minus-simple-body"))

      (println))

    (println "Output files")
    (doseq [[file-type path]
            (:output-files result)]

      (println
       " "
       (name file-type)
       "->"
       path))))
