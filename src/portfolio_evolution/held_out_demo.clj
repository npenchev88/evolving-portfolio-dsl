(ns portfolio-evolution.held-out-demo
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [portfolio-evolution.core :as core]
   [portfolio-evolution.held-out-evaluation :as evaluation])
  (:gen-class))

(def csv-headers
  [:instance-index
   :instance-id
   :instance-seed
   :asset-count
   :budget
   :optimal-objective
   :baseline-objective
   :baseline-score
   :baseline-gap
   :baseline-runtime-ms
   :evolved-objective
   :evolved-score
   :evolved-gap
   :evolved-runtime-ms
   :score-difference
   :gap-improvement])

(defn- csv-cell
  [value]
  (let [text
        (if (nil? value)
          ""
          (str value))]

    (if (re-find #"[\",\n\r]"
                 text)
      (str "\""
           (str/replace
            text
            "\""
            "\"\"")
           "\"")
      text)))

(defn- write-csv!
  [file rows]
  (with-open [writer
              (io/writer file)]

    (.write
     writer
     (str
      (str/join
       ","
       (map csv-cell
            csv-headers))
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
          csv-headers))
        "\n")))))

(defn- flatten-comparison
  [comparison]
  {:instance-index
   (:instance-index comparison)

   :instance-id
   (:instance-id comparison)

   :instance-seed
   (:instance-seed comparison)

   :asset-count
   (:asset-count comparison)

   :budget
   (:budget comparison)

   :optimal-objective
   (:optimal-objective comparison)

   :baseline-objective
   (get-in comparison
           [:baseline
            :best-objective])

   :baseline-score
   (get-in comparison
           [:baseline
            :normalized-score])

   :baseline-gap
   (get-in comparison
           [:baseline
            :optimality-gap])

   :baseline-runtime-ms
   (get-in comparison
           [:baseline
            :runtime-ms])

   :evolved-objective
   (get-in comparison
           [:evolved
            :best-objective])

   :evolved-score
   (get-in comparison
           [:evolved
            :normalized-score])

   :evolved-gap
   (get-in comparison
           [:evolved
            :optimality-gap])

   :evolved-runtime-ms
   (get-in comparison
           [:evolved
            :runtime-ms])

   :score-difference
   (:normalized-score-difference
    comparison)

   :gap-improvement
   (:optimality-gap-improvement
    comparison)})

(defn- read-best-program!
  [results-dir seed]
  (let [file
        (io/file
         results-dir
         (format
          "best-evolved-optimizer-seed-%d.edn"
          seed))]

    (when-not (.isFile file)
      (throw
       (ex-info
        "Best evolved optimizer file was not found. Run :evolve-demo first."
        {:file
         (.getPath file)})))

    (let [candidate
          (edn/read-string
           (slurp file))

          program
          (:program candidate)]

      (when-not program
        (throw
         (ex-info
          "Saved candidate does not contain an optimizer program."
          {:file
           (.getPath file)})))

      program)))

(defn- write-results!
  [result results-dir seed]
  (let [directory
        (io/file
         results-dir)

        _directory
        (when-not (.exists directory)
          (.mkdirs directory))

        edn-file
        (io/file
         directory
         (format
          "held-out-comparison-seed-%d.edn"
          seed))

        csv-file
        (io/file
         directory
         (format
          "held-out-comparison-seed-%d.csv"
          seed))]

    (spit
     edn-file
     (with-out-str
       (pprint/pprint
        result)))

    (write-csv!
     csv-file
     (mapv flatten-comparison
           (:instances result)))

    {:edn
     (.getPath edn-file)

     :csv
     (.getPath csv-file)}))

(defn -main
  [& args]
  (let [{:keys [seed
                results-dir]}
        (core/parse-args
         args)

        optimizer-program
        (read-best-program!
         results-dir
         seed)

        result
        (evaluation/evaluate
         seed
         optimizer-program)

        output-files
        (write-results!
         result
         results-dir
         seed)

        baseline-summary
        (:baseline-summary
         result)

        evolved-summary
        (:evolved-summary
         result)

        paired-summary
        (:paired-summary
         result)]

    (println "Held-out Evaluation")
    (println "Master seed:" seed)
    (println "Training instances used:"
             (:training-instances-used
              result))
    (println "Test instances used:"
             (:test-instance-count-used
              result))
    (println "Evaluation budget per method and instance:"
             (get-in result
                     [:config
                      :evaluation-budget]))
    (println)

    (println "Frozen evolved optimizer")
    (pprint/pprint
     optimizer-program)
    (println)

    (println "Per-instance comparison")
    (doseq [comparison
            (:instances result)]

      (printf
       "  %-9s baseline=%.6f evolved=%.6f difference=%+.6f optimum=%d%n"
       (:instance-id comparison)
       (get-in comparison
               [:baseline
                :normalized-score])
       (get-in comparison
               [:evolved
                :normalized-score])
       (:normalized-score-difference
        comparison)
       (:optimal-objective
        comparison)))

    (println)
    (println "Seed-level aggregate")

    (println "  Baseline mean score:"
             (format
              "%.6f"
              (:mean-normalized-score
               baseline-summary)))

    (println "  Evolved mean score:"
             (format
              "%.6f"
              (:mean-normalized-score
               evolved-summary)))

    (println "  Baseline mean gap:"
             (format
              "%.6f"
              (:mean-optimality-gap
               baseline-summary)))

    (println "  Evolved mean gap:"
             (format
              "%.6f"
              (:mean-optimality-gap
               evolved-summary)))

    (println "  Mean paired score difference:"
             (format
              "%+.6f"
              (:mean-normalized-score-difference
               paired-summary)))

    (println "  Evolved wins:"
             (:evolved-wins
              paired-summary))

    (println "  Ties:"
             (:ties
              paired-summary))

    (println "  Baseline wins:"
             (:baseline-wins
              paired-summary))

    (println)
    (println
     "One master seed: this is a smoke test, not a confidence-interval result.")

    (println)
    (println "Output files")
    (doseq [[file-type path]
            output-files]
      (println
       " "
       (name file-type)
       "->"
       path))))
