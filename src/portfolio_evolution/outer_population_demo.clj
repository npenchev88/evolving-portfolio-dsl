(ns portfolio-evolution.outer-population-demo
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [portfolio-evolution.core :as core]
   [portfolio-evolution.outer-gp :as outer-gp])
  (:gen-class))

(def csv-headers
  [:rank
   :candidate-id
   :fitness
   :mean-normalized-score
   :minimum-normalized-score
   :maximum-normalized-score
   :instance-score-standard-deviation
   :mean-optimality-gap
   :program-node-count
   :program-depth
   :parsimony-penalty
   :mean-runtime-ms
   :all-solutions-feasible?])

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

(defn- write-results!
  [result results-dir]
  (let [directory
        (io/file
         results-dir)

        _directory-creation
        (when-not (.exists directory)
          (when-not (.mkdirs directory)
            (throw
             (ex-info
              "Could not create results directory."
              {:results-dir results-dir}))))

        master-seed
        (:master-seed
         result)

        population-edn
        (io/file
         directory
         (format
          "outer-initial-population-seed-%d.edn"
          master-seed))

        population-csv
        (io/file
         directory
         (format
          "outer-initial-population-seed-%d.csv"
          master-seed))

        best-optimizer-edn
        (io/file
         directory
         (format
          "best-initial-optimizer-seed-%d.edn"
          master-seed))]

    (spit
     population-edn
     (with-out-str
       (pprint/pprint
        result)))

    (write-csv!
     population-csv
     (:candidates result))

    (spit
     best-optimizer-edn
     (with-out-str
       (pprint/pprint
        (:best-candidate
         result))))

    {:population-edn
     (.getPath
      population-edn)

     :population-csv
     (.getPath
      population-csv)

     :best-optimizer-edn
     (.getPath
      best-optimizer-edn)}))

(defn- print-candidate
  [{:keys [rank
           candidate-id
           fitness
           mean-normalized-score
           minimum-normalized-score
           mean-optimality-gap
           program-node-count
           program-depth
           mean-runtime-ms]}]

  (printf
   "  rank=%2d id=%2d fitness=%.6f mean=%.6f min=%.6f gap=%.6f nodes=%2d depth=%d runtime=%.2f ms%n"
   rank
   candidate-id
   fitness
   mean-normalized-score
   minimum-normalized-score
   mean-optimality-gap
   program-node-count
   program-depth
   mean-runtime-ms))

(defn -main
  [& args]
  (let [{:keys [seed
                results-dir]}
        (core/parse-args
         args)

        result
        (outer-gp/evaluate-initial-population
         seed)

        output-files
        (write-results!
         result
         results-dir)

        summary
        (:summary result)

        best-candidate
        (:best-candidate
         result)]

    (println "Outer GP — Initial Population")
    (println "Generation:" (:generation result))
    (println "Master seed:" seed)
    (println)

    (println "Protocol")
    (println "  Candidate optimizers:"
             (:candidate-count
              summary))
    (println "  Unique programs:"
             (:unique-program-count
              summary))
    (println "  Training instances used:"
             (:training-instance-count-used
              result))
    (println "  Test instances used:"
             (:test-instances-used
              result))
    (println "  Inner evaluation budget:"
             (get-in result
                     [:config
                      :inner-evaluation-budget]))
    (println "  Common inner seeds:"
             (:common-inner-seeds
              result))
    (println)

    (println "Ranked candidate optimizers")
    (doseq [candidate
            (:candidates result)]
      (print-candidate
       candidate))

    (println)
    (println "Generation-0 summary")
    (println "  Population mean score:"
             (format
              "%.6f"
              (:population-mean-normalized-score
               summary)))
    (println "  Population score standard deviation:"
             (format
              "%.6f"
              (:population-score-standard-deviation
               summary)))
    (println "  Best mean score:"
             (format
              "%.6f"
              (:best-mean-normalized-score
               summary)))
    (println "  Best minimum score:"
             (format
              "%.6f"
              (:best-minimum-normalized-score
               summary)))
    (println "  Best fitness:"
             (format
              "%.6f"
              (:best-fitness
               summary)))
    (println "  Outer evaluation runtime ms:"
             (format
              "%.3f"
              (:outer-runtime-ms
               result)))

    (println)
    (println "Best optimizer program")
    (pprint/pprint
     (:program
      best-candidate))

    (println)
    (println "Best optimizer training-instance results")
    (doseq [{:keys [instance-id
                    normalized-score
                    optimality-gap
                    best-objective
                    optimal-objective]}
            (:instance-results
             best-candidate)]

      (printf
       "  %-9s score=%.6f gap=%.6f objective=%d/%d%n"
       instance-id
       normalized-score
       optimality-gap
       best-objective
       optimal-objective))

    (println)
    (println "Output files")
    (doseq [[file-type path]
            output-files]
      (println " "
               (name file-type)
               "->"
               path))))
