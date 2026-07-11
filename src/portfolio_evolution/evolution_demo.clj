(ns portfolio-evolution.evolution-demo
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [portfolio-evolution.core :as core]
   [portfolio-evolution.outer-gp :as outer-gp])
  (:gen-class))

(def history-headers
  [:generation
   :best-fitness
   :best-mean-normalized-score
   :best-minimum-normalized-score
   :best-mean-optimality-gap
   :population-mean-normalized-score
   :population-score-standard-deviation
   :best-program-node-count
   :best-program-depth
   :unique-program-count
   :generation-runtime-ms])

(def population-headers
  [:generation
   :rank
   :candidate-id
   :origin
   :parent-candidate-id
   :parent-rank
   :fitness
   :mean-normalized-score
   :minimum-normalized-score
   :mean-optimality-gap
   :program-node-count
   :program-depth
   :parsimony-penalty
   :mean-runtime-ms
   :program])

(defn- csv-cell
  [value]
  (let [text
        (cond
          (nil? value)
          ""

          (keyword? value)
          (name value)

          (seq? value)
          (pr-str value)

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

(defn- ensure-directory!
  [directory-path]
  (let [directory
        (io/file directory-path)]

    (when-not (.exists directory)
      (when-not (.mkdirs directory)
        (throw
         (ex-info
          "Could not create result directory."
          {:directory directory-path}))))

    directory))

(defn- write-results!
  [result results-dir]
  (let [directory
        (ensure-directory!
         results-dir)

        seed
        (:master-seed
         result)

        complete-edn
        (io/file
         directory
         (format
          "outer-evolution-seed-%d.edn"
          seed))

        history-csv
        (io/file
         directory
         (format
          "outer-evolution-history-seed-%d.csv"
          seed))

        final-population-csv
        (io/file
         directory
         (format
          "outer-evolution-final-population-seed-%d.csv"
          seed))

        best-optimizer-edn
        (io/file
         directory
         (format
          "best-evolved-optimizer-seed-%d.edn"
          seed))]

    (spit
     complete-edn
     (with-out-str
       (pprint/pprint
        result)))

    (write-csv!
     history-csv
     history-headers
     (:history result))

    (write-csv!
     final-population-csv
     population-headers
     (:final-candidates
      result))

    (spit
     best-optimizer-edn
     (with-out-str
       (pprint/pprint
        (:best-candidate
         result))))

    {:complete-edn
     (.getPath complete-edn)

     :history-csv
     (.getPath history-csv)

     :final-population-csv
     (.getPath final-population-csv)

     :best-optimizer-edn
     (.getPath best-optimizer-edn)}))

(defn- print-generation
  [{:keys [generation
           best-fitness
           best-mean-normalized-score
           best-minimum-normalized-score
           population-mean-normalized-score
           population-score-standard-deviation
           best-program-node-count
           best-program-depth
           unique-program-count
           generation-runtime-ms]}]

  (printf
   "  gen=%2d best-fitness=%.6f best-score=%.6f min=%.6f population-mean=%.6f sd=%.6f nodes=%2d depth=%d unique=%d runtime=%.2f ms%n"
   generation
   best-fitness
   best-mean-normalized-score
   best-minimum-normalized-score
   population-mean-normalized-score
   population-score-standard-deviation
   best-program-node-count
   best-program-depth
   unique-program-count
   generation-runtime-ms))

(defn -main
  [& args]
  (let [{:keys [seed
                results-dir]}
        (core/parse-args
         args)

        result
        (outer-gp/evolve
         seed)

        output-files
        (write-results!
         result
         results-dir)

        best-candidate
        (:best-candidate
         result)]

    (println "Outer Genetic Programming Evolution")
    (println "Master seed:" seed)
    (println)

    (println "Protocol")
    (println "  Outer population:"
             (get-in result
                     [:config
                      :outer-population-size]))
    (println "  Outer generations:"
             (:generation-count
              result))
    (println "  Tournament size:"
             (get-in result
                     [:config
                      :outer-tournament-size]))
    (println "  Elites:"
             (get-in result
                     [:config
                      :elite-count]))
    (println "  Training instances:"
             (:training-instance-count-used
              result))
    (println "  Test instances used:"
             (:test-instances-used
              result))
    (println "  Inner evaluation budget:"
             (get-in result
                     [:config
                      :inner-evaluation-budget]))
    (println)

    (println "Evolution history")
    (doseq [generation-summary
            (:history result)]
      (print-generation
       generation-summary))

    (println)
    (println "Best evolved optimizer")
    (println "  Final rank:"
             (:rank
              best-candidate))
    (println "  Fitness:"
             (format
              "%.6f"
              (:fitness
               best-candidate)))
    (println "  Mean training score:"
             (format
              "%.6f"
              (:mean-normalized-score
               best-candidate)))
    (println "  Minimum training score:"
             (format
              "%.6f"
              (:minimum-normalized-score
               best-candidate)))
    (println "  Nodes:"
             (:program-node-count
              best-candidate))
    (println "  Depth:"
             (:program-depth
              best-candidate))
    (println)

    (pprint/pprint
     (:program
      best-candidate))

    (println)
    (println "Training-instance performance")
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
    (println "Total outer runtime ms:"
             (format
              "%.3f"
              (:outer-runtime-ms
               result)))

    (println)
    (println "Output files")
    (doseq [[file-type path]
            output-files]
      (println
       " "
       (name file-type)
       "->"
       path))))
