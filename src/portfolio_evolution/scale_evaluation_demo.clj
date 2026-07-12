(ns portfolio-evolution.scale-evaluation-demo
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [portfolio-evolution.core :as core]
   [portfolio-evolution.dsl :as dsl]
   [portfolio-evolution.scale-evaluation :as evaluation])
  (:gen-class))

(def summary-headers
  [:asset-count
   :instance-count
   :random-mean-score
   :ga-mean-score
   :evolved-mean-score
   :random-mean-gap
   :ga-mean-gap
   :evolved-mean-gap
   :evolved-minus-random
   :evolved-minus-ga
   :evolved-wins-vs-random
   :evolved-wins-vs-ga
   :random-mean-runtime-ms
   :ga-mean-runtime-ms
   :evolved-mean-runtime-ms])

(def instance-headers
  [:asset-count
   :instance-index
   :instance-id
   :instance-seed
   :budget
   :optimal-objective

   :random-objective
   :random-score
   :random-gap
   :random-runtime-ms

   :ga-objective
   :ga-score
   :ga-gap
   :ga-runtime-ms

   :evolved-objective
   :evolved-score
   :evolved-gap
   :evolved-runtime-ms

   :evolved-minus-random
   :evolved-minus-ga])

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

      (dsl/validate!
       program)

      program)))

(defn- summary-row
  [size-result]
  {:asset-count
   (:asset-count
    size-result)

   :instance-count
   (:instance-count
    size-result)

   :random-mean-score
   (get-in size-result
           [:random-summary
            :mean-normalized-score])

   :ga-mean-score
   (get-in size-result
           [:ga-summary
            :mean-normalized-score])

   :evolved-mean-score
   (get-in size-result
           [:evolved-summary
            :mean-normalized-score])

   :random-mean-gap
   (get-in size-result
           [:random-summary
            :mean-optimality-gap])

   :ga-mean-gap
   (get-in size-result
           [:ga-summary
            :mean-optimality-gap])

   :evolved-mean-gap
   (get-in size-result
           [:evolved-summary
            :mean-optimality-gap])

   :evolved-minus-random
   (get-in size-result
           [:evolved-vs-random
            :mean-score-difference])

   :evolved-minus-ga
   (get-in size-result
           [:evolved-vs-ga
            :mean-score-difference])

   :evolved-wins-vs-random
   (get-in size-result
           [:evolved-vs-random
            :wins])

   :evolved-wins-vs-ga
   (get-in size-result
           [:evolved-vs-ga
            :wins])

   :random-mean-runtime-ms
   (get-in size-result
           [:random-summary
            :mean-runtime-ms])

   :ga-mean-runtime-ms
   (get-in size-result
           [:ga-summary
            :mean-runtime-ms])

   :evolved-mean-runtime-ms
   (get-in size-result
           [:evolved-summary
            :mean-runtime-ms])})

(defn- instance-row
  [comparison]
  {:asset-count
   (:asset-count
    comparison)

   :instance-index
   (:instance-index
    comparison)

   :instance-id
   (:instance-id
    comparison)

   :instance-seed
   (:instance-seed
    comparison)

   :budget
   (:budget
    comparison)

   :optimal-objective
   (:optimal-objective
    comparison)

   :random-objective
   (get-in comparison
           [:random
            :best-objective])

   :random-score
   (get-in comparison
           [:random
            :normalized-score])

   :random-gap
   (get-in comparison
           [:random
            :optimality-gap])

   :random-runtime-ms
   (get-in comparison
           [:random
            :runtime-ms])

   :ga-objective
   (get-in comparison
           [:ga
            :best-objective])

   :ga-score
   (get-in comparison
           [:ga
            :normalized-score])

   :ga-gap
   (get-in comparison
           [:ga
            :optimality-gap])

   :ga-runtime-ms
   (get-in comparison
           [:ga
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

   :evolved-minus-random
   (:evolved-minus-random
    comparison)

   :evolved-minus-ga
   (:evolved-minus-ga
    comparison)})

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

    directory))

(defn- write-results!
  [result results-dir seed]
  (let [directory
        (ensure-directory!
         results-dir)

        edn-file
        (io/file
         directory
         (format
          "scale-evaluation-seed-%d.edn"
          seed))

        summary-csv
        (io/file
         directory
         (format
          "scale-evaluation-summary-seed-%d.csv"
          seed))

        instances-csv
        (io/file
         directory
         (format
          "scale-evaluation-instances-seed-%d.csv"
          seed))

        summary-rows
        (mapv summary-row
              (:sizes result))

        instance-rows
        (mapv instance-row
              (mapcat :instances
                      (:sizes result)))]

    (spit
     edn-file
     (with-out-str
       (pprint/pprint
        result)))

    (write-csv!
     summary-csv
     summary-headers
     summary-rows)

    (write-csv!
     instances-csv
     instance-headers
     instance-rows)

    {:edn
     (.getPath edn-file)

     :summary-csv
     (.getPath summary-csv)

     :instances-csv
     (.getPath instances-csv)}))

(defn- print-size-result
  [size-result]
  (let [asset-count
        (:asset-count
         size-result)

        random-score
        (get-in size-result
                [:random-summary
                 :mean-normalized-score])

        ga-score
        (get-in size-result
                [:ga-summary
                 :mean-normalized-score])

        evolved-score
        (get-in size-result
                [:evolved-summary
                 :mean-normalized-score])

        evolved-minus-random
        (get-in size-result
                [:evolved-vs-random
                 :mean-score-difference])

        evolved-minus-ga
        (get-in size-result
                [:evolved-vs-ga
                 :mean-score-difference])]

    (printf
     "  assets=%3d random=%.6f ga=%.6f evolved=%.6f evolved-random=%+.6f evolved-ga=%+.6f%n"
     asset-count
     random-score
     ga-score
     evolved-score
     evolved-minus-random
     evolved-minus-ga)

    (printf
     "             wins vs random=%d/%d   wins vs GA=%d/%d%n"
     (get-in size-result
             [:evolved-vs-random
              :wins])

     (:instance-count
      size-result)

     (get-in size-result
             [:evolved-vs-ga
              :wins])

     (:instance-count
      size-result))))

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
         seed)]

    (println "Scale Generalization Evaluation")
    (println "Master seed:" seed)
    (println "Training instances used during this phase:"
             (:training-instances-used
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

    (println "Size-level results")
    (doseq [size-result
            (:sizes result)]
      (print-size-result
       size-result))

    (println)
    (println
     "One master seed: smoke test only; confidence intervals are not estimable.")

    (println "Total evaluation runtime ms:"
             (format
              "%.3f"
              (:total-runtime-ms
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
