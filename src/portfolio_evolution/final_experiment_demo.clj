(ns portfolio-evolution.final-experiment-demo
  (:require
   [portfolio-evolution.final-experiment :as experiment])
  (:gen-class))

(defn- parse-long!
  [value flag]
  (try
    (Long/parseLong
     value)
    (catch NumberFormatException _
      (throw
       (ex-info
        "Expected an integer command-line value."
        {:flag flag
         :value value})))))

(defn parse-args
  [args]
  (loop [remaining
         (seq args)

         config
         {:seed-start 1
          :seed-count 30
          :results-dir
          "results/final-experiment"
          :resume? true}]

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
              (parse-long!
               value
               flag))))

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
              (parse-long!
               value
               flag))))

          "--results-dir"
          (let [value
                (second remaining)]

            (when-not value
              (throw
               (ex-info
                "--results-dir requires a value."
                {})))

            (recur
             (nnext remaining)
             (assoc
              config
              :results-dir
              value)))

          "--resume"
          (recur
           (next remaining)
           (assoc
            config
            :resume?
            true))

          "--no-resume"
          (recur
           (next remaining)
           (assoc
            config
            :resume?
            false))

          (throw
           (ex-info
            "Unknown command-line argument."
            {:argument flag})))))))

(defn- format-ci
  [statistics]
  (if (nil?
       (:ci95-lower
        statistics))

    (format
     "%.6f (CI not estimable)"
     (:mean statistics))

    (format
     "%.6f [%.6f, %.6f]"
     (:mean statistics)
     (:ci95-lower statistics)
     (:ci95-upper statistics))))

(defn- print-size
  [row]
  (printf
   "  assets=%3d  random=%s  ga=%s  evolved=%s%n"
   (:asset-count row)

   (format
    "%.6f [%.6f, %.6f]"
    (:random-score-mean row)
    (:random-score-ci95-lower row)
    (:random-score-ci95-upper row))

   (format
    "%.6f [%.6f, %.6f]"
    (:ga-score-mean row)
    (:ga-score-ci95-lower row)
    (:ga-score-ci95-upper row))

   (format
    "%.6f [%.6f, %.6f]"
    (:final-dsl-score-mean row)
    (:final-dsl-score-ci95-lower row)
    (:final-dsl-score-ci95-upper row)))

  (printf
   "              evolved-random=%+.6f [%.6f, %.6f]  evolved-ga=%+.6f [%.6f, %.6f]%n"
   (:final-minus-random-mean row)
   (:final-minus-random-ci95-lower row)
   (:final-minus-random-ci95-upper row)

   (:final-minus-ga-mean row)
   (:final-minus-ga-ci95-lower row)
   (:final-minus-ga-ci95-upper row))

  (printf
   "              final-initial=%+.6f [%.6f, %.6f]%n"
   (:final-minus-initial-mean row)
   (:final-minus-initial-ci95-lower row)
   (:final-minus-initial-ci95-upper row)))

(defn -main
  [& args]
  (let [config
        (parse-args
         args)

        result
        (experiment/run-batch
         config)

        aggregate
        (:aggregate
         result)

        outer
        (:outer
         aggregate)]

    (println)
    (println "Final Multi-seed Experiment")
    (println "Seeds:"
             (:seed-count aggregate))
    (println)

    (println "Outer evolution changes")
    (println "  Fitness improvement:"
             (format-ci
              (:fitness-improvement
               outer)))

    (println "  Training score improvement:"
             (format-ci
              (:training-score-improvement
               outer)))

    (println "  Node reduction:"
             (format-ci
              (:node-reduction
               outer)))

    (println "  Depth reduction:"
             (format-ci
              (:depth-reduction
               outer)))

    (println)
    (println "Scale-generalization results")
    (doseq [size-row
            (:by-size aggregate)]
      (print-size
       size-row))

    (println)
    (println "Output files")
    (doseq [[file-type path]
            (:output-files result)]
      (println
       " "
       (name file-type)
       "->"
       path))))
