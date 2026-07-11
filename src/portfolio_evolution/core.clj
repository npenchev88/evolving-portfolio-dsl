(ns portfolio-evolution.core
  (:require
   [portfolio-evolution.experiment :as experiment])
  (:gen-class))

(defn parse-long-argument
  [value]
  (try
    (Long/parseLong value)
    (catch NumberFormatException _
      (throw
       (ex-info "Expected an integer value."
                {:value value})))))

(defn parse-args
  [args]
  (loop [remaining args
         config {:seed 1
                 :results-dir "results"}]

    (if (empty? remaining)
      config

      (let [[flag value & more]
            remaining]

        (case flag
          "--seed"
          (do
            (when-not value
              (throw
               (ex-info "--seed requires an integer value."
                        {:arguments args})))

            (recur
             more
             (assoc config
                    :seed
                    (parse-long-argument
                     value))))

          "--results-dir"
          (do
            (when-not value
              (throw
               (ex-info
                "--results-dir requires a directory path."
                {:arguments args})))

            (recur
             more
             (assoc config
                    :results-dir
                    value)))

          (throw
           (ex-info "Unknown command-line argument."
                    {:argument flag
                     :arguments args})))))))

(defn- print-instance-result
  [{:keys [instance-id
           normalized-score
           optimality-gap
           best-objective
           optimal-objective
           runtime-ms]}]

  (printf
   "  %-9s score=%.6f gap=%.6f objective=%d/%d runtime=%.3f ms%n"
   instance-id
   normalized-score
   optimality-gap
   best-objective
   optimal-objective
   runtime-ms))

(defn -main
  [& args]
  (let [{:keys [seed results-dir]}
        (parse-args args)

        result
        (experiment/run-baseline-experiment
         {:master-seed seed
          :results-dir results-dir})

        summary
        (:summary result)]

    (println "Evolving Portfolio DSL")
    (println "Baseline GA held-out evaluation")
    (println "Master algorithm seed:" seed)
    (println)

    (println "Protocol")
    (println "  Training instances generated:"
             (:training-instance-count-generated
              result))
    (println "  Training instances used:"
             (:training-instances-used
              result))
    (println "  Held-out test instances:"
             (:test-instance-count
              result))
    (println)

    (println "Per-instance results")
    (doseq [instance-result
            (:instances result)]
      (print-instance-result
       instance-result))

    (println)
    (println "Seed-level aggregate")
    (println "  Mean normalized score:"
             (format
              "%.6f"
              (:mean-normalized-score
               summary)))
    (println "  Minimum normalized score:"
             (format
              "%.6f"
              (:minimum-normalized-score
               summary)))
    (println "  Maximum normalized score:"
             (format
              "%.6f"
              (:maximum-normalized-score
               summary)))
    (println "  Mean optimality gap:"
             (format
              "%.6f"
              (:mean-optimality-gap
               summary)))
    (println "  Mean runtime ms:"
             (format
              "%.3f"
              (:mean-runtime-ms
               summary)))
    (println "  Mean best-found fraction:"
             (format
              "%.4f"
              (:mean-best-found-fraction
               summary)))
    (println "  All final solutions feasible:"
             (:all-final-solutions-feasible?
              summary))
    (println)

    (println "Statistical status")
    (println
     "  One master seed: smoke test only; 95% CI is not yet estimable.")
    (println)

    (println "Output files")
    (doseq [[file-type path]
            (:output-files result)]
      (println " "
               (name file-type)
               "->"
               path))))
