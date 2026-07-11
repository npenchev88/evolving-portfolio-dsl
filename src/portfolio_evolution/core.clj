(ns portfolio-evolution.core
  (:gen-class))

(defn parse-long
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
         config {:seed 1}]
    (if (empty? remaining)
      config
      (let [[flag value & more] remaining]
        (case flag
          "--seed"
          (do
            (when-not value
              (throw
               (ex-info "--seed requires an integer value."
                        {:arguments args})))
            (recur more
                   (assoc config :seed (parse-long value))))

          (throw
           (ex-info "Unknown command-line argument."
                    {:argument flag
                     :arguments args})))))))

(defn -main
  [& args]
  (let [{:keys [seed]} (parse-args args)]
    (println "Evolving Portfolio DSL")
    (println "Clojure version:" (clojure-version))
    (println "Algorithm seed:" seed)
    (println "Status: environment ready")))
