(ns portfolio-evolution.metrics)

(defn normalized-score
  "Returns obtained / optimal.

  A score of 1.0 means that the known optimum was reached."
  [obtained-objective optimal-objective]
  (when (neg? obtained-objective)
    (throw
     (ex-info "Obtained objective must not be negative."
              {:obtained-objective obtained-objective})))

  (when (neg? optimal-objective)
    (throw
     (ex-info "Optimal objective must not be negative."
              {:optimal-objective optimal-objective})))

  (if (zero? optimal-objective)
    (if (zero? obtained-objective)
      1.0
      0.0)

    (/ (double obtained-objective)
       (double optimal-objective))))

(defn optimality-gap
  "Returns the relative distance from the known optimum.

  A gap of 0.0 means that the optimum was reached."
  [obtained-objective optimal-objective]
  (when (neg? obtained-objective)
    (throw
     (ex-info "Obtained objective must not be negative."
              {:obtained-objective obtained-objective})))

  (when (neg? optimal-objective)
    (throw
     (ex-info "Optimal objective must not be negative."
              {:optimal-objective optimal-objective})))

  (if (zero? optimal-objective)
    0.0

    (/ (- (double optimal-objective)
          (double obtained-objective))
       (double optimal-objective))))

(defn score
  [obtained-objective optimal-objective]
  {:normalized-score
   (normalized-score
    obtained-objective
    optimal-objective)

   :optimality-gap
   (optimality-gap
    obtained-objective
    optimal-objective)})

(defn mean
  [values]
  (let [values (mapv double values)]
    (when (empty? values)
      (throw
       (ex-info "Cannot calculate a mean over an empty collection."
                {})))

    (/ (reduce + values)
       (double (count values)))))

(defn sample-standard-deviation
  "Calculates sample standard deviation using denominator n - 1.

  For a single observation, returns 0.0."
  [values]
  (let [values (mapv double values)
        observation-count (count values)]

    (when (zero? observation-count)
      (throw
       (ex-info
        "Cannot calculate standard deviation over an empty collection."
        {})))

    (if (= observation-count 1)
      0.0

      (let [average (mean values)
            squared-deviations
            (map
             (fn [value]
               (let [difference
                     (- value average)]
                 (* difference difference)))
             values)]

        (Math/sqrt
         (/ (reduce + squared-deviations)
            (double
             (dec observation-count))))))))

(defn summarize-instance-results
  "Aggregates the results for one master algorithm seed across a fixed
  collection of test instances.

  This is not a confidence interval. The test instances are averaged to
  obtain one seed-level result."
  [instance-results]
  (when (empty? instance-results)
    (throw
     (ex-info "Cannot summarize an empty result collection."
              {})))

  (let [normalized-scores
        (mapv :normalized-score
              instance-results)

        optimality-gaps
        (mapv :optimality-gap
              instance-results)

        runtimes
        (mapv :runtime-ms
              instance-results)

        best-found-fractions
        (mapv :best-found-fraction
              instance-results)]

    {:instance-count
     (count instance-results)

     :mean-normalized-score
     (mean normalized-scores)

     :minimum-normalized-score
     (apply min normalized-scores)

     :maximum-normalized-score
     (apply max normalized-scores)

     :instance-score-standard-deviation
     (sample-standard-deviation
      normalized-scores)

     :mean-optimality-gap
     (mean optimality-gaps)

     :mean-runtime-ms
     (mean runtimes)

     :total-runtime-ms
     (reduce + runtimes)

     :mean-best-found-fraction
     (mean best-found-fractions)

     :mean-raw-feasibility-rate
     (mean
      (map :raw-feasibility-rate
           instance-results))

     :mean-selected-asset-count
     (mean
      (map :selected-asset-count
           instance-results))

     :all-final-solutions-feasible?
     (every? :feasible?
             instance-results)}))
