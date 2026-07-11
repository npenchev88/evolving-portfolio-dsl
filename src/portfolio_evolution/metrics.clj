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
