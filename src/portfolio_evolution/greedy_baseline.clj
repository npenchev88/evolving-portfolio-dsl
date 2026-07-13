(ns portfolio-evolution.greedy-baseline
  (:require
   [portfolio-evolution.knapsack :as knapsack]))

(defn return-per-cost
  "Returns the standard value-to-cost ratio used by the DSL initializer."
  [asset]
  (/ (double (:expected-profit asset))
     (double (:cost asset))))

(defn ordered-asset-indices
  "Orders assets exactly as the DSL return-per-cost initializer does.

  Assets are sorted by descending expected-profit/cost ratio. Equal scores
  are resolved by the original asset index, making the baseline fully
  deterministic."
  [instance]
  (knapsack/validate-instance! instance)

  (let [assets
        (:assets instance)]

    (vec
     (sort-by
      (fn [index]
        [(- (return-per-cost
             (nth assets index)))
         index])
      (range
       (count assets))))))

(defn solve
  "Constructs one feasible 0/1 solution with a single greedy pass.

  This intentionally mirrors the greedy solution embedded in
  `(mixed-population ... return-per-cost)`. It does not use the exact
  dynamic-programming solution and is not guaranteed to be optimal."
  [{:keys [assets budget]
    :as instance}]
  (knapsack/validate-instance! instance)

  (:solution
   (reduce
    (fn [{:keys [solution cost]
          :as state}
         asset-index]

      (let [asset-cost
            (:cost
             (nth assets
                  asset-index))]

        (if (<= (+ cost asset-cost)
                budget)

          {:solution
           (assoc solution
                  asset-index
                  1)

           :cost
           (+ cost asset-cost)}

          state)))

    {:solution
     (vec
      (repeat
       (count assets)
       0))

     :cost
     0}

    (ordered-asset-indices
     instance))))

(defn run
  "Runs the deterministic return-per-cost greedy baseline once.

  The method constructs and evaluates one portfolio, so its objective-
  evaluation count is 1 rather than the 150 evaluations used by the
  stochastic deployment methods."
  [instance]
  (let [started-at
        (System/nanoTime)

        solution
        (solve instance)

        evaluation
        (knapsack/evaluate-solution
         instance
         solution)

        elapsed-nanoseconds
        (- (System/nanoTime)
           started-at)]

    (when-not (:feasible? evaluation)
      (throw
       (ex-info
        "Greedy construction produced an infeasible solution."
        {:instance-id (:id instance)
         :evaluation evaluation})))

    {:algorithm
     :return-per-cost-greedy

     :instance-id
     (:id instance)

     :best-solution
     (:solution evaluation)

     :best-objective
     (:objective evaluation)

     :best-cost
     (:cost evaluation)

     :selected-asset-count
     (:selected-asset-count evaluation)

     :feasible?
     (:feasible? evaluation)

     :fitness-evaluations-used
     1

     :best-found-at-evaluation
     1

     :raw-feasibility-rate
     1.0

     :feasibility-rate
     1.0

     :runtime-ms
     (/ (double elapsed-nanoseconds)
        1000000.0)}))
