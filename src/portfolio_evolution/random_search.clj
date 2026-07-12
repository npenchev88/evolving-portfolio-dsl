(ns portfolio-evolution.random-search
  (:require
   [portfolio-evolution.baseline-ga :as baseline-ga]
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.rng :as rng]))

(def default-config
  {:selection-probability 0.30
   :evaluation-budget 150})

(defn- validate-config!
  [{:keys [selection-probability
           evaluation-budget]
    :as config}]

  (when-not (and (number? selection-probability)
                 (<= 0.0
                     (double selection-probability)
                     1.0))
    (throw
     (ex-info
      "Selection probability must be between zero and one."
      {:selection-probability
       selection-probability})))

  (when-not (and (integer? evaluation-budget)
                 (pos? evaluation-budget))
    (throw
     (ex-info
      "Evaluation budget must be a positive integer."
      {:evaluation-budget
       evaluation-budget})))

  config)

(defn- random-solution
  [random asset-count selection-probability]
  (mapv
   (fn [_]
     (if (rng/chance?
          random
          selection-probability)
       1
       0))
   (range asset-count)))

(defn- compare-candidates
  "Orders better candidates first.

  Primary criterion:
  - larger objective

  Tie breakers:
  - lower cost
  - lexicographically smaller binary vector"
  [candidate-a candidate-b]
  (let [objective-comparison
        (compare
         (:objective candidate-b)
         (:objective candidate-a))

        cost-comparison
        (compare
         (:cost candidate-a)
         (:cost candidate-b))]

    (cond
      (not (zero?
            objective-comparison))
      objective-comparison

      (not (zero?
            cost-comparison))
      cost-comparison

      :else
      (compare
       (:solution candidate-a)
       (:solution candidate-b)))))

(defn- better-candidate?
  [candidate current-best]
  (or (nil? current-best)
      (neg?
       (compare-candidates
        candidate
        current-best))))

(defn- run-search
  [instance seed config]
  (knapsack/validate-instance!
   instance)

  (let [{:keys [selection-probability
                evaluation-budget]}
        config

        asset-count
        (count
         (:assets instance))

        random
        (rng/create seed)]

    (loop [evaluation-number
           1

           best
           nil

           best-found-at
           nil

           raw-feasible-count
           0]

      (if (> evaluation-number
             evaluation-budget)

        {:algorithm
         :random-portfolio-search

         :instance-id
         (:id instance)

         :seed
         seed

         :config
         config

         :best-solution
         (:solution best)

         :best-objective
         (:objective best)

         :best-cost
         (:cost best)

         :selected-asset-count
         (:selected-asset-count
          best)

         :feasible?
         (:feasible? best)

         :fitness-evaluations-used
         evaluation-budget

         :best-found-at-evaluation
         best-found-at

         :raw-feasibility-rate
         (/ (double raw-feasible-count)
            (double evaluation-budget))

         :feasibility-rate
         1.0}

        (let [raw-solution
              (random-solution
               random
               asset-count
               selection-probability)

              raw-evaluation
              (knapsack/evaluate-solution
               instance
               raw-solution)

              repaired-solution
              (baseline-ga/ratio-repair
               instance
               raw-solution)

              repaired-evaluation
              (knapsack/evaluate-solution
               instance
               repaired-solution)

              candidate
              {:solution
               repaired-solution

               :objective
               (:objective
                repaired-evaluation)

               :cost
               (:cost
                repaired-evaluation)

               :selected-asset-count
               (:selected-asset-count
                repaired-evaluation)

               :feasible?
               (:feasible?
                repaired-evaluation)}

              current-objective
              (when best
                (:objective best))

              strict-improvement?
              (or (nil? best)
                  (> (:objective candidate)
                     current-objective))

              next-best
              (if (better-candidate?
                   candidate
                   best)
                candidate
                best)

              next-best-found-at
              (if strict-improvement?
                evaluation-number
                best-found-at)]

          (when-not (:feasible?
                     repaired-evaluation)
            (throw
             (ex-info
              "Random-search repair produced an infeasible solution."
              {:instance-id
               (:id instance)

               :solution
               repaired-solution})))

          (recur
           (inc evaluation-number)
           next-best
           next-best-found-at
           (+ raw-feasible-count
              (if (:feasible?
                   raw-evaluation)
                1
                0))))))))

(defn run
  "Generates independent random portfolios without evolution.

  Every asset is initially selected with probability 0.30 by default.
  At N=100, the expected initial cardinality is therefore 30 assets."
  ([instance seed]
   (run
    instance
    seed
    {}))

  ([instance seed config-overrides]
   (let [config
         (validate-config!
          (merge
           default-config
           config-overrides))

         started-at
         (System/nanoTime)

         result
         (run-search
          instance
          seed
          config)

         elapsed-nanoseconds
         (- (System/nanoTime)
            started-at)]

     (assoc
      result
      :runtime-ms
      (/ (double elapsed-nanoseconds)
         1000000.0)))))
