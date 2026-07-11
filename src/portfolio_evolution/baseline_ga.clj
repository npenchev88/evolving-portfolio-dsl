(ns portfolio-evolution.baseline-ga
  (:require
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.rng :as rng]))

(def default-config
  {:population-size 50
   :tournament-size 2
   :crossover-probability 0.7
   :mutation-probability nil
   :elite-count 1
   :evaluation-budget 3000})

(defn- valid-probability?
  [value]
  (and (number? value)
       (<= 0.0 value 1.0)))

(defn- validate-config!
  [asset-count config]
  (when-not (pos? asset-count)
    (throw
     (ex-info "The instance must contain at least one asset."
              {:asset-count asset-count})))

  (let [{:keys [population-size
                tournament-size
                crossover-probability
                mutation-probability
                elite-count
                evaluation-budget]
         :as resolved-config}
        (assoc config
               :mutation-probability
               (or (:mutation-probability config)
                   (/ 1.0 (double asset-count))))]

    (when-not (and (integer? population-size)
                   (> population-size 1))
      (throw
       (ex-info "Population size must be greater than one."
                {:population-size population-size})))

    (when-not (and (integer? tournament-size)
                   (pos? tournament-size))
      (throw
       (ex-info "Tournament size must be positive."
                {:tournament-size tournament-size})))

    (when-not (valid-probability? crossover-probability)
      (throw
       (ex-info "Invalid crossover probability."
                {:crossover-probability
                 crossover-probability})))

    (when-not (valid-probability? mutation-probability)
      (throw
       (ex-info "Invalid mutation probability."
                {:mutation-probability
                 mutation-probability})))

    (when-not (and (integer? elite-count)
                   (<= 0 elite-count)
                   (< elite-count population-size))
      (throw
       (ex-info
        "Elite count must be non-negative and smaller than population size."
        {:elite-count elite-count
         :population-size population-size})))

    (when-not (and (integer? evaluation-budget)
                   (>= evaluation-budget population-size))
      (throw
       (ex-info
        "Evaluation budget must be at least the population size."
        {:evaluation-budget evaluation-budget
         :population-size population-size})))

    resolved-config))

(defn- asset-ratio
  [{:keys [cost expected-profit]}]
  (/ (double expected-profit)
     (double cost)))

(defn ratio-repair
  "Repairs an infeasible portfolio by removing selected assets with the
  lowest expected-profit / cost ratio.

  Ties are resolved by asset index, making the operation deterministic."
  [{:keys [assets budget] :as instance} solution]
  (knapsack/validate-instance! instance)

  (when-not (= (count assets)
               (count solution))
    (throw
     (ex-info "Solution length does not match the number of assets."
              {:asset-count (count assets)
               :solution-length (count solution)})))

  (let [normalized-solution
        (mapv
         #(if (knapsack/selected-gene? %) 1 0)
         solution)

        initial-cost
        (reduce-kv
         (fn [total index gene]
           (if (= gene 1)
             (+ total
                (:cost (nth assets index)))
             total))
         0
         normalized-solution)

        removal-order
        (->> normalized-solution
             (keep-indexed
              (fn [index gene]
                (when (= gene 1)
                  index)))
             (sort-by
              (fn [index]
                [(asset-ratio (nth assets index))
                 index]))
             vec)]

    (loop [repaired normalized-solution
           current-cost initial-cost
           remaining removal-order]

      (if (<= current-cost budget)
        repaired

        (let [asset-index (first remaining)]
          (when (nil? asset-index)
            (throw
             (ex-info "Unable to repair solution."
                      {:solution solution
                       :current-cost current-cost
                       :budget budget})))

          (recur
           (assoc repaired asset-index 0)
           (- current-cost
              (:cost (nth assets asset-index)))
           (rest remaining)))))))

(defn- prepare-individual
  [instance solution]
  (let [raw-evaluation
        (knapsack/evaluate-solution instance solution)

        repaired-solution
        (ratio-repair instance solution)

        repaired-evaluation
        (knapsack/evaluate-solution
         instance
         repaired-solution)]

    (when-not (:feasible? repaired-evaluation)
      (throw
       (ex-info "Repair produced an infeasible individual."
                {:solution solution
                 :repaired-solution repaired-solution
                 :evaluation repaired-evaluation})))

    {:solution repaired-solution
     :evaluation repaired-evaluation
     :raw-feasible? (:feasible? raw-evaluation)}))

(defn- random-solution
  [random asset-count]
  (mapv
   (fn [_]
     (if (rng/next-boolean random)
       1
       0))
   (range asset-count)))

(defn- compare-individuals
  "Comparator where better individuals are ordered first."
  [individual-a individual-b]
  (let [evaluation-a (:evaluation individual-a)
        evaluation-b (:evaluation individual-b)

        objective-comparison
        (compare
         (:objective evaluation-b)
         (:objective evaluation-a))

        cost-comparison
        (compare
         (:cost evaluation-a)
         (:cost evaluation-b))]

    (cond
      (not (zero? objective-comparison))
      objective-comparison

      (not (zero? cost-comparison))
      cost-comparison

      :else
      (compare
       (:solution individual-a)
       (:solution individual-b)))))

(defn- better-individual?
  [candidate current]
  (neg?
   (compare-individuals candidate current)))

(defn- best-individual
  [population]
  (first
   (sort compare-individuals population)))

(defn- select-elites
  [population elite-count]
  (vec
   (take elite-count
         (sort compare-individuals population))))

(defn- tournament-select
  [random population tournament-size]
  (loop [remaining tournament-size
         winner nil]

    (if (zero? remaining)
      winner

      (let [candidate
            (nth population
                 (rng/next-int
                  random
                  (count population)))

            next-winner
            (if (or (nil? winner)
                    (better-individual?
                     candidate
                     winner))
              candidate
              winner)]

        (recur
         (dec remaining)
         next-winner)))))

(defn- one-point-crossover
  [random probability parent-a parent-b]
  (let [asset-count (count parent-a)]

    (if (and (> asset-count 1)
             (rng/chance? random probability))

      (let [crossover-point
            (rng/next-int-inclusive
             random
             1
             (dec asset-count))]

        [(into
          (subvec parent-a 0 crossover-point)
          (subvec parent-b crossover-point))

         (into
          (subvec parent-b 0 crossover-point)
          (subvec parent-a crossover-point))])

      [parent-a parent-b])))

(defn- mutate
  [random mutation-probability solution]
  (mapv
   (fn [gene]
     (if (rng/chance?
          random
          mutation-probability)
       (- 1 gene)
       gene))
   solution))

(defn- produce-offspring
  [random
   instance
   population
   offspring-count
   {:keys [tournament-size
           crossover-probability
           mutation-probability]}]

  (loop [offspring []]

    (if (= (count offspring)
           offspring-count)
      offspring

      (let [parent-a
            (tournament-select
             random
             population
             tournament-size)

            parent-b
            (tournament-select
             random
             population
             tournament-size)

            child-solutions
            (one-point-crossover
             random
             crossover-probability
             (:solution parent-a)
             (:solution parent-b))

            remaining-count
            (- offspring-count
               (count offspring))

            required-solutions
            (take remaining-count
                  child-solutions)

            prepared-children
            (mapv
             (fn [solution]
               (prepare-individual
                instance
                (mutate
                 random
                 mutation-probability
                 solution)))
             required-solutions)]

        (recur
         (into offspring
               prepared-children))))))

(defn- scan-evaluations
  "Updates the best individual while preserving the evaluation number at
  which the final best objective value was first discovered."
  [current-best
   current-best-found-at
   evaluations-before
   individuals]

  (reduce-kv
   (fn [{:keys [best] :as state}
        index
        individual]

     (let [evaluation-number
           (+ evaluations-before
              index
              1)

           candidate-objective
           (get-in individual
                   [:evaluation :objective])

           current-objective
           (when best
             (get-in best
                     [:evaluation :objective]))]

       (cond
         (nil? best)
         {:best individual
          :best-found-at evaluation-number}

         (> candidate-objective
            current-objective)
         {:best individual
          :best-found-at evaluation-number}

         (better-individual?
          individual
          best)
         (assoc state :best individual)

         :else
         state)))

   {:best current-best
    :best-found-at current-best-found-at}

   individuals))

(defn- raw-feasible-count
  [individuals]
  (count
   (filter :raw-feasible?
           individuals)))

(defn- feasible-count
  [individuals]
  (count
   (filter
    #(get-in %
             [:evaluation :feasible?])
    individuals)))

(defn- run-search
  [instance seed config-overrides]
  (let [asset-count
        (count (:assets instance))

        config
        (validate-config!
         asset-count
         (merge default-config
                config-overrides))

        {:keys [population-size
                elite-count
                evaluation-budget]}
        config

        offspring-per-generation
        (- population-size elite-count)

        random
        (rng/create seed)

        initial-population
        (mapv
         (fn [_]
           (prepare-individual
            instance
            (random-solution
             random
             asset-count)))
         (range population-size))

        initial-scan
        (scan-evaluations
         nil
         nil
         0
         initial-population)

        initial-best
        (:best initial-scan)

        initial-history
        [{:generation-step 0
          :evaluations-used population-size
          :best-objective
          (get-in initial-best
                  [:evaluation :objective])
          :best-found-at-evaluation
          (:best-found-at initial-scan)}]]

    (loop [population initial-population
           evaluations-used population-size
           generation-step 0
           best initial-best
           best-found-at
           (:best-found-at initial-scan)
           raw-feasible-total
           (raw-feasible-count
            initial-population)
           feasible-total
           (feasible-count
            initial-population)
           convergence initial-history]

      (if (= evaluations-used
             evaluation-budget)

        (let [best-evaluation
              (:evaluation best)]

          {:algorithm :baseline-ga
           :instance-id (:id instance)
           :seed seed
           :config config

           :best-solution
           (:solution best)

           :best-objective
           (:objective best-evaluation)

           :best-cost
           (:cost best-evaluation)

           :selected-asset-count
           (:selected-asset-count
            best-evaluation)

           :feasible?
           (:feasible? best-evaluation)

           :fitness-evaluations-used
           evaluations-used

           :best-found-at-evaluation
           best-found-at

           :generation-steps
           generation-step

           :raw-feasibility-rate
           (/ (double raw-feasible-total)
              (double evaluations-used))

           :feasibility-rate
           (/ (double feasible-total)
              (double evaluations-used))

           :convergence convergence})

        (let [remaining-budget
              (- evaluation-budget
                 evaluations-used)

              current-offspring-count
              (min offspring-per-generation
                   remaining-budget)

              offspring
              (produce-offspring
               random
               instance
               population
               current-offspring-count
               config)

              scan
              (scan-evaluations
               best
               best-found-at
               evaluations-used
               offspring)

              next-best
              (:best scan)

              next-evaluations-used
              (+ evaluations-used
                 current-offspring-count)

              complete-generation?
              (= current-offspring-count
                 offspring-per-generation)

              next-population
              (if complete-generation?
                (into
                 (select-elites
                  population
                  elite-count)
                 offspring)
                population)

              next-generation-step
              (inc generation-step)

              history-entry
              {:generation-step
               next-generation-step

               :complete-generation?
               complete-generation?

               :evaluations-used
               next-evaluations-used

               :best-objective
               (get-in next-best
                       [:evaluation :objective])

               :best-found-at-evaluation
               (:best-found-at scan)}]

          (recur
           next-population
           next-evaluations-used
           next-generation-step
           next-best
           (:best-found-at scan)
           (+ raw-feasible-total
              (raw-feasible-count offspring))
           (+ feasible-total
              (feasible-count offspring))
           (conj convergence
                 history-entry)))))))

(defn run
  "Runs the fixed human-written baseline GA."
  ([instance seed]
   (run instance seed {}))

  ([instance seed config-overrides]
   (let [started-at
         (System/nanoTime)

         result
         (run-search
          instance
          seed
          config-overrides)

         elapsed-nanoseconds
         (- (System/nanoTime)
            started-at)]

     (assoc result
            :runtime-ms
            (/ (double elapsed-nanoseconds)
               1000000.0)))))
