(ns portfolio-evolution.optimizer-runtime
  (:require
   [portfolio-evolution.baseline-ga :as baseline-ga]
   [portfolio-evolution.dsl :as dsl]
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.rng :as rng]))

(def default-config
  {:evaluation-budget 3000})

(defn- operation-key
  [form]
  (keyword
   (name
    (first form))))

(defn- scorer-value
  [scorer asset]
  (cond
    (= scorer 'expected-profit)
    (double
     (:expected-profit asset))

    (= scorer 'return-per-cost)
    (/ (double
        (:expected-profit asset))
       (double
        (:cost asset)))

    (= scorer 'cost)
    (double
     (:cost asset))

    (seq? scorer)
    (let [[operator & arguments]
          scorer]

      (case (keyword
             (name operator))

        :+
        (+ (scorer-value
            (nth arguments 0)
            asset)
           (scorer-value
            (nth arguments 1)
            asset))

        :-
        (- (scorer-value
            (nth arguments 0)
            asset)
           (scorer-value
            (nth arguments 1)
            asset))

        :*
        (* (double
            (nth arguments 0))
           (scorer-value
            (nth arguments 1)
            asset))))

    :else
    (throw
     (ex-info
      "Unsupported asset scorer."
      {:scorer scorer}))))

(defn- solution-cost
  [assets solution]
  (reduce-kv
   (fn [total index gene]
     (if (= gene 1)
       (+ total
          (:cost
           (nth assets index)))
       total))
   0
   solution))

(defn- random-removal-repair
  [{:keys [assets budget]}
   random
   solution]

  (loop [current-solution
         (vec solution)

         current-cost
         (solution-cost
          assets
          solution)]

    (if (<= current-cost
            budget)

      current-solution

      (let [selected-indices
            (vec
             (keep-indexed
              (fn [index gene]
                (when (= gene 1)
                  index))
              current-solution))]

        (when (empty?
               selected-indices)
          (throw
           (ex-info
            "Unable to repair infeasible solution."
            {:solution solution
             :budget budget
             :current-cost current-cost})))

        (let [selected-position
              (rng/next-int
               random
               (count selected-indices))

              removed-index
              (nth selected-indices
                   selected-position)]

          (recur
           (assoc
            current-solution
            removed-index
            0)

           (- current-cost
              (:cost
               (nth assets
                    removed-index)))))))))

(defn- repair-solution
  [state solution]
  (case (:repair-strategy state)

    ratio-repair
    (baseline-ga/ratio-repair
     (:instance state)
     solution)

    random-removal-repair
    (random-removal-repair
     (:instance state)
     (:random state)
     solution)

    (throw
     (ex-info
      "Unsupported repair strategy."
      {:repair-strategy
       (:repair-strategy state)}))))

(defn- compare-individuals
  [individual-a individual-b]
  (let [evaluation-a
        (:evaluation individual-a)

        evaluation-b
        (:evaluation individual-b)

        objective-comparison
        (compare
         (:objective evaluation-b)
         (:objective evaluation-a))

        cost-comparison
        (compare
         (:cost evaluation-a)
         (:cost evaluation-b))]

    (cond
      (not (zero?
            objective-comparison))
      objective-comparison

      (not (zero?
            cost-comparison))
      cost-comparison

      :else
      (compare
       (:solution individual-a)
       (:solution individual-b)))))

(defn- better-individual?
  [candidate current]
  (neg?
   (compare-individuals
    candidate
    current)))

(defn- best-objective
  [state]
  (get-in state
          [:best
           :evaluation
           :objective]))

(defn- evaluate-one
  [state solution]
  (let [raw-evaluation
        (knapsack/evaluate-solution
         (:instance state)
         solution)

        repaired-solution
        (repair-solution
         state
         solution)

        repaired-evaluation
        (knapsack/evaluate-solution
         (:instance state)
         repaired-solution)

        _feasibility-check
        (when-not (:feasible?
                   repaired-evaluation)
          (throw
           (ex-info
            "DSL repair produced an infeasible solution."
            {:solution solution
             :repaired-solution repaired-solution
             :evaluation repaired-evaluation})))

        evaluation-number
        (inc
         (:evaluations-used state))

        individual
        {:solution
         repaired-solution

         :evaluation
         repaired-evaluation

         :raw-feasible?
         (:feasible?
          raw-evaluation)}

        current-best
        (:best state)

        current-objective
        (best-objective state)

        candidate-objective
        (:objective
         repaired-evaluation)

        strict-improvement?
        (or (nil? current-best)
            (> candidate-objective
               current-objective))

        updated-best
        (cond
          strict-improvement?
          individual

          (better-individual?
           individual
           current-best)
          individual

          :else
          current-best)

        updated-best-found-at
        (if strict-improvement?
          evaluation-number
          (:best-found-at-evaluation
           state))]

    [(-> state
         (assoc
          :best
          updated-best

          :best-found-at-evaluation
          updated-best-found-at)

         (update
          :evaluations-used
          inc)

         (update
          :raw-feasible-count
          +
          (if (:feasible?
               raw-evaluation)
            1
            0))

         (update
          :feasible-count
          inc))

     individual

     strict-improvement?]))

(defn- remaining-evaluations
  [state]
  (- (:evaluation-budget state)
     (:evaluations-used state)))

(defn- evaluate-batch
  [state action solutions]
  (let [initial-evaluation-count
        (:evaluations-used state)]

    (loop [remaining
           (seq solutions)

           current-state
           state

           individuals
           []

           strict-improvement?
           false]

      (if (or (nil? remaining)
              (zero?
               (remaining-evaluations
                current-state)))

        (let [evaluated-count
              (- (:evaluations-used
                  current-state)
                 initial-evaluation-count)]

          (if (zero?
               evaluated-count)

            [current-state
             individuals]

            (let [next-state
                  (-> current-state
                      (assoc
                       :stagnation-steps
                       (if strict-improvement?
                         0
                         (inc
                          (:stagnation-steps
                           current-state))))

                      (update
                       :convergence
                       conj
                       {:action
                        action

                        :evaluations-used
                        (:evaluations-used
                         current-state)

                        :best-objective
                        (best-objective
                         current-state)

                        :best-found-at-evaluation
                        (:best-found-at-evaluation
                         current-state)

                        :stagnation-steps
                        (if strict-improvement?
                          0
                          (inc
                           (:stagnation-steps
                            current-state))) }))]

              [next-state
               individuals])))

        (let [[next-state
               individual
               improved?]
              (evaluate-one
               current-state
               (first remaining))]

          (recur
           (next remaining)
           next-state
           (conj individuals
                 individual)
           (or strict-improvement?
               improved?)))))))

(defn- survivors
  [individuals limit]
  (vec
   (take limit
         (sort
          compare-individuals
          individuals))))

(defn- random-solution
  [state]
  (let [asset-count
        (count
         (get-in state
                 [:instance
                  :assets]))]

    (mapv
     (fn [_]
       (if (rng/next-boolean
            (:random state))
         1
         0))
     (range asset-count))))

(defn- ordered-asset-indices
  [instance scorer]
  (let [assets
        (:assets instance)]

    (vec
     (sort-by
      (fn [index]
        [(- (scorer-value
             scorer
             (nth assets index)))
         index])
      (range
       (count assets))))))

(defn- greedy-solution
  [{:keys [assets budget]
    :as instance}
   scorer]

  (:solution
   (reduce
    (fn [{:keys [solution cost]
          :as state}
         asset-index]

      (let [asset-cost
            (:cost
             (nth assets
                  asset-index))]

        (if (<= (+ cost
                   asset-cost)
                budget)

          {:solution
           (assoc
            solution
            asset-index
            1)

           :cost
           (+ cost
              asset-cost)}

          state)))

    {:solution
     (vec
      (repeat
       (count assets)
       0))

     :cost
     0}

    (ordered-asset-indices
     instance
     scorer))))

(defn- initializer-size
  [initializer]
  (let [[operator & arguments]
        initializer]

    (case (keyword
           (name operator))

      :random-population
      (first arguments)

      :mixed-population
      (+ (nth arguments 0)
         (nth arguments 1)))))

(defn- initialize-solutions
  [state initializer]
  (let [[operator & arguments]
        initializer]

    (case (keyword
           (name operator))

      :random-population
      (let [population-size
            (first arguments)]

        (mapv
         (fn [_]
           (random-solution
            state))
         (range population-size)))

      :mixed-population
      (let [random-count
            (nth arguments 0)

            greedy-count
            (nth arguments 1)

            scorer
            (nth arguments 2)

            random-solutions
            (mapv
             (fn [_]
               (random-solution
                state))
             (range random-count))

            greedy
            (greedy-solution
             (:instance state)
             scorer)

            greedy-solutions
            (vec
             (repeat
              greedy-count
              greedy))]

        (into
         random-solutions
         greedy-solutions)))))

(defn- selected-indices
  [solution]
  (vec
   (keep-indexed
    (fn [index gene]
      (when (= gene 1)
        index))
    solution)))

(defn- unselected-indices
  [solution]
  (vec
   (keep-indexed
    (fn [index gene]
      (when (= gene 0)
        index))
    solution)))

(defn- best-fitting-unselected
  [instance solution scorer excluded-index]
  (let [{:keys [assets budget]}
        instance

        current-cost
        (solution-cost
         assets
         solution)]

    (some
     (fn [asset-index]
       (when (and
              (not=
               asset-index
               excluded-index)

              (<= (+ current-cost
                     (:cost
                      (nth assets
                           asset-index)))
                  budget))
         asset-index))

     (->> (unselected-indices
           solution)

          (sort-by
           (fn [asset-index]
             [(- (scorer-value
                  scorer
                  (nth assets
                       asset-index)))
              asset-index]))))))

(defn- apply-portfolio-operation
  [state operation solution]
  (let [[operator & arguments]
        operation

        operation-type
        (keyword
         (name operator))

        instance
        (:instance state)

        assets
        (:assets instance)]

    (case operation-type

      :swap-assets
      (let [selected
            (selected-indices
             solution)

            unselected
            (unselected-indices
             solution)]

        (if (or (empty? selected)
                (empty? unselected))

          solution

          (let [removed-index
                (nth
                 selected
                 (rng/next-int
                  (:random state)
                  (count selected)))

                added-index
                (nth
                 unselected
                 (rng/next-int
                  (:random state)
                  (count unselected)))]

            (-> solution
                (assoc
                 removed-index
                 0)
                (assoc
                 added-index
                 1)))))

      :remove-worst
      (let [scorer
            (first arguments)

            selected
            (selected-indices
             solution)]

        (if (empty? selected)
          solution

          (let [worst-index
                (first
                 (sort-by
                  (fn [asset-index]
                    [(scorer-value
                      scorer
                      (nth assets
                           asset-index))
                     asset-index])
                  selected))]

            (assoc
             solution
             worst-index
             0))))

      :add-best
      (let [scorer
            (first arguments)

            added-index
            (best-fitting-unselected
             instance
             solution
             scorer
             nil)]

        (if (nil? added-index)
          solution
          (assoc
           solution
           added-index
           1)))

      :drop-add
      (let [scorer
            (first arguments)

            selected
            (selected-indices
             solution)]

        (if (empty? selected)
          (apply-portfolio-operation
           state
           (list
            'add-best
            scorer)
           solution)

          (let [removed-index
                (first
                 (sort-by
                  (fn [asset-index]
                    [(scorer-value
                      scorer
                      (nth assets
                           asset-index))
                     asset-index])
                  selected))

                after-removal
                (assoc
                 solution
                 removed-index
                 0)

                added-index
                (best-fitting-unselected
                 instance
                 after-removal
                 scorer
                 removed-index)]

            (if (nil? added-index)
              after-removal

              (assoc
               after-removal
               added-index
               1))))))))

(defn- population-diversity
  [population]
  (if (empty? population)
    0.0

    (/ (double
        (count
         (distinct
          (map :solution
               population))))
       (double
        (count population)))))

(defn- average-budget-utilization
  [state]
  (let [population
        (:population state)

        budget
        (double
         (get-in state
                 [:instance
                  :budget]))]

    (if (or (empty? population)
            (zero? budget))
      0.0

      (/ (reduce
          +
          (map
           (fn [individual]
             (/ (double
                 (get-in individual
                         [:evaluation
                          :cost]))
                budget))
           population))

         (double
          (count population))))))

(defn- evaluate-predicate
  [state predicate-form]
  (let [[operator & arguments]
        predicate-form]

    (case (keyword
           (name operator))

      :stagnant-for?
      (>= (:stagnation-steps
           state)
          (first arguments))

      :diversity-below?
      (< (population-diversity
          (:population state))
         (double
          (first arguments)))

      :budget-utilization-below?
      (< (average-budget-utilization
          state)
         (double
          (first arguments))))))

(declare execute-program)

(defn- execute-sequence
  [state programs]
  (reduce
   (fn [current-state program]
     (execute-program
      current-state
      program))
   state
   programs))

(defn- execute-inject-random
  [state count-to-inject]
  (let [solutions
        (mapv
         (fn [_]
           (random-solution
            state))
         (range count-to-inject))

        [next-state individuals]
        (evaluate-batch
         state
         :inject-random
         solutions)]

    (assoc
     next-state
     :population
     (survivors
      (concat
       (:population state)
       individuals)
      (:population-limit state)))))

(defn- execute-transform
  [state operation]
  (let [solutions
        (mapv
         (fn [individual]
           (apply-portfolio-operation
            state
            operation
            (:solution individual)))
         (:population state))

        [next-state individuals]
        (evaluate-batch
         state
         :transform
         solutions)]

    (assoc
     next-state
     :population
     (survivors
      (concat
       (:population state)
       individuals)
      (:population-limit state)))))

(defn- execute-local-search
  [state operation]
  (let [parents
        (:population state)

        solutions
        (mapv
         (fn [individual]
           (apply-portfolio-operation
            state
            operation
            (:solution individual)))
         parents)

        [next-state candidates]
        (evaluate-batch
         state
         :local-search
         solutions)

        evaluated-count
        (count candidates)

        processed-parents
        (take evaluated-count
              parents)

        untouched-parents
        (drop evaluated-count
              parents)

        winners
        (mapv
         (fn [parent candidate]
           (if (better-individual?
                candidate
                parent)
             candidate
             parent))
         processed-parents
         candidates)]

    (assoc
     next-state
     :population
     (survivors
      (concat
       winners
       untouched-parents)
      (:population-limit state)))))

(defn- execute-program
  [state program]
  (if (zero?
       (remaining-evaluations
        state))

    state

    (let [[operator & arguments]
          program

          operation
          (operation-key
           program)]

      (case operation

        :sequence
        (execute-sequence
         state
         arguments)

        :repeat-until-budget
        (let [body
              (first arguments)]

          (loop [current-state
                 state]

            (if (zero?
                 (remaining-evaluations
                  current-state))

              current-state

              (let [before
                    (:evaluations-used
                     current-state)

                    next-state
                    (execute-program
                     current-state
                     body)

                    after
                    (:evaluations-used
                     next-state)]

                (when (= before
                         after)
                  (throw
                   (ex-info
                    "repeat-until-budget body did not consume any evaluations."
                    {:program body
                     :evaluations-used before})))

                (recur
                 next-state)))))

        :if
        (let [[predicate-form
               then-program
               else-program]
              arguments]

          (execute-program
           state
           (if (evaluate-predicate
                state
                predicate-form)
             then-program
             else-program)))

        :select-best
        (assoc
         state
         :population
         (survivors
          (:population state)
          (first arguments)))

        :inject-random
        (execute-inject-random
         state
         (first arguments))

        :transform
        (execute-transform
         state
         (first arguments))

        :local-search
        (execute-local-search
         state
         (first arguments))

        :no-op
        state))))

(defn- run-search
  [instance optimizer-form seed config]
  (dsl/validate!
   optimizer-form)

  (knapsack/validate-instance!
   instance)

  (let [{:keys [evaluation-budget]}
        config

        _budget-validation
        (when-not (and (integer?
                        evaluation-budget)
                       (pos?
                        evaluation-budget))
          (throw
           (ex-info
            "Evaluation budget must be a positive integer."
            {:evaluation-budget
             evaluation-budget})))

        [_ initializer repair-strategy program]
        optimizer-form

        population-limit
        (initializer-size
         initializer)

        _initializer-budget-validation
        (when (> population-limit
                 evaluation-budget)
          (throw
           (ex-info
            "Initializer population exceeds evaluation budget."
            {:population-size
             population-limit

             :evaluation-budget
             evaluation-budget})))

        initial-state
        {:instance
         instance

         :random
         (rng/create seed)

         :repair-strategy
         repair-strategy

         :evaluation-budget
         evaluation-budget

         :evaluations-used
         0

         :population-limit
         population-limit

         :population
         []

         :best
         nil

         :best-found-at-evaluation
         nil

         :raw-feasible-count
         0

         :feasible-count
         0

         :stagnation-steps
         0

         :convergence
         []}

        initial-solutions
        (initialize-solutions
         initial-state
         initializer)

        [evaluated-state
         initial-population]
        (evaluate-batch
         initial-state
         :initialize
         initial-solutions)

        initialized-state
        (assoc
         evaluated-state
         :population
         (survivors
          initial-population
          population-limit))

        final-state
        (execute-program
         initialized-state
         program)

        best
        (:best final-state)

        best-evaluation
        (:evaluation best)

        evaluations-used
        (:evaluations-used
         final-state)]

    {:algorithm
     :typed-s-expression-optimizer

     :instance-id
     (:id instance)

     :seed
     seed

     :program
     optimizer-form

     :program-node-count
     (dsl/program-node-count
      optimizer-form)

     :program-depth
     (dsl/program-depth
      optimizer-form)

     :config
     config

     :best-solution
     (:solution best)

     :best-objective
     (:objective
      best-evaluation)

     :best-cost
     (:cost
      best-evaluation)

     :selected-asset-count
     (:selected-asset-count
      best-evaluation)

     :feasible?
     (:feasible?
      best-evaluation)

     :fitness-evaluations-used
     evaluations-used

     :best-found-at-evaluation
     (:best-found-at-evaluation
      final-state)

     :program-steps
     (count
      (:convergence
       final-state))

     :raw-feasibility-rate
     (/ (double
         (:raw-feasible-count
          final-state))
        (double
         evaluations-used))

     :feasibility-rate
     (/ (double
         (:feasible-count
          final-state))
        (double
         evaluations-used))

     :final-population-size
     (count
      (:population
       final-state))

     :convergence
     (:convergence
      final-state)}))

(defn run
  "Validates and executes one typed S-expression optimizer."
  ([instance optimizer-form seed]
   (run
    instance
    optimizer-form
    seed
    {}))

  ([instance optimizer-form seed config-overrides]
   (let [config
         (merge
          default-config
          config-overrides)

         started-at
         (System/nanoTime)

         result
         (run-search
          instance
          optimizer-form
          seed
          config)

         elapsed-nanoseconds
         (- (System/nanoTime)
            started-at)]

     (assoc
      result
      :runtime-ms
      (/ (double
          elapsed-nanoseconds)
         1000000.0)))))
