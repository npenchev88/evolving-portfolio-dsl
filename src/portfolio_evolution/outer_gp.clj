(ns portfolio-evolution.outer-gp
  (:require
   [portfolio-evolution.dsl :as dsl]
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.metrics :as metrics]
   [portfolio-evolution.optimizer-runtime :as runtime]
   [portfolio-evolution.program-generation :as generation]
   [portfolio-evolution.rng :as rng]
   [portfolio-evolution.synthetic-data :as synthetic-data]))

(def default-config
  {:outer-population-size 20

   ;; For the first smoke test we use only the first five fixed
   ;; training instances.
   :training-instance-count 5

   ;; Each candidate optimizer receives the same inner evaluation budget
   ;; on every training instance.
   :inner-evaluation-budget 300

   ;; Fitness penalty per DSL node.
   :parsimony-penalty-coefficient 0.0005

   :dataset-config {}

   :generator-config
   {:max-depth 8
    :population-size 50}})

(defn- validate-config!
  [{:keys [outer-population-size
           training-instance-count
           inner-evaluation-budget
           parsimony-penalty-coefficient]
    :as config}]

  (when-not (and (integer? outer-population-size)
                 (> outer-population-size 1))
    (throw
     (ex-info
      "Outer population size must be greater than one."
      {:outer-population-size
       outer-population-size})))

  (when-not (and (integer? training-instance-count)
                 (pos? training-instance-count))
    (throw
     (ex-info
      "Training instance count must be positive."
      {:training-instance-count
       training-instance-count})))

  (when-not (and (integer? inner-evaluation-budget)
                 (pos? inner-evaluation-budget))
    (throw
     (ex-info
      "Inner evaluation budget must be positive."
      {:inner-evaluation-budget
       inner-evaluation-budget})))

  (when-not (and (number?
                  parsimony-penalty-coefficient)
                 (not (neg?
                       parsimony-penalty-coefficient)))
    (throw
     (ex-info
      "Parsimony penalty coefficient must be non-negative."
      {:parsimony-penalty-coefficient
       parsimony-penalty-coefficient})))

  config)

(defn- prepare-training-suite
  [dataset training-instance-count]
  (let [available-training-instances
        (:train dataset)]

    (when (> training-instance-count
             (count available-training-instances))
      (throw
       (ex-info
        "Requested more training instances than the dataset contains."
        {:requested training-instance-count
         :available
         (count available-training-instances)})))

    (mapv
     (fn [instance-index instance]
       (let [exact-result
             (knapsack/exact-solve
              instance)]

         {:instance-index
          instance-index

          :instance
          instance

          :instance-id
          (:id instance)

          :optimal-objective
          (:optimal-objective
           exact-result)}))

     (range training-instance-count)
     (take training-instance-count
           available-training-instances))))

(defn- generate-unique-programs
  [random
   population-size
   generator-config]

  (let [maximum-attempts
        (* population-size 500)]

    (loop [programs []
           seen #{}
           attempts 0]

      (cond
        (= (count programs)
           population-size)
        programs

        (>= attempts
            maximum-attempts)
        (throw
         (ex-info
          "Could not generate enough unique optimizer programs."
          {:requested population-size
           :generated (count programs)
           :attempts attempts}))

        :else
        (let [program
              (generation/generate-optimizer
               random
               generator-config)]

          (if (contains? seen
                         program)

            (recur
             programs
             seen
             (inc attempts))

            (recur
             (conj programs
                   program)
             (conj seen
                   program)
             (inc attempts))))))))

(defn- evaluate-on-training-instance
  [optimizer-program
   training-case
   inner-seed
   inner-evaluation-budget]

  (let [{:keys [instance-index
                instance
                instance-id
                optimal-objective]}
        training-case

        optimizer-result
        (runtime/run
         instance
         optimizer-program
         inner-seed
         {:evaluation-budget
          inner-evaluation-budget})

        score-result
        (metrics/score
         (:best-objective
          optimizer-result)
         optimal-objective)

        normalized-score
        (:normalized-score
         score-result)]

    (when (> normalized-score
             (+ 1.0 1.0e-12))
      (throw
       (ex-info
        "Optimizer exceeded the exact dynamic-programming optimum."
        {:instance-id instance-id
         :optimizer-objective
         (:best-objective
          optimizer-result)
         :optimal-objective
         optimal-objective})))

    {:instance-index
     instance-index

     :instance-id
     instance-id

     :inner-seed
     inner-seed

     :best-objective
     (:best-objective
      optimizer-result)

     :optimal-objective
     optimal-objective

     :normalized-score
     normalized-score

     :optimality-gap
     (:optimality-gap
      score-result)

     :fitness-evaluations-used
     (:fitness-evaluations-used
      optimizer-result)

     :best-found-at-evaluation
     (:best-found-at-evaluation
      optimizer-result)

     :best-found-fraction
     (/ (double
         (:best-found-at-evaluation
          optimizer-result))
        (double
         (:fitness-evaluations-used
          optimizer-result)))

     :selected-asset-count
     (:selected-asset-count
      optimizer-result)

     :raw-feasibility-rate
     (:raw-feasibility-rate
      optimizer-result)

     :feasibility-rate
     (:feasibility-rate
      optimizer-result)

     :feasible?
     (:feasible?
      optimizer-result)

     :runtime-ms
     (:runtime-ms
      optimizer-result)}))

(defn- evaluate-candidate
  [candidate-id
   optimizer-program
   training-suite
   common-inner-seeds
   {:keys [inner-evaluation-budget
           parsimony-penalty-coefficient]}]

  (let [instance-results
        (mapv
         (fn [training-case inner-seed]
           (evaluate-on-training-instance
            optimizer-program
            training-case
            inner-seed
            inner-evaluation-budget))

         training-suite
         common-inner-seeds)

        normalized-scores
        (mapv :normalized-score
              instance-results)

        optimality-gaps
        (mapv :optimality-gap
              instance-results)

        runtimes
        (mapv :runtime-ms
              instance-results)

        node-count
        (dsl/program-node-count
         optimizer-program)

        depth
        (dsl/program-depth
         optimizer-program)

        mean-normalized-score
        (metrics/mean
         normalized-scores)

        parsimony-penalty
        (* (double
            parsimony-penalty-coefficient)
           (double
            node-count))

        fitness
        (- mean-normalized-score
           parsimony-penalty)]

    {:candidate-id
     candidate-id

     :program
     optimizer-program

     :program-node-count
     node-count

     :program-depth
     depth

     :mean-normalized-score
     mean-normalized-score

     :minimum-normalized-score
     (apply min
            normalized-scores)

     :maximum-normalized-score
     (apply max
            normalized-scores)

     :instance-score-standard-deviation
     (metrics/sample-standard-deviation
      normalized-scores)

     :mean-optimality-gap
     (metrics/mean
      optimality-gaps)

     :mean-runtime-ms
     (metrics/mean
      runtimes)

     :parsimony-penalty
     parsimony-penalty

     :fitness
     fitness

     :all-solutions-feasible?
     (every? :feasible?
             instance-results)

     :instance-results
     instance-results}))

(defn- candidate-sort-key
  [candidate]
  [(- (:fitness
       candidate))

   (- (:mean-normalized-score
       candidate))

   (:program-node-count
    candidate)

   (pr-str
    (:program candidate))])

(defn- rank-candidates
  [candidates]
  (mapv
   (fn [rank-index candidate]
     (assoc candidate
            :rank
            (inc rank-index)))

   (range)
   (sort-by
    candidate-sort-key
    candidates)))

(defn evaluate-initial-population
  "Generates and evaluates generation zero of the outer GP.

  Every optimizer is evaluated on the same fixed training instances,
  receives the same inner evaluation budget, and receives the same inner
  seed for a given training instance.

  Held-out test instances are generated but are never used here."
  ([master-seed]
   (evaluate-initial-population
    master-seed
    {}))

  ([master-seed config-overrides]
   (let [config
         (validate-config!
          (merge default-config
                 config-overrides))

         {:keys [outer-population-size
                 training-instance-count
                 generator-config]}
         config

         dataset
         (synthetic-data/generate-dataset
          (:dataset-config
           config))

         training-suite
         (prepare-training-suite
          dataset
          training-instance-count)

         program-generation-seed
         (rng/derive-seed
          master-seed
          900000)

         generator-random
         (rng/create
          program-generation-seed)

         common-inner-seeds
         (mapv
          (fn [instance-index]
            (rng/derive-seed
             master-seed
             (+ 10000
                instance-index)))
          (range training-instance-count))

         programs
         (generate-unique-programs
          generator-random
          outer-population-size
          generator-config)

         started-at
         (System/nanoTime)

         evaluated-candidates
         (mapv
          (fn [candidate-id program]
            (evaluate-candidate
             candidate-id
             program
             training-suite
             common-inner-seeds
             config))
          (range)
          programs)

         ranked-candidates
         (rank-candidates
          evaluated-candidates)

         elapsed-nanoseconds
         (- (System/nanoTime)
            started-at)

         best-candidate
         (first ranked-candidates)

         candidate-scores
         (mapv :mean-normalized-score
               ranked-candidates)]

     {:experiment
      :outer-gp-initial-population

      :generation
      0

      :master-seed
      master-seed

      :program-generation-seed
      program-generation-seed

      :config
      config

      :data-config
      (:config dataset)

      :training-instance-ids
      (mapv :instance-id
            training-suite)

      :training-instance-count-used
      training-instance-count

      :test-instance-count-generated
      (count (:test dataset))

      :test-instances-used
      0

      :common-inner-seeds
      common-inner-seeds

      :outer-runtime-ms
      (/ (double
          elapsed-nanoseconds)
         1000000.0)

      :candidates
      ranked-candidates

      :best-candidate
      best-candidate

      :summary
      {:candidate-count
       (count ranked-candidates)

       :unique-program-count
       (count
        (distinct
         (map :program
              ranked-candidates)))

       :best-fitness
       (:fitness
        best-candidate)

       :best-mean-normalized-score
       (:mean-normalized-score
        best-candidate)

       :best-minimum-normalized-score
       (:minimum-normalized-score
        best-candidate)

       :best-program-node-count
       (:program-node-count
        best-candidate)

       :best-program-depth
       (:program-depth
        best-candidate)

       :population-mean-normalized-score
       (metrics/mean
        candidate-scores)

       :population-score-standard-deviation
       (metrics/sample-standard-deviation
        candidate-scores)}})))

;; ---------------------------------------------------------------------------
;; Multi-generation outer genetic programming
;; ---------------------------------------------------------------------------

(def evolution-default-config
  {:outer-population-size 12
   :training-instance-count 3
   :inner-evaluation-budget 150

   :outer-generations 5
   :outer-tournament-size 3
   :elite-count 1

   :max-offspring-attempts 10000})

(defn- validate-evolution-config!
  [{:keys [outer-population-size
           outer-generations
           outer-tournament-size
           elite-count
           max-offspring-attempts]
    :as config}]

  (when-not (and (integer? outer-generations)
                 (not (neg? outer-generations)))
    (throw
     (ex-info
      "Outer generation count must be a non-negative integer."
      {:outer-generations outer-generations})))

  (when-not (and (integer? outer-tournament-size)
                 (pos? outer-tournament-size)
                 (<= outer-tournament-size
                     outer-population-size))
    (throw
     (ex-info
      "Outer tournament size must be positive and no larger than the population."
      {:outer-tournament-size outer-tournament-size
       :outer-population-size outer-population-size})))

  (when-not (and (integer? elite-count)
                 (pos? elite-count)
                 (< elite-count
                    outer-population-size))
    (throw
     (ex-info
      "Elite count must be positive and smaller than the outer population."
      {:elite-count elite-count
       :outer-population-size outer-population-size})))

  (when-not (and (integer? max-offspring-attempts)
                 (pos? max-offspring-attempts))
    (throw
     (ex-info
      "Maximum offspring attempts must be positive."
      {:max-offspring-attempts
       max-offspring-attempts})))

  config)

(defn- better-ranked-candidate?
  [candidate-a candidate-b]
  (neg?
   (compare
    (candidate-sort-key candidate-a)
    (candidate-sort-key candidate-b))))

(defn- tournament-select-candidate
  [random candidates tournament-size]
  (loop [remaining tournament-size
         winner nil]

    (if (zero? remaining)
      winner

      (let [candidate
            (nth candidates
                 (rng/next-int
                  random
                  (count candidates)))

            next-winner
            (if (or (nil? winner)
                    (better-ranked-candidate?
                     candidate
                     winner))
              candidate
              winner)]

        (recur
         (dec remaining)
         next-winner)))))

(defn- summarize-generation
  [generation candidates generation-runtime-ms]
  (let [best-candidate
        (first candidates)

        scores
        (mapv :mean-normalized-score
              candidates)]

    {:generation
     generation

     :candidate-count
     (count candidates)

     :unique-program-count
     (count
      (distinct
       (map :program
            candidates)))

     :best-candidate-id
     (:candidate-id
      best-candidate)

     :best-fitness
     (:fitness
      best-candidate)

     :best-mean-normalized-score
     (:mean-normalized-score
      best-candidate)

     :best-minimum-normalized-score
     (:minimum-normalized-score
      best-candidate)

     :best-mean-optimality-gap
     (:mean-optimality-gap
      best-candidate)

     :best-program-node-count
     (:program-node-count
      best-candidate)

     :best-program-depth
     (:program-depth
      best-candidate)

     :best-program
     (:program
      best-candidate)

     :population-mean-normalized-score
     (metrics/mean
      scores)

     :population-score-standard-deviation
     (metrics/sample-standard-deviation
      scores)

     :generation-runtime-ms
     generation-runtime-ms}))

(defn- evaluate-generation
  [generation
   candidate-specifications
   training-suite
   common-inner-seeds
   config]

  (let [started-at
        (System/nanoTime)

        evaluated-candidates
        (mapv
         (fn [candidate-id candidate-specification]
           (let [evaluated
                 (evaluate-candidate
                  candidate-id
                  (:program candidate-specification)
                  training-suite
                  common-inner-seeds
                  config)]

             (merge
              evaluated

              (dissoc
               candidate-specification
               :program)

              {:generation
               generation})))

         (range)
         candidate-specifications)

        ranked-candidates
        (rank-candidates
         evaluated-candidates)

        elapsed-nanoseconds
        (- (System/nanoTime)
           started-at)

        generation-runtime-ms
        (/ (double elapsed-nanoseconds)
           1000000.0)]

    {:generation
     generation

     :candidates
     ranked-candidates

     :summary
     (summarize-generation
      generation
      ranked-candidates
      generation-runtime-ms)}))

(defn- next-generation-specifications
  [random ranked-candidates config]
  (let [{:keys [outer-population-size
                outer-tournament-size
                elite-count
                max-offspring-attempts
                generator-config]}
        config

        elites
        (vec
         (take elite-count
               ranked-candidates))

        elite-specifications
        (mapv
         (fn [elite]
           {:program
            (:program elite)

            :origin
            :elite

            :parent-candidate-id
            (:candidate-id elite)

            :parent-rank
            (:rank elite)})

         elites)

        initial-seen
        (set
         (map :program
              elite-specifications))]

    (loop [specifications
           elite-specifications

           seen
           initial-seen

           attempts
           0]

      (cond
        (= (count specifications)
           outer-population-size)
        specifications

        (>= attempts
            max-offspring-attempts)
        (throw
         (ex-info
          "Could not create enough unique offspring programs."
          {:requested-population-size
           outer-population-size

           :generated
           (count specifications)

           :attempts
           attempts}))

        :else
        (let [parent
              (tournament-select-candidate
               random
               ranked-candidates
               outer-tournament-size)

              child-program
              (generation/mutate
               random
               (:program parent)
               generator-config)]

          (if (contains?
               seen
               child-program)

            (recur
             specifications
             seen
             (inc attempts))

            (recur
             (conj
              specifications
              {:program
               child-program

               :origin
               :mutation

               :parent-candidate-id
               (:candidate-id parent)

               :parent-rank
               (:rank parent)})

             (conj seen
                   child-program)

             (inc attempts))))))))

(defn evolve
  "Runs mutation-only outer genetic programming.

  Generation zero contains randomly generated optimizer programs.
  Subsequent generations use tournament selection, type-preserving subtree
  mutation and elitism.

  All candidates across all generations are evaluated on the same training
  instances, using the same common inner seeds and inner evaluation budget.

  Held-out test instances are never used."
  ([master-seed]
   (evolve
    master-seed
    {}))

  ([master-seed config-overrides]
   (let [started-at
         (System/nanoTime)

         config
         (-> (merge
              default-config
              evolution-default-config
              config-overrides)

             validate-config!
             validate-evolution-config!)

         {:keys [outer-generations
                 training-instance-count]}
         config

         initial-result
         (evaluate-initial-population
          master-seed
          config)

         dataset
         (synthetic-data/generate-dataset
          (:dataset-config
           config))

         training-suite
         (prepare-training-suite
          dataset
          training-instance-count)

         common-inner-seeds
         (:common-inner-seeds
          initial-result)

         outer-evolution-seed
         (rng/derive-seed
          master-seed
          910000)

         outer-random
         (rng/create
          outer-evolution-seed)

         initial-candidates
         (mapv
          (fn [candidate]
            (assoc
             candidate

             :generation
             0

             :origin
             :initial

             :parent-candidate-id
             nil

             :parent-rank
             nil))

          (:candidates
           initial-result))

         initial-generation
         {:generation
          0

          :candidates
          initial-candidates

          :summary
          (summarize-generation
           0
           initial-candidates
           (:outer-runtime-ms
            initial-result))}]

     (loop [current-generation
            0

            current-candidates
            initial-candidates

            generation-records
            [initial-generation]]

       (if (= current-generation
              outer-generations)

         (let [elapsed-nanoseconds
               (- (System/nanoTime)
                  started-at)

               final-candidates
               current-candidates

               best-candidate
               (first
                final-candidates)]

           {:experiment
            :outer-gp-evolution

            :master-seed
            master-seed

            :program-generation-seed
            (:program-generation-seed
             initial-result)

            :outer-evolution-seed
            outer-evolution-seed

            :config
            config

            :data-config
            (:config dataset)

            :training-instance-ids
            (mapv :instance-id
                  training-suite)

            :training-instance-count-used
            training-instance-count

            :test-instance-count-generated
            (count
             (:test dataset))

            :test-instances-used
            0

            :common-inner-seeds
            common-inner-seeds

            :generation-count
            outer-generations

            :generations
            generation-records

            :history
            (mapv :summary
                  generation-records)

            :final-candidates
            final-candidates

            :best-candidate
            best-candidate

            :outer-runtime-ms
            (/ (double elapsed-nanoseconds)
               1000000.0)})

         (let [next-generation
               (inc
                current-generation)

               candidate-specifications
               (next-generation-specifications
                outer-random
                current-candidates
                config)

               evaluated-generation
               (evaluate-generation
                next-generation
                candidate-specifications
                training-suite
                common-inner-seeds
                config)

               next-candidates
               (:candidates
                evaluated-generation)]

           (recur
            next-generation
            next-candidates
            (conj
             generation-records
             evaluated-generation))))))))
