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
