(ns portfolio-evolution.scale-evaluation
  (:require
   [portfolio-evolution.baseline-ga :as baseline-ga]
   [portfolio-evolution.dsl :as dsl]
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.metrics :as metrics]
   [portfolio-evolution.optimizer-runtime :as runtime]
   [portfolio-evolution.random-search :as random-search]
   [portfolio-evolution.rng :as rng]
   [portfolio-evolution.synthetic-data :as synthetic-data]))

(def default-config
  {:asset-counts
   [50 100 250 500]

   :test-instance-count
   10

   :evaluation-budget
   150

   :base-test-data-seed
   9898

   :dataset-config
   {}

   :random-search-config
   {:selection-probability 0.30}

   :baseline-ga-config
   {}})

(defn- validate-config!
  [{:keys [asset-counts
           test-instance-count
           evaluation-budget]
    :as config}]

  (when-not (and (sequential? asset-counts)
                 (seq asset-counts)
                 (every?
                  #(and (integer? %)
                        (pos? %))
                  asset-counts))
    (throw
     (ex-info
      "Asset counts must be a non-empty collection of positive integers."
      {:asset-counts
       asset-counts})))

  (when-not (= (count asset-counts)
               (count
                (distinct asset-counts)))
    (throw
     (ex-info
      "Asset counts must be unique."
      {:asset-counts
       asset-counts})))

  (when-not (and (integer? test-instance-count)
                 (pos? test-instance-count))
    (throw
     (ex-info
      "Test instance count must be positive."
      {:test-instance-count
       test-instance-count})))

  (when-not (and (integer? evaluation-budget)
                 (pos? evaluation-budget))
    (throw
     (ex-info
      "Evaluation budget must be positive."
      {:evaluation-budget
       evaluation-budget})))

  config)

(defn- method-result
  [method run-result optimal-objective]
  (let [score-result
        (metrics/score
         (:best-objective
          run-result)
         optimal-objective)

        evaluations-used
        (:fitness-evaluations-used
         run-result)]

    {:method
     method

     :best-objective
     (:best-objective
      run-result)

     :optimal-objective
     optimal-objective

     :normalized-score
     (:normalized-score
      score-result)

     :optimality-gap
     (:optimality-gap
      score-result)

     :best-cost
     (:best-cost
      run-result)

     :selected-asset-count
     (:selected-asset-count
      run-result)

     :feasible?
     (:feasible?
      run-result)

     :fitness-evaluations-used
     evaluations-used

     :best-found-at-evaluation
     (:best-found-at-evaluation
      run-result)

     :best-found-fraction
     (/ (double
         (:best-found-at-evaluation
          run-result))
        (double
         evaluations-used))

     :raw-feasibility-rate
     (:raw-feasibility-rate
      run-result)

     :feasibility-rate
     (:feasibility-rate
      run-result)

     :runtime-ms
     (:runtime-ms
      run-result)}))

(defn- validate-method-result!
  [instance-id
   evaluation-budget
   optimal-objective
   result]

  (when-not (= evaluation-budget
               (:fitness-evaluations-used
                result))
    (throw
     (ex-info
      "A compared method did not consume the required evaluation budget."
      {:instance-id
       instance-id

       :method
       (:method result)

       :required-budget
       evaluation-budget

       :actual-budget
       (:fitness-evaluations-used
        result)})))

  (when (> (:best-objective result)
           optimal-objective)
    (throw
     (ex-info
      "A heuristic exceeded the exact dynamic-programming optimum."
      {:instance-id
       instance-id

       :method
       (:method result)

       :objective
       (:best-objective result)

       :optimal-objective
       optimal-objective})))

  (when-not (:feasible?
             result)
    (throw
     (ex-info
      "A compared method returned an infeasible final solution."
      {:instance-id
       instance-id

       :method
       (:method result)})))

  result)

(defn- evaluate-instance
  [master-seed
   size-index
   instance-index
   instance
   optimizer-program
   {:keys [evaluation-budget
           random-search-config
           baseline-ga-config]}]

  (let [stream-id
        (+ (* 100000
              size-index)
           instance-index)

        instance-seed
        (rng/derive-seed
         master-seed
         stream-id)

        exact-result
        (knapsack/exact-solve
         instance)

        optimal-objective
        (:optimal-objective
         exact-result)

        random-run
        (random-search/run
         instance
         instance-seed
         (merge
          random-search-config
          {:evaluation-budget
           evaluation-budget}))

        ga-run
        (baseline-ga/run
         instance
         instance-seed
         (merge
          baseline-ga-config
          {:evaluation-budget
           evaluation-budget}))

        evolved-run
        (runtime/run
         instance
         optimizer-program
         instance-seed
         {:evaluation-budget
          evaluation-budget})

        random-result
        (method-result
         :random-search
         random-run
         optimal-objective)

        ga-result
        (method-result
         :baseline-ga
         ga-run
         optimal-objective)

        evolved-result
        (method-result
         :evolved-dsl
         evolved-run
         optimal-objective)]

    (doseq [result
            [random-result
             ga-result
             evolved-result]]

      (validate-method-result!
       (:id instance)
       evaluation-budget
       optimal-objective
       result))

    {:instance-index
     instance-index

     :instance-id
     (:id instance)

     :instance-seed
     instance-seed

     :asset-count
     (count
      (:assets instance))

     :budget
     (:budget instance)

     :optimal-objective
     optimal-objective

     :random
     random-result

     :ga
     ga-result

     :evolved
     evolved-result

     :evolved-minus-random
     (- (:normalized-score
         evolved-result)
        (:normalized-score
         random-result))

     :evolved-minus-ga
     (- (:normalized-score
         evolved-result)
        (:normalized-score
         ga-result))}))

(defn- method-summary
  [instance-results method-key]
  (metrics/summarize-instance-results
   (mapv method-key
         instance-results)))

(defn- paired-summary
  [instance-results
   better-method-key
   reference-method-key]

  (let [tolerance
        1.0e-12

        differences
        (mapv
         (fn [comparison]
           (- (get-in comparison
                      [better-method-key
                       :normalized-score])

              (get-in comparison
                      [reference-method-key
                       :normalized-score])))
         instance-results)

        outcomes
        (mapv
         (fn [difference]
           (cond
             (> difference tolerance)
             :win

             (< difference
                (- tolerance))
             :loss

             :else
             :tie))
         differences)]

    {:mean-score-difference
     (metrics/mean
      differences)

     :minimum-score-difference
     (apply min
            differences)

     :maximum-score-difference
     (apply max
            differences)

     :wins
     (count
      (filter #{:win}
              outcomes))

     :ties
     (count
      (filter #{:tie}
              outcomes))

     :losses
     (count
      (filter #{:loss}
              outcomes))}))

(defn- generate-size-suite
  [asset-count
   test-instance-count
   base-test-data-seed
   dataset-config]

  (let [data-seed
        (rng/derive-seed
         base-test-data-seed
         asset-count)

        config
        (-> synthetic-data/default-config
            (merge dataset-config)

            (assoc
             :asset-count
             asset-count

             :test-instance-count
             test-instance-count))

        family
        (keyword
         (str
          "test-"
          asset-count
          "-assets"))

        instances
        (synthetic-data/generate-instances
         data-seed
         family
         test-instance-count
         config)]

    {:asset-count
     asset-count

     :data-seed
     data-seed

     :instances
     instances}))

(defn- evaluate-size
  [master-seed
   size-index
   size-suite
   optimizer-program
   config]

  (let [started-at
        (System/nanoTime)

        instance-results
        (mapv
         (fn [instance-index instance]
           (evaluate-instance
            master-seed
            size-index
            instance-index
            instance
            optimizer-program
            config))
         (range)
         (:instances
          size-suite))

        elapsed-nanoseconds
        (- (System/nanoTime)
           started-at)]

    {:asset-count
     (:asset-count
      size-suite)

     :data-seed
     (:data-seed
      size-suite)

     :instance-count
     (count
      instance-results)

     :instances
     instance-results

     :random-summary
     (method-summary
      instance-results
      :random)

     :ga-summary
     (method-summary
      instance-results
      :ga)

     :evolved-summary
     (method-summary
      instance-results
      :evolved)

     :evolved-vs-random
     (paired-summary
      instance-results
      :evolved
      :random)

     :evolved-vs-ga
     (paired-summary
      instance-results
      :evolved
      :ga)

     :total-size-runtime-ms
     (/ (double
         elapsed-nanoseconds)
        1000000.0)}))

(defn evaluate
  "Evaluates one frozen evolved optimizer across multiple problem sizes.

  The optimizer is not changed or retrained between sizes.

  Random search, baseline GA and evolved DSL receive:
  - the same instances;
  - paired instance-level seeds;
  - the same objective-evaluation budget."
  ([master-seed optimizer-program]
   (evaluate
    master-seed
    optimizer-program
    {}))

  ([master-seed optimizer-program config-overrides]
   (dsl/validate!
    optimizer-program)

   (let [config
         (validate-config!
          (merge
           default-config
           config-overrides))

         {:keys [asset-counts
                 test-instance-count
                 base-test-data-seed
                 dataset-config]}
         config

         started-at
         (System/nanoTime)

         size-suites
         (mapv
          (fn [asset-count]
            (generate-size-suite
             asset-count
             test-instance-count
             base-test-data-seed
             dataset-config))
          asset-counts)

         size-results
         (mapv
          (fn [size-index size-suite]
            (evaluate-size
             master-seed
             size-index
             size-suite
             optimizer-program
             config))
          (range)
          size-suites)

         elapsed-nanoseconds
         (- (System/nanoTime)
            started-at)]

     {:experiment
      :scale-generalization

      :master-seed
      master-seed

      :config
      config

      :training-instances-used
      0

      :frozen-evolved-program
      optimizer-program

      :sizes
      size-results

      :total-runtime-ms
      (/ (double
          elapsed-nanoseconds)
         1000000.0)})))
