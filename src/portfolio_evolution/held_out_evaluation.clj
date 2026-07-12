(ns portfolio-evolution.held-out-evaluation
  (:require
   [portfolio-evolution.baseline-ga :as baseline-ga]
   [portfolio-evolution.knapsack :as knapsack]
   [portfolio-evolution.metrics :as metrics]
   [portfolio-evolution.optimizer-runtime :as runtime]
   [portfolio-evolution.rng :as rng]
   [portfolio-evolution.synthetic-data :as synthetic-data]))

(def default-config
  {:test-instance-count 10

   ;; The baseline and evolved optimizer receive exactly the same budget.
   :evaluation-budget 150

   :dataset-config {}
   :baseline-config {}})

(defn- validate-config!
  [{:keys [test-instance-count
           evaluation-budget]
    :as config}]

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
  [method optimizer-result optimal-objective]
  (let [score-result
        (metrics/score
         (:best-objective optimizer-result)
         optimal-objective)

        evaluation-budget
        (:fitness-evaluations-used
         optimizer-result)]

    {:method
     method

     :best-objective
     (:best-objective
      optimizer-result)

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
      optimizer-result)

     :selected-asset-count
     (:selected-asset-count
      optimizer-result)

     :feasible?
     (:feasible?
      optimizer-result)

     :fitness-evaluations-used
     evaluation-budget

     :best-found-at-evaluation
     (:best-found-at-evaluation
      optimizer-result)

     :best-found-fraction
     (/ (double
         (:best-found-at-evaluation
          optimizer-result))
        (double
         evaluation-budget))

     :raw-feasibility-rate
     (:raw-feasibility-rate
      optimizer-result)

     :feasibility-rate
     (:feasibility-rate
      optimizer-result)

     :runtime-ms
     (:runtime-ms
      optimizer-result)}))

(defn- evaluate-instance
  [master-seed
   instance-index
   instance
   optimizer-program
   {:keys [evaluation-budget
           baseline-config]}]

  (let [instance-seed
        (rng/derive-seed
         master-seed
         (+ 20000
            instance-index))

        exact-result
        (knapsack/exact-solve
         instance)

        optimal-objective
        (:optimal-objective
         exact-result)

        baseline-run
        (baseline-ga/run
         instance
         instance-seed
         (merge
          baseline-config
          {:evaluation-budget
           evaluation-budget}))

        evolved-run
        (runtime/run
         instance
         optimizer-program
         instance-seed
         {:evaluation-budget
          evaluation-budget})

        baseline
        (method-result
         :baseline-ga
         baseline-run
         optimal-objective)

        evolved
        (method-result
         :evolved-dsl
         evolved-run
         optimal-objective)]

    (when-not (= evaluation-budget
                 (:fitness-evaluations-used
                  baseline)
                 (:fitness-evaluations-used
                  evolved))
      (throw
       (ex-info
        "Compared algorithms did not use the same evaluation budget."
        {:instance-id
         (:id instance)

         :required-budget
         evaluation-budget

         :baseline-budget
         (:fitness-evaluations-used
          baseline)

         :evolved-budget
         (:fitness-evaluations-used
          evolved)})))

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

     :baseline
     baseline

     :evolved
     evolved

     :normalized-score-difference
     (- (:normalized-score evolved)
        (:normalized-score baseline))

     ;; Positive means that the evolved optimizer has a smaller gap.
     :optimality-gap-improvement
     (- (:optimality-gap baseline)
        (:optimality-gap evolved))}))

(defn- paired-summary
  [instance-comparisons]
  (let [tolerance
        1.0e-12

        score-differences
        (mapv
         :normalized-score-difference
         instance-comparisons)

        gap-improvements
        (mapv
         :optimality-gap-improvement
         instance-comparisons)

        comparison-sign
        (fn [difference]
          (cond
            (> difference tolerance)
            :win

            (< difference
               (- tolerance))
            :loss

            :else
            :tie))

        outcomes
        (mapv
         (comp comparison-sign
               :normalized-score-difference)
         instance-comparisons)]

    {:mean-normalized-score-difference
     (metrics/mean
      score-differences)

     :minimum-normalized-score-difference
     (apply min
            score-differences)

     :maximum-normalized-score-difference
     (apply max
            score-differences)

     :mean-optimality-gap-improvement
     (metrics/mean
      gap-improvements)

     :evolved-wins
     (count
      (filter #{:win}
              outcomes))

     :ties
     (count
      (filter #{:tie}
              outcomes))

     :baseline-wins
     (count
      (filter #{:loss}
              outcomes))}))

(defn evaluate
  "Evaluates one frozen evolved optimizer against the fixed baseline GA.

  Both methods receive:
  - the same held-out test instances;
  - the same instance-level seeds;
  - the same fitness-evaluation budget.

  Training instances are not evaluated here."
  ([master-seed optimizer-program]
   (evaluate
    master-seed
    optimizer-program
    {}))

  ([master-seed optimizer-program config-overrides]
   (let [config
         (validate-config!
          (merge default-config
                 config-overrides))

         dataset
         (synthetic-data/generate-dataset
          (:dataset-config
           config))

         requested-count
         (:test-instance-count
          config)

         available-test-instances
         (:test dataset)]

     (when (> requested-count
              (count available-test-instances))
       (throw
        (ex-info
         "Not enough test instances are available."
         {:requested
          requested-count

          :available
          (count available-test-instances)})))

     (let [test-instances
           (vec
            (take requested-count
                  available-test-instances))

           instance-comparisons
           (mapv
            (fn [instance-index instance]
              (evaluate-instance
               master-seed
               instance-index
               instance
               optimizer-program
               config))
            (range)
            test-instances)

           baseline-results
           (mapv :baseline
                 instance-comparisons)

           evolved-results
           (mapv :evolved
                 instance-comparisons)]

       {:experiment
        :held-out-evolved-vs-baseline

        :master-seed
        master-seed

        :config
        config

        :data-config
        (:config dataset)

        :training-instances-used
        0

        :test-instance-count-used
        (count
         test-instances)

        :test-instance-ids
        (mapv :instance-id
              instance-comparisons)

        :evolved-program
        optimizer-program

        :instances
        instance-comparisons

        :baseline-summary
        (metrics/summarize-instance-results
         baseline-results)

        :evolved-summary
        (metrics/summarize-instance-results
         evolved-results)

        :paired-summary
        (paired-summary
         instance-comparisons)}))))
