(ns portfolio-evolution.program-generation
  (:require
   [portfolio-evolution.dsl :as dsl]
   [portfolio-evolution.rng :as rng]))

(def default-config
  {:max-depth 8

   ;; Population size remains fixed for fair comparisons.
   :population-size 50
   :greedy-counts [5 10 15 20]

   :selection-counts [10 20 25 30]
   :injection-counts [5 10 15 20]

   :stagnation-thresholds [2 4 6 8]
   :diversity-thresholds [0.10 0.20 0.30 0.50]
   :budget-utilization-thresholds [0.70 0.80 0.90]

   :scorer-scalars [0.1 0.2 0.5 1.0 2.0]

   :scorer-recursion-probability 0.40
   :program-recursion-probability 0.55

   :max-mutation-attempts 100})

(def scorer-terminals
  ['expected-profit
   'return-per-cost
   'cost])

(def repair-strategies
  ['ratio-repair
   'random-removal-repair])

(def evolvable-types
  #{:initializer
    :repair-strategy
    :program
    :predicate
    :portfolio-operation
    :asset-scorer})

(def minimum-expression-depth
  {:asset-scorer 1
   :repair-strategy 1
   :initializer 2
   :predicate 2
   :portfolio-operation 2
   :program 2
   :optimizer 4})

(defn- choose
  [random values]
  (let [values
        (vec values)]

    (when (empty? values)
      (throw
       (ex-info
        "Cannot choose from an empty collection."
        {})))

    (nth values
         (rng/next-int
          random
          (count values)))))

(defn- validate-config!
  [{:keys [max-depth
           population-size
           greedy-counts
           selection-counts
           injection-counts
           stagnation-thresholds
           diversity-thresholds
           budget-utilization-thresholds
           scorer-scalars
           scorer-recursion-probability
           program-recursion-probability
           max-mutation-attempts]
    :as config}]

  (when-not (and (integer? max-depth)
                 (>= max-depth 4))
    (throw
     (ex-info
      "Maximum optimizer depth must be at least four."
      {:max-depth max-depth})))

  (when-not (and (integer? population-size)
                 (> population-size 1))
    (throw
     (ex-info
      "Population size must be greater than one."
      {:population-size population-size})))

  (doseq [greedy-count greedy-counts]
    (when-not (and (integer? greedy-count)
                   (pos? greedy-count)
                   (< greedy-count population-size))
      (throw
       (ex-info
        "Every greedy count must be positive and smaller than population size."
        {:greedy-count greedy-count
         :population-size population-size}))))

  (doseq [[name values]
          [[:selection-counts selection-counts]
           [:injection-counts injection-counts]
           [:stagnation-thresholds stagnation-thresholds]]]

    (when (empty? values)
      (throw
       (ex-info
        "Generator option collection must not be empty."
        {:option name})))

    (doseq [value values]
      (when-not (and (integer? value)
                     (pos? value))
        (throw
         (ex-info
          "Generator integer options must be positive."
          {:option name
           :value value})))))

  (doseq [[name values]
          [[:diversity-thresholds
            diversity-thresholds]

           [:budget-utilization-thresholds
            budget-utilization-thresholds]]]

    (when (empty? values)
      (throw
       (ex-info
        "Threshold collection must not be empty."
        {:option name})))

    (doseq [value values]
      (when-not (and (number? value)
                     (<= 0.0
                         (double value)
                         1.0))
        (throw
         (ex-info
          "Thresholds must be between zero and one."
          {:option name
           :value value})))))

  (when-not (every? number?
                    scorer-scalars)
    (throw
     (ex-info
      "All scorer scalars must be numeric."
      {:scorer-scalars scorer-scalars})))

  (doseq [[name probability]
          [[:scorer-recursion-probability
            scorer-recursion-probability]

           [:program-recursion-probability
            program-recursion-probability]]]

    (when-not (and (number? probability)
                   (<= 0.0
                       (double probability)
                       1.0))
      (throw
       (ex-info
        "Recursion probabilities must be between zero and one."
        {:option name
         :probability probability}))))

  (when-not (and (integer? max-mutation-attempts)
                 (pos? max-mutation-attempts))
    (throw
     (ex-info
      "Maximum mutation attempts must be positive."
      {:max-mutation-attempts
       max-mutation-attempts})))

  config)

(defn- generate-asset-scorer
  [random max-depth config]
  (let [recursive?
        (and (> max-depth 1)
             (rng/chance?
              random
              (:scorer-recursion-probability
               config)))]

    (if-not recursive?
      (choose random
              scorer-terminals)

      (case
       (choose random
               [:+ :- :multiply])

        :+
        (list
         '+
         (generate-asset-scorer
          random
          (dec max-depth)
          config)

         (generate-asset-scorer
          random
          (dec max-depth)
          config))

        :-
        (list
         '-
         (generate-asset-scorer
          random
          (dec max-depth)
          config)

         (generate-asset-scorer
          random
          (dec max-depth)
          config))

        :multiply
        (list
         '*
         (choose
          random
          (:scorer-scalars config))

         (generate-asset-scorer
          random
          (dec max-depth)
          config))))))

(defn- generate-initializer
  [random config]
  (let [population-size
        (:population-size config)]

    (case
     (choose random
             [:random :mixed])

      :random
      (list
       'random-population
       population-size)

      :mixed
      (let [greedy-count
            (choose
             random
             (:greedy-counts config))

            random-count
            (- population-size
               greedy-count)]

        (list
         'mixed-population
         random-count
         greedy-count
         (generate-asset-scorer
          random
          2
          config))))))

(defn- generate-predicate
  [random config]
  (case
   (choose
    random
    [:stagnation
     :diversity
     :budget-utilization])

    :stagnation
    (list
     'stagnant-for?
     (choose
      random
      (:stagnation-thresholds config)))

    :diversity
    (list
     'diversity-below?
     (choose
      random
      (:diversity-thresholds config)))

    :budget-utilization
    (list
     'budget-utilization-below?
     (choose
      random
      (:budget-utilization-thresholds
       config)))))

(defn- generate-portfolio-operation
  [random max-depth config]
  (let [operation
        (choose
         random
         [:swap
          :drop-add
          :add-best
          :remove-worst])]

    (case operation
      :swap
      '(swap-assets)

      :drop-add
      (list
       'drop-add
       (generate-asset-scorer
        random
        (max 1
             (dec max-depth))
        config))

      :add-best
      (list
       'add-best
       (generate-asset-scorer
        random
        (max 1
             (dec max-depth))
        config))

      :remove-worst
      (list
       'remove-worst
       (generate-asset-scorer
        random
        (max 1
             (dec max-depth))
        config)))))

(declare generate-program)

(defn- generate-base-program
  [random max-depth config]
  (let [available-operations
        (cond-> [:inject]
          (>= max-depth 3)
          (into
           [:transform
            :local-search]))

        operation
        (choose
         random
         available-operations)]

    (case operation
      :inject
      (list
       'inject-random
       (choose
        random
        (:injection-counts config)))

      :transform
      (list
       'transform
       (generate-portfolio-operation
        random
        (dec max-depth)
        config))

      :local-search
      (list
       'local-search
       (generate-portfolio-operation
        random
        (dec max-depth)
        config)))))

(defn- generate-recursive-program
  [random max-depth config]
  (let [child-depth
        (dec max-depth)]

    (case
     (choose
      random
      [:sequence
       :conditional
       :select-then-operate])

      :sequence
      (let [child-count
            (rng/next-int-inclusive
             random
             2
             3)]

        (apply
         list
         'sequence

         (repeatedly
          child-count
          #(generate-program
            random
            child-depth
            config))))

      :conditional
      (list
       'if
       (generate-predicate
        random
        config)

       (generate-program
        random
        child-depth
        config)

       (generate-program
        random
        child-depth
        config))

      :select-then-operate
      (list
       'sequence

       (list
        'select-best
        (choose
         random
         (:selection-counts config)))

       (generate-program
        random
        child-depth
        config)))))

(defn- generate-program
  "Generates a program that is guaranteed to consume at least one
  fitness evaluation when executed on a non-empty population."
  [random max-depth config]
  (when (< max-depth 2)
    (throw
     (ex-info
      "Program generation requires depth of at least two."
      {:max-depth max-depth})))

  (let [recursive?
        (and (>= max-depth 3)
             (rng/chance?
              random
              (:program-recursion-probability
               config)))]

    (if recursive?
      (generate-recursive-program
       random
       max-depth
       config)

      (generate-base-program
       random
       max-depth
       config))))

(defn generate-expression
  "Generates a valid expression of a requested DSL type.

  max-depth refers to the maximum depth available for the generated
  replacement subtree."
  [random target-type max-depth config]
  (let [minimum-depth
        (get minimum-expression-depth
             target-type)]

    (when (nil? minimum-depth)
      (throw
       (ex-info
        "Unsupported generated DSL type."
        {:target-type target-type})))

    (when (< max-depth
             minimum-depth)
      (throw
       (ex-info
        "Insufficient depth for generated expression."
        {:target-type target-type
         :max-depth max-depth
         :minimum-depth minimum-depth})))

    (case target-type
      :asset-scorer
      (generate-asset-scorer
       random
       max-depth
       config)

      :repair-strategy
      (choose
       random
       repair-strategies)

      :initializer
      (generate-initializer
       random
       config)

      :predicate
      (generate-predicate
       random
       config)

      :portfolio-operation
      (generate-portfolio-operation
       random
       max-depth
       config)

      :program
      (generate-program
       random
       max-depth
       config)

      :optimizer
      (throw
       (ex-info
        "Use generate-optimizer for complete optimizers."
        {})))))

(defn generate-optimizer
  "Generates a complete valid optimizer S-expression.

  The outer repeat-until-budget wrapper is always preserved so every
  optimizer receives the same maximum fitness-evaluation budget."
  ([random]
   (generate-optimizer
    random
    {}))

  ([random config-overrides]
   (let [config
         (validate-config!
          (merge default-config
                 config-overrides))

         max-depth
         (:max-depth config)

         ;; optimizer -> repeat-until-budget -> generated body
         body-max-depth
         (- max-depth 2)

         optimizer
         (list
          'optimizer

          (generate-initializer
           random
           config)

          (choose
           random
           repair-strategies)

          (list
           'repeat-until-budget

           (generate-program
            random
            body-max-depth
            config)))]

     (dsl/validate!
      optimizer)

     (when (> (dsl/program-depth
               optimizer)
              max-depth)
       (throw
        (ex-info
         "Generated optimizer exceeded maximum depth."
         {:optimizer optimizer
          :actual-depth
          (dsl/program-depth optimizer)
          :maximum-depth max-depth})))

     optimizer)))

(defn subtree-at
  "Returns a subtree using argument-based paths.

  The operator itself is not part of the path. For example, path [2 0]
  means the first argument of the third optimizer argument."
  [form path]
  (loop [current
         form

         remaining-path
         (seq path)]

    (if (nil? remaining-path)
      current

      (let [argument-index
            (first remaining-path)]

        (when-not (seq? current)
          (throw
           (ex-info
            "Cannot descend into an atomic DSL value."
            {:form form
             :path path
             :current current})))

        (let [arguments
              (vec
               (rest current))]

          (when-not (< -1
                       argument-index
                       (count arguments))
            (throw
             (ex-info
              "DSL subtree path is out of bounds."
              {:form form
               :path path
               :argument-index argument-index
               :argument-count
               (count arguments)})))

          (recur
           (nth arguments
                argument-index)
           (next remaining-path)))))))

(defn replace-subtree
  "Returns a new S-expression with the subtree at path replaced."
  [form path replacement]
  (if (empty? path)
    replacement

    (do
      (when-not (seq? form)
        (throw
         (ex-info
          "Cannot replace a child of an atomic DSL value."
          {:form form
           :path path
           :replacement replacement})))

      (let [argument-index
            (first path)

            arguments
            (vec
             (rest form))

            old-child
            (nth arguments
                 argument-index)

            new-child
            (replace-subtree
             old-child
             (rest path)
             replacement)

            new-arguments
            (assoc arguments
                   argument-index
                   new-child)]

        (apply
         list
         (first form)
         new-arguments)))))

(defn typed-nodes
  "Returns all evolvable typed nodes in a DSL program.

  Operator symbols and raw numeric literals are not independently
  evolvable. Their enclosing typed expression is mutated instead."
  [form]
  (letfn
   [(walk
      [node path]
      (let [node-type
            (try
              (dsl/infer-type
               node)

              (catch clojure.lang.ExceptionInfo _
                nil))

            current-node
            (when
             (contains?
              evolvable-types
              node-type)

              [{:path path
                :form node
                :type node-type
                :depth-from-root
                (inc
                 (count path))}])

            child-nodes
            (if (seq? node)
              (mapcat
               (fn [[argument-index
                     argument]]
                 (walk
                  argument
                  (conj path
                        argument-index)))

               (map-indexed
                vector
                (rest node)))

              [])]

        (concat
         current-node
         child-nodes)))]

    (vec
     (walk form []))))

(defn- protected-node?
  [{:keys [path form]}]
  (or
   ;; Do not replace the complete optimizer.
   (empty? path)

   ;; Preserve the fixed-budget wrapper.
   (and (seq? form)
        (= 'repeat-until-budget
           (first form)))))

(defn- mutation-candidates
  [form max-depth]
  (->> (typed-nodes form)

       (remove
        protected-node?)

       (keep
        (fn [{:keys [type
                    depth-from-root]
             :as node}]

          (let [replacement-max-depth
                (inc
                 (- max-depth
                    depth-from-root))

                minimum-depth
                (get minimum-expression-depth
                     type)]

            (when (and minimum-depth
                       (>= replacement-max-depth
                           minimum-depth))
              (assoc
               node
               :replacement-max-depth
               replacement-max-depth)))))

       vec))

(defn mutate
  "Performs type-preserving subtree mutation.

  A selected subtree is replaced only by a newly generated subtree with
  exactly the same static DSL type."
  ([random optimizer]
   (mutate
    random
    optimizer
    {}))

  ([random optimizer config-overrides]
   (dsl/validate!
    optimizer)

   (let [config
         (validate-config!
          (merge default-config
                 config-overrides))

         max-depth
         (:max-depth config)

         candidates
         (mutation-candidates
          optimizer
          max-depth)]

     (when (empty? candidates)
       (throw
        (ex-info
         "Optimizer contains no mutable nodes."
         {:optimizer optimizer
          :max-depth max-depth})))

     (loop [attempt
            1]

       (when (> attempt
                (:max-mutation-attempts
                 config))
         (throw
          (ex-info
           "Could not produce a distinct valid mutation."
           {:optimizer optimizer
            :attempts
            (:max-mutation-attempts
             config)})))

       (let [{:keys [path
                     form
                     type
                     replacement-max-depth]}
             (choose
              random
              candidates)

             replacement
             (generate-expression
              random
              type
              replacement-max-depth
              config)

             mutated
             (replace-subtree
              optimizer
              path
              replacement)]

         (if (and
              (not= form
                    replacement)

              (dsl/valid?
               mutated)

              (<= (dsl/program-depth
                   mutated)
                  max-depth))

           mutated

           (recur
            (inc attempt))))))))
