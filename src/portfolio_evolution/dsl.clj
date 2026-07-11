(ns portfolio-evolution.dsl)

(def terminal-types
  {'expected-profit       :asset-scorer
   'return-per-cost       :asset-scorer
   'cost         :asset-scorer

   'ratio-repair          :repair-strategy
   'random-removal-repair :repair-strategy})

(def example-optimizer
  '(optimizer
     (mixed-population
       40
       10
       return-per-cost)

     ratio-repair

     (repeat-until-budget
       (sequence
         (select-best 25)

         (if (stagnant-for? 4)
           (inject-random 15)
           (local-search
             (drop-add return-per-cost)))

         (transform
           (swap-assets))))))

(defn- fail!
  [message form path details]
  (throw
   (ex-info
    message
    (merge
     {:form form
      :path path}
     details))))

(defn- operator-key
  [operator form path]
  (when-not (symbol? operator)
    (fail!
     "DSL operator must be a symbol."
     form
     path
     {:operator operator}))

  (when (namespace operator)
    (fail!
     "Namespaced symbols are not allowed in the DSL."
     form
     path
     {:operator operator}))

  (keyword (name operator)))

(defn- require-arity!
  [operator arguments expected form path]
  (when-not (= expected
               (count arguments))
    (fail!
     "Invalid DSL operator arity."
     form
     path
     {:operator operator
      :expected-arity expected
      :actual-arity (count arguments)})))

(defn- require-minimum-arity!
  [operator arguments minimum form path]
  (when (< (count arguments)
           minimum)
    (fail!
     "DSL operator has too few arguments."
     form
     path
     {:operator operator
      :minimum-arity minimum
      :actual-arity (count arguments)})))

(defn- require-positive-integer!
  [value form path]
  (when-not (and (integer? value)
                 (pos? value))
    (fail!
     "Expected a positive integer."
     form
     path
     {:value value})))

(defn- require-ratio!
  [value form path]
  (when-not (and (number? value)
                 (<= 0.0
                     (double value)
                     1.0))
    (fail!
     "Expected a number between 0 and 1."
     form
     path
     {:value value})))

(declare infer-type*)

(defn- require-type!
  [form expected-type path]
  (let [actual-type
        (infer-type* form path)]

    (when-not (= expected-type
                 actual-type)
      (fail!
       "DSL type mismatch."
       form
       path
       {:expected-type expected-type
        :actual-type actual-type}))

    actual-type))

(defn- infer-symbol-type
  [form path]
  (if-let [dsl-type
           (get terminal-types form)]

    dsl-type

    (fail!
     "Unknown symbol in DSL program."
     form
     path
     {:allowed-symbols
      (vec
       (sort
        (map str
             (keys terminal-types))))})))

(defn- infer-list-type
  [form path]
  (when (empty? form)
    (fail!
     "Empty lists are not valid DSL expressions."
     form
     path
     {}))

  (let [operator
        (first form)

        arguments
        (vec (rest form))

        operation
        (operator-key
         operator
         form
         path)]

    (case operation

      :optimizer
      (do
        (require-arity!
         operator arguments 3 form path)

        (require-type!
         (nth arguments 0)
         :initializer
         (conj path 1))

        (require-type!
         (nth arguments 1)
         :repair-strategy
         (conj path 2))

        (require-type!
         (nth arguments 2)
         :program
         (conj path 3))

        :optimizer)

      :random-population
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-positive-integer!
         (first arguments)
         form
         (conj path 1))

        :initializer)

      :mixed-population
      (do
        (require-arity!
         operator arguments 3 form path)

        (require-positive-integer!
         (nth arguments 0)
         form
         (conj path 1))

        (require-positive-integer!
         (nth arguments 1)
         form
         (conj path 2))

        (require-type!
         (nth arguments 2)
         :asset-scorer
         (conj path 3))

        :initializer)

      :sequence
      (do
        (require-minimum-arity!
         operator arguments 1 form path)

        (doseq [[index argument]
                (map-indexed vector arguments)]
          (require-type!
           argument
           :program
           (conj path (inc index))))

        :program)

      :repeat-until-budget
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-type!
         (first arguments)
         :program
         (conj path 1))

        :program)

      :if
      (do
        (require-arity!
         operator arguments 3 form path)

        (require-type!
         (nth arguments 0)
         :predicate
         (conj path 1))

        (require-type!
         (nth arguments 1)
         :program
         (conj path 2))

        (require-type!
         (nth arguments 2)
         :program
         (conj path 3))

        :program)

      :select-best
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-positive-integer!
         (first arguments)
         form
         (conj path 1))

        :program)

      :inject-random
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-positive-integer!
         (first arguments)
         form
         (conj path 1))

        :program)

      :transform
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-type!
         (first arguments)
         :portfolio-operation
         (conj path 1))

        :program)

      :local-search
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-type!
         (first arguments)
         :portfolio-operation
         (conj path 1))

        :program)

      :no-op
      (do
        (require-arity!
         operator arguments 0 form path)

        :program)

      :stagnant-for?
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-positive-integer!
         (first arguments)
         form
         (conj path 1))

        :predicate)

      :diversity-below?
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-ratio!
         (first arguments)
         form
         (conj path 1))

        :predicate)

      :budget-utilization-below?
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-ratio!
         (first arguments)
         form
         (conj path 1))

        :predicate)

      :swap-assets
      (do
        (require-arity!
         operator arguments 0 form path)

        :portfolio-operation)

      :drop-add
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-type!
         (first arguments)
         :asset-scorer
         (conj path 1))

        :portfolio-operation)

      :add-best
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-type!
         (first arguments)
         :asset-scorer
         (conj path 1))

        :portfolio-operation)

      :remove-worst
      (do
        (require-arity!
         operator arguments 1 form path)

        (require-type!
         (first arguments)
         :asset-scorer
         (conj path 1))

        :portfolio-operation)

      :+
      (do
        (require-arity!
         operator arguments 2 form path)

        (require-type!
         (nth arguments 0)
         :asset-scorer
         (conj path 1))

        (require-type!
         (nth arguments 1)
         :asset-scorer
         (conj path 2))

        :asset-scorer)

      :-
      (do
        (require-arity!
         operator arguments 2 form path)

        (require-type!
         (nth arguments 0)
         :asset-scorer
         (conj path 1))

        (require-type!
         (nth arguments 1)
         :asset-scorer
         (conj path 2))

        :asset-scorer)

      :*
      (do
        (require-arity!
         operator arguments 2 form path)

        (when-not (number?
                   (nth arguments 0))
          (fail!
           "The first argument to * must be numeric."
           form
           (conj path 1)
           {:value
            (nth arguments 0)}))

        (require-type!
         (nth arguments 1)
         :asset-scorer
         (conj path 2))

        :asset-scorer)

      (fail!
       "Unknown DSL operator."
       form
       path
       {:operator operator}))))

(defn- infer-type*
  [form path]
  (cond
    (number? form)
    :number

    (symbol? form)
    (infer-symbol-type
     form
     path)

    (seq? form)
    (infer-list-type
     form
     path)

    :else
    (fail!
     "Unsupported value in DSL program."
     form
     path
     {:value-type
      (type form)})))

(defn infer-type
  "Returns the static DSL type of an expression or throws ExceptionInfo."
  [form]
  (infer-type*
   form
   []))

(defn validate!
  "Validates a complete optimizer program and returns it unchanged."
  [form]
  (require-type!
   form
   :optimizer
   [])

  form)

(defn valid?
  [form]
  (try
    (validate! form)
    true

    (catch clojure.lang.ExceptionInfo _
      false)))

(defn program-node-count
  "Counts operators, literals and terminals in the S-expression tree."
  [form]
  (count
   (tree-seq
    seq?
    seq
    form)))

(defn program-depth
  "Returns the maximum tree depth. Atomic values have depth one."
  [form]
  (if (seq? form)
    (inc
     (apply
      max
      0
      (map program-depth form)))
    1))
