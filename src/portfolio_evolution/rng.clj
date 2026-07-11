(ns portfolio-evolution.rng
  (:import
   [java.util SplittableRandom]))

(defn create
  "Creates a deterministic random-number generator from a seed."
  ^SplittableRandom
  [seed]
  (SplittableRandom. (long seed)))

(defn next-int-inclusive
  "Returns an integer in the inclusive range [lower, upper]."
  [^SplittableRandom random lower upper]
  (when (> lower upper)
    (throw
     (ex-info "Lower bound must not exceed upper bound."
              {:lower lower
               :upper upper})))
  (.nextInt random
            (int lower)
            (int (inc upper))))
