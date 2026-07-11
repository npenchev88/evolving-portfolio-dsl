(ns portfolio-evolution.rng
  (:import
   [java.util SplittableRandom]))

(defn create
  "Creates a deterministic random-number generator from a seed."
  ^SplittableRandom
  [seed]
  (SplittableRandom. (long seed)))

(defn next-int
  "Returns an integer in [0, upper-exclusive)."
  [^SplittableRandom random upper-exclusive]
  (when-not (pos? upper-exclusive)
    (throw
     (ex-info "Upper bound must be positive."
              {:upper-exclusive upper-exclusive})))
  (.nextInt random (int upper-exclusive)))

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

(defn next-double
  "Returns a double in [0.0, 1.0)."
  [^SplittableRandom random]
  (.nextDouble random))

(defn next-boolean
  [^SplittableRandom random]
  (.nextBoolean random))

(defn chance?
  "Returns true with the supplied probability."
  [^SplittableRandom random probability]
  (when-not (<= 0.0 probability 1.0)
    (throw
     (ex-info "Probability must be between 0 and 1."
              {:probability probability})))
  (< (next-double random)
     (double probability)))
