(ns portfolio-evolution.knapsack)

(defn validate-instance!
  [{:keys [assets budget] :as instance}]
  (when-not (vector? assets)
    (throw
     (ex-info "Instance assets must be a vector."
              {:instance instance})))

  (when-not (and (integer? budget)
                 (not (neg? budget)))
    (throw
     (ex-info "Budget must be a non-negative integer."
              {:budget budget})))

  (doseq [[index asset] (map-indexed vector assets)]
    (when-not (and (integer? (:cost asset))
                   (pos? (:cost asset)))
      (throw
       (ex-info "Every asset must have a positive integer cost."
                {:asset-index index
                 :asset asset})))

    (when-not (and (integer? (:expected-profit asset))
                   (not (neg? (:expected-profit asset))))
      (throw
       (ex-info
        "Every asset must have a non-negative integer expected profit."
        {:asset-index index
         :asset asset}))))

  instance)

(defn selected-gene?
  [gene]
  (or (= gene 1)
      (true? gene)))

(defn valid-gene?
  [gene]
  (or (= gene 0)
      (= gene 1)
      (false? gene)
      (true? gene)))

(defn evaluate-solution
  "Evaluates a binary portfolio.

  The solution may contain 0/1 values or false/true values."
  [{:keys [assets budget] :as instance} solution]
  (validate-instance! instance)

  (when-not (= (count assets)
               (count solution))
    (throw
     (ex-info "Solution length must match the number of assets."
              {:asset-count (count assets)
               :solution-length (count solution)})))

  (doseq [[index gene] (map-indexed vector solution)]
    (when-not (valid-gene? gene)
      (throw
       (ex-info "Solution genes must be binary."
                {:gene-index index
                 :gene gene}))))

  (let [{:keys [cost objective selected-indices]}
        (reduce-kv
         (fn [acc index gene]
           (if (selected-gene? gene)
             (let [{:keys [cost expected-profit]}
                   (nth assets index)]
               (-> acc
                   (update :cost + cost)
                   (update :objective + expected-profit)
                   (update :selected-indices conj index)))
             acc))

         {:cost 0
          :objective 0
          :selected-indices []}

         (vec solution))]

    {:solution (mapv #(if (selected-gene? %) 1 0)
                     solution)
     :cost cost
     :objective objective
     :selected-indices selected-indices
     :selected-asset-count (count selected-indices)
     :feasible? (<= cost budget)}))

(defn exact-solve
  "Solves a 0/1 knapsack instance exactly using dynamic programming.

  This is the reference oracle for small synthetic instances. It is not an
  experimental baseline."
  [{:keys [assets budget] :as instance}]
  (validate-instance! instance)

  (let [asset-count (count assets)

        ;; Row i stores the best value attainable by considering
        ;; the first i assets.
        table
        (vec
         (repeatedly
          (inc asset-count)
          #(long-array (inc budget))))]

    ;; Build the dynamic-programming table.
    (doseq [i (range 1 (inc asset-count))]
      (let [{:keys [cost expected-profit]}
            (nth assets (dec i))

            ^longs previous-row
            (nth table (dec i))

            ^longs current-row
            (nth table i)]

        (dotimes [available-budget (inc budget)]
          (let [without-current
                (aget previous-row available-budget)

                with-current
                (if (<= cost available-budget)
                  (+ (long expected-profit)
                     (aget previous-row
                           (- available-budget cost)))
                  Long/MIN_VALUE)]

            (aset-long
             current-row
             available-budget
             (long
              (max without-current
                   with-current)))))))

    ;; Reconstruct one optimal solution.
    (let [selected-indices
          (loop [i asset-count
                 remaining-budget budget
                 selected []]

            (if (zero? i)
              (vec (reverse selected))

              (let [asset-index (dec i)
                    {:keys [cost]}
                    (nth assets asset-index)

                    ^longs current-row
                    (nth table i)

                    ^longs previous-row
                    (nth table asset-index)

                    current-value
                    (aget current-row remaining-budget)

                    previous-value
                    (aget previous-row remaining-budget)]

                (if (= current-value previous-value)
                  (recur asset-index
                         remaining-budget
                         selected)

                  (recur asset-index
                         (- remaining-budget cost)
                         (conj selected asset-index))))))

          selected-index-set
          (set selected-indices)

          solution
          (mapv
           (fn [asset-index]
             (if (contains? selected-index-set asset-index)
               1
               0))
           (range asset-count))

          evaluation
          (evaluate-solution instance solution)

          ^longs final-row
          (nth table asset-count)

          optimal-objective
          (aget final-row budget)]

      (when-not (= optimal-objective
                   (:objective evaluation))
        (throw
         (ex-info
          "Dynamic-programming reconstruction does not match the optimum."
          {:table-objective optimal-objective
           :reconstructed-objective (:objective evaluation)})))

      (assoc evaluation
             :optimal? true
             :optimal-objective optimal-objective))))
