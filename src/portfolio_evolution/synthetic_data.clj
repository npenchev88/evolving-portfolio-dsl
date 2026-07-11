(ns portfolio-evolution.synthetic-data
  (:require
   [portfolio-evolution.rng :as rng]))

(def default-config
  {:train-data-seed 4242
   :test-data-seed 9898
   :train-instance-count 10
   :test-instance-count 10
   :asset-count 50
   :minimum-cost 10
   :maximum-cost 100
   :minimum-expected-profit 5
   :maximum-expected-profit 150
   :budget-ratio 0.30})

(defn generate-asset
  "Generates one synthetic asset.

  In the initial experiment:
  - cost represents required capital;
  - expected-profit represents the utility obtained by selecting the asset."
  [random asset-id
   {:keys [minimum-cost
           maximum-cost
           minimum-expected-profit
           maximum-expected-profit]}]
  {:id asset-id
   :cost
   (rng/next-int-inclusive
    random
    minimum-cost
    maximum-cost)

   :expected-profit
   (rng/next-int-inclusive
    random
    minimum-expected-profit
    maximum-expected-profit)})

(defn generate-instance
  "Generates one synthetic binary portfolio-selection instance."
  [random family instance-index
   {:keys [asset-count budget-ratio]
    :as config}]
  (let [instance-id
        (format "%s-%03d"
                (name family)
                instance-index)

        assets
        (mapv
         (fn [asset-index]
           (generate-asset
            random
            (format "%s-asset-%03d"
                    instance-id
                    asset-index)
            config))
         (range asset-count))

        total-cost
        (reduce + (map :cost assets))

        budget
        (max 1
             (long
              (* (double budget-ratio)
                 (double total-cost))))]

    {:id instance-id
     :family family
     :assets assets
     :budget budget
     :total-available-cost total-cost}))

(defn generate-instances
  "Generates a deterministic collection of instances from one data seed."
  [data-seed family instance-count config]
  (let [random (rng/create data-seed)]
    (mapv
     (fn [instance-index]
       (generate-instance
        random
        family
        instance-index
        config))
     (range instance-count))))

(defn generate-dataset
  "Generates fixed training and held-out test datasets.

  Training and test data use separate fixed seeds. Algorithm seeds must not
  alter these instances."
  ([]
   (generate-dataset {}))

  ([config-overrides]
   (let [{:keys [train-data-seed
                 test-data-seed
                 train-instance-count
                 test-instance-count]
          :as config}
         (merge default-config config-overrides)]

     {:config config

      :train
      (generate-instances
       train-data-seed
       :train
       train-instance-count
       config)

      :test
      (generate-instances
       test-data-seed
       :test
       test-instance-count
       config)})))
