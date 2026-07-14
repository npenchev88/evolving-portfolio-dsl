# Evolving Interpretable Optimizers as Typed Clojure S-Expressions

A reproducible research prototype for evolving executable optimization programs with **Genetic Programming** in Clojure.

Instead of evolving portfolio solutions directly or tuning only fixed hyperparameters, the system evolves **typed S-expression programs** that define:

- population initialization;
- feasibility repair;
- local-search and transformation operators;
- repeated search behavior.

The programs are evaluated on synthetic binary 0–1 knapsack-like portfolio-selection problems. An exact **Dynamic Programming** solver provides the reference optimum.

> **Main scientific result:** the evolved programs strongly outperform `Random Search` and a `Standard Genetic Algorithm`, but ablation and `Greedy-only` experiments show that almost all of the quality comes from the provided `return-per-cost` greedy initialization primitive. The outer evolutionary process mainly selects this strong domain heuristic and simplifies programs around it; it does not consistently evolve search bodies that materially improve over the heuristic itself.

## Research questions

The project investigates whether:

1. Genetic Programming can synthesize valid and interpretable optimization programs as typed S-expressions.
2. The evolved optimizer programs generalize from 50-asset training problems to unseen problems with 50, 100, 250, and 500 assets.
3. Outer evolution improves held-out solution quality relative to the best program in generation 0.
4. Parsimony pressure reduces program complexity without reducing solution quality.
5. The evolved search body adds value beyond greedy initialization.
6. `Evolved DSL` provides a practically meaningful advantage over `Greedy-only`.

`DSL` means **Domain-Specific Language**.

## Representative evolved optimizer

```clojure
(optimizer
  (mixed-population 40 10 return-per-cost)
  random-removal-repair
  (repeat-until-budget
    (local-search
      (swap-assets))))
```

The expression is both:

- an executable optimizer specification;
- a typed program tree that can be generated, validated, measured, and mutated.

The runtime uses a restricted interpreter rather than unrestricted `eval`.

## Experimental design

| Component | Setting |
|---|---:|
| Training tasks | 10 synthetic instances |
| Training size | 50 assets |
| Test tasks | 10 instances per problem size |
| Test sizes | 50, 100, 250, 500 assets |
| Master seeds | 30 |
| Outer population | 20 optimizer programs |
| Outer generations | 10 after generation 0 |
| Inner evaluation budget | 150 solution evaluations per program and training instance |
| Parsimony coefficient | 0.0005 per program node |
| Exact reference | Dynamic Programming |
| Deployment methods | Random Search, Standard Genetic Algorithm, Greedy-only, Evolved DSL |

For each asset \(i\), the generator creates a positive cost \(c_i\) and profit \(p_i\). A solution selects assets with binary variables \(x_i \in \{0,1\}\):

```text
maximize  Σ p_i x_i
subject to Σ c_i x_i ≤ capacity
```

The capacity is 30% of the total asset cost.

The normalized score is:

```text
best objective / exact Dynamic Programming optimum
```

A score of `1.0` is optimal.

## Main results

Mean normalized score across 30 master seeds:

| Assets | Random Search | Standard Genetic Algorithm | Greedy-only | Evolved DSL |
|---:|---:|---:|---:|---:|
| 50 | 0.748132 | 0.902878 | 0.996688 | 0.996827 |
| 100 | 0.703119 | 0.866410 | 0.998205 | 0.998217 |
| 250 | 0.637872 | 0.816603 | 0.999327 | 0.999334 |
| 500 | 0.604568 | 0.792221 | 0.999675 | 0.999676 |

The `Evolved DSL − Greedy-only` paired difference was:

| Assets | Mean difference | 95% Student-t confidence interval |
|---:|---:|---:|
| 50 | +0.000139 | [0.000009, 0.000269] |
| 100 | +0.000012 | [−0.000004, 0.000028] |
| 250 | +0.000007 | [−0.000001, 0.000014] |
| 500 | +0.000001 | [−0.000001, 0.000003] |

Only the 50-asset result is statistically above zero, and the effect is practically very small.

### Ablation findings

Removing greedy initialization causes a large drop:

| Assets | Full Evolved DSL | Without greedy initialization | Difference |
|---:|---:|---:|---:|
| 50 | 0.996827 | 0.750799 | +0.246028 |
| 100 | 0.998217 | 0.709934 | +0.288284 |
| 250 | 0.999334 | 0.660309 | +0.339025 |
| 500 | 0.999676 | 0.635364 | +0.364311 |

Replacing the evolved search body with a simple random-injection body produces almost the same result as the full optimizer.

This indicates that performance is **initializer-driven**, not search-body-driven.

### Program simplification

Across 30 outer-evolution runs:

| Metric | Mean change | 95% Student-t confidence interval |
|---|---:|---:|
| Composite fitness improvement | +0.006371 | [0.000817, 0.011924] |
| Training-score improvement | +0.003037 | [−0.002185, 0.008260] |
| Node reduction | 6.67 nodes | [2.37, 10.96] |
| Depth reduction | 0.87 levels | [0.25, 1.48] |

The outer process reliably reduces program size and depth, while the improvement in raw training score is not statistically conclusive.

## Runtime

Deployment runtime is measured separately from the one-time outer optimizer-design cost.

Mean deployment runtime in milliseconds:

| Assets | Random Search | Standard Genetic Algorithm | Greedy-only | Evolved DSL |
|---:|---:|---:|---:|---:|
| 50 | 9.69 | 11.43 | 1.37 | 11.10 |
| 100 | 19.77 | 23.12 | 1.17 | 24.45 |
| 250 | 54.78 | 60.74 | 1.50 | 85.70 |
| 500 | 115.99 | 125.38 | 1.81 | 249.88 |

`Greedy-only` constructs one deterministic solution and uses one objective evaluation. The stochastic methods use a budget of 150 solution evaluations per instance.

The mean outer-evolution runtime is approximately **46.57 seconds per master seed** in the measured environment. This is an offline design cost and must not be mixed with deployment runtime.

## Requirements

### Clojure experiments

- Java 21
- Clojure CLI
- Clojure 1.12.5

The Clojure dependencies are declared in `deps.edn`.

### Figure generation

- Python 3.10 or newer
- packages from `requirements-figures.txt`

## Quick start

Run the complete test suite:

```bash
clojure -M:test
```

Run a fixed baseline experiment:

```bash
clojure -M:run \
  --seed 1 \
  --results-dir results/baseline-demo
```

Evolve one optimizer program:

```bash
clojure -M:evolve-demo \
  --seed 1 \
  --results-dir results/evolution-demo
```

Evaluate the saved optimizer on held-out 50-asset tasks:

```bash
clojure -M:evaluate-demo \
  --seed 1 \
  --results-dir results/evolution-demo
```

Evaluate scale generalization:

```bash
clojure -M:scale-evaluate-demo \
  --seed 1 \
  --results-dir results/evolution-demo
```

## Reproduce the 30-seed experiment

```bash
clojure -M:final-experiment \
  --seed-start 1 \
  --seed-count 30 \
  --results-dir results/final-30-seeds
```

The experiment supports resume. Existing per-seed result files are loaded automatically when the same command is run again.

Main outputs:

```text
results/final-30-seeds/
├── seed-01.edn
├── ...
├── seed-30.edn
├── aggregate.edn
├── aggregate-by-size.csv
├── aggregate-outer.csv
├── outer-seed-results.csv
└── size-seed-results.csv
```

## Run the ablation experiment

The ablation runner reuses the already evolved final programs. It does not repeat outer evolution.

```bash
clojure -M \
  -m portfolio-evolution.ablation-experiment \
  --seed-start 1 \
  --seed-count 30 \
  --input-dir results/final-30-seeds \
  --output-dir results/ablation-30-seeds
```

It evaluates:

1. the full evolved optimizer;
2. the same optimizer without greedy initialization;
3. the same initializer and repair strategy with a simple search body.

## Run the Greedy-only baseline

```bash
clojure -M:greedy-baseline \
  --seed-start 1 \
  --seed-count 30 \
  --input-dir results/final-30-seeds \
  --output-dir results/greedy-baseline-30-seeds
```

This adds the deterministic `return-per-cost` constructive heuristic as a standalone baseline.

## Generate paper figures

Create a Python environment:

```bash
python3 -m venv .venv-figures
source .venv-figures/bin/activate

python -m pip install --upgrade pip
python -m pip install -r requirements-figures.txt
```

Generate PNG and PDF figures:

```bash
python scripts/generate_paper_figures.py \
  --final-dir results/final-30-seeds \
  --ablation-dir results/ablation-30-seeds \
  --greedy-dir results/greedy-baseline-30-seeds \
  --output-dir results/paper-figures \
  --formats png pdf
```

The script produces figures for:

- solution quality;
- optimality gap;
- deployment runtime;
- quality–runtime trade-off;
- outer-evolution changes;
- node and depth reduction;
- ablation results;
- `Evolved DSL − Greedy-only`;
- offline outer-evolution runtime.

## Project structure

```text
src/portfolio_evolution/
├── rng.clj                         # explicit deterministic seed handling
├── synthetic_data.clj              # fixed train/test synthetic instances
├── knapsack.clj                    # exact Dynamic Programming oracle
├── metrics.clj                     # normalized score, gap, Student-t intervals
├── baseline_ga.clj                 # Standard Genetic Algorithm
├── random_search.clj               # random portfolio baseline
├── greedy_baseline.clj             # standalone return-per-cost baseline
├── dsl.clj                         # typed S-expression language and validation
├── optimizer_runtime.clj           # restricted interpreter
├── program_generation.clj          # valid generation and typed mutation
├── outer_gp.clj                    # outer Genetic Programming process
├── held_out_evaluation.clj         # held-out evaluation
├── scale_evaluation.clj            # 50/100/250/500-asset evaluation
├── final_experiment.clj            # reproducible multi-seed pipeline
├── ablation_experiment.clj         # initializer and body ablations
└── greedy_baseline_experiment.clj  # Greedy-only comparison

scripts/
└── generate_paper_figures.py

test/portfolio_evolution/
└── ...                             # unit and end-to-end tests
```

## Reproducibility and safeguards

The project is designed around:

- explicit master seeds and deterministic derived seeds;
- fixed training and held-out test suites;
- no test instances during outer selection;
- equal deployment budgets for the stochastic methods;
- exact Dynamic Programming reference values;
- typed, valid-by-construction programs;
- restricted interpretation without arbitrary `eval`;
- per-seed result persistence and resume;
- paired statistical comparisons;
- 95% Student-t confidence intervals across seed-level aggregates.

## Interpretation and limitations

The results should not be interpreted as evidence that Genetic Programming independently invented the `profit / cost` heuristic. The `return-per-cost` primitive was explicitly included in the language.

The present experiment is also limited by:

- one synthetic generator with independent costs and profits;
- a strong domain-specific primitive in the grammar;
- mutation-only outer evolution;
- no equal-budget `Random DSL Search`;
- incomplete population-level semantic-diversity measurement;
- one class of combinatorial optimization problems;
- runtime dependence on Java Virtual Machine warm-up and local hardware.

A natural next experiment is to remove `return-per-cost` as a ready-made primitive and allow the scoring formula itself to evolve from arithmetic S-expressions.

## Scientific takeaway

High benchmark scores alone would suggest successful optimizer synthesis. The ablation and strong heuristic baseline reveal a more precise result:

> Genetic Programming repeatedly identifies a dominant domain-specific initialization heuristic and compresses programs around it, while the evolved search body contributes little additional solution quality on the studied synthetic problem family.

That finding is the central result of the repository.
