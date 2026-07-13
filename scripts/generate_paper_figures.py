#!/usr/bin/env python3
"""
Generate publication-ready figures for the typed S-expression optimizer study.

Expected input files
--------------------
final results directory:
  aggregate-by-size.csv
  aggregate-outer.csv
  outer-seed-results.csv
  size-seed-results.csv

ablation results directory:
  ablation-aggregate-by-size.csv
  ablation-seed-results.csv

greedy baseline directory:
  greedy-baseline-aggregate-by-size.csv
  greedy-baseline-seed-results.csv

Example
-------
python scripts/generate_paper_figures.py \
  --final-dir results/final-30-seeds \
  --ablation-dir results/ablation-30-seeds \
  --greedy-dir results/greedy-baseline-30-seeds \
  --output-dir results/paper-figures \
  --formats png pdf
"""

from __future__ import annotations

import argparse
import math
from pathlib import Path
from typing import Iterable, Sequence

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.stats import t as student_t


FINAL_FILES = {
    "aggregate_size": "aggregate-by-size.csv",
    "aggregate_outer": "aggregate-outer.csv",
    "outer_seed": "outer-seed-results.csv",
    "size_seed": "size-seed-results.csv",
}

ABLATION_FILES = {
    "aggregate": "ablation-aggregate-by-size.csv",
    "seed": "ablation-seed-results.csv",
}

GREEDY_FILES = {
    "aggregate": "greedy-baseline-aggregate-by-size.csv",
    "seed": "greedy-baseline-seed-results.csv",
}

MANIFEST_COLUMNS = ["figure", "title", "description"]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate paper figures from the completed experiment CSV files."
    )
    parser.add_argument(
        "--final-dir",
        type=Path,
        default=Path("results/final-30-seeds"),
        help="Directory containing the main 30-seed experiment CSV files.",
    )
    parser.add_argument(
        "--ablation-dir",
        type=Path,
        default=Path("results/ablation-30-seeds"),
        help="Directory containing the ablation experiment CSV files.",
    )
    parser.add_argument(
        "--greedy-dir",
        type=Path,
        default=Path("results/greedy-baseline-30-seeds"),
        help="Directory containing the greedy baseline CSV files.",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("results/paper-figures"),
        help="Directory where generated figures will be written.",
    )
    parser.add_argument(
        "--formats",
        nargs="+",
        choices=("png", "pdf", "svg"),
        default=("png", "pdf"),
        help="One or more output formats. Default: png pdf.",
    )
    parser.add_argument(
        "--dpi",
        type=int,
        default=220,
        help="Raster resolution for PNG output. Default: 220.",
    )
    parser.add_argument(
        "--title-prefix",
        default="",
        help="Optional prefix added to every figure title.",
    )
    return parser.parse_args()


def read_required_csv(directory: Path, filename: str) -> pd.DataFrame:
    path = directory / filename
    if not path.is_file():
        raise FileNotFoundError(f"Required CSV file was not found: {path}")
    return pd.read_csv(path)


def require_columns(df: pd.DataFrame, source: str, columns: Iterable[str]) -> None:
    missing = sorted(set(columns) - set(df.columns))
    if missing:
        raise ValueError(
            f"{source} is missing required columns: {', '.join(missing)}"
        )


def numeric_array(values: Sequence[float] | pd.Series) -> np.ndarray:
    array = pd.to_numeric(pd.Series(values), errors="coerce").to_numpy(dtype=float)
    return array[np.isfinite(array)]


def student_t_summary(values: Sequence[float] | pd.Series) -> dict[str, float]:
    array = numeric_array(values)
    n = len(array)
    if n == 0:
        raise ValueError("Cannot calculate statistics for an empty sample.")

    mean = float(np.mean(array))
    if n == 1:
        return {
            "n": 1,
            "mean": mean,
            "sd": math.nan,
            "lower": math.nan,
            "upper": math.nan,
        }

    sd = float(np.std(array, ddof=1))
    standard_error = sd / math.sqrt(n)
    critical = float(student_t.ppf(0.975, df=n - 1))
    half_width = critical * standard_error

    return {
        "n": n,
        "mean": mean,
        "sd": sd,
        "lower": mean - half_width,
        "upper": mean + half_width,
    }


def grouped_t_summary(
    df: pd.DataFrame,
    group_column: str,
    value_column: str,
) -> pd.DataFrame:
    rows: list[dict[str, float]] = []
    for group_value, group in df.groupby(group_column, sort=True):
        summary = student_t_summary(group[value_column])
        rows.append({group_column: group_value, **summary})
    return pd.DataFrame(rows)


def y_errors(
    means: np.ndarray,
    lowers: np.ndarray,
    uppers: np.ndarray,
) -> np.ndarray:
    return np.vstack((means - lowers, uppers - means))


def title(prefix: str, text: str) -> str:
    return f"{prefix}{text}" if prefix else text


def save_figure(
    output_dir: Path,
    stem: str,
    formats: Sequence[str],
    dpi: int,
) -> list[Path]:
    output_paths: list[Path] = []
    plt.tight_layout()

    for file_format in formats:
        output_path = output_dir / f"{stem}.{file_format}"
        save_kwargs = {"bbox_inches": "tight"}
        if file_format == "png":
            save_kwargs["dpi"] = dpi
        plt.savefig(output_path, **save_kwargs)
        output_paths.append(output_path)

    plt.close()
    return output_paths


def add_error_line(
    x: np.ndarray,
    mean: np.ndarray,
    lower: np.ndarray,
    upper: np.ndarray,
    label: str,
) -> None:
    plt.errorbar(
        x,
        mean,
        yerr=y_errors(mean, lower, upper),
        marker="o",
        capsize=4,
        label=label,
    )


def figure_quality_by_size(
    greedy_aggregate: pd.DataFrame,
    output_dir: Path,
    formats: Sequence[str],
    dpi: int,
    prefix: str,
) -> list[Path]:
    x = greedy_aggregate["asset-count"].to_numpy(dtype=float)

    plt.figure(figsize=(8.4, 5.2))

    for label, mean_column, lower_column, upper_column in (
        (
            "Random search",
            "random-score-mean",
            "random-score-ci95-lower",
            "random-score-ci95-upper",
        ),
        (
            "Standard GA",
            "ga-score-mean",
            "ga-score-ci95-lower",
            "ga-score-ci95-upper",
        ),
        (
            "Evolved DSL",
            "evolved-score-mean",
            "evolved-score-ci95-lower",
            "evolved-score-ci95-upper",
        ),
    ):
        add_error_line(
            x,
            greedy_aggregate[mean_column].to_numpy(dtype=float),
            greedy_aggregate[lower_column].to_numpy(dtype=float),
            greedy_aggregate[upper_column].to_numpy(dtype=float),
            label,
        )

    plt.plot(
        x,
        greedy_aggregate["greedy-score"].to_numpy(dtype=float),
        marker="o",
        label="Greedy-only",
    )

    plt.xlabel("Number of assets")
    plt.ylabel("Mean normalized score")
    plt.title(title(prefix, "Solution quality by problem size"))
    plt.xticks(x.astype(int))
    plt.ylim(0.55, 1.01)
    plt.grid(True, alpha=0.3)
    plt.legend()

    return save_figure(
        output_dir,
        "figure_01_quality_by_size",
        formats,
        dpi,
    )


def figure_gap_by_size(
    size_seed: pd.DataFrame,
    greedy_aggregate: pd.DataFrame,
    output_dir: Path,
    formats: Sequence[str],
    dpi: int,
    prefix: str,
) -> list[Path]:
    summaries = {
        "Random search": grouped_t_summary(
            size_seed, "asset-count", "random-mean-gap"
        ),
        "Standard GA": grouped_t_summary(
            size_seed, "asset-count", "ga-mean-gap"
        ),
        "Evolved DSL": grouped_t_summary(
            size_seed, "asset-count", "final-dsl-mean-gap"
        ),
    }

    plt.figure(figsize=(8.4, 5.2))

    for label, summary in summaries.items():
        add_error_line(
            summary["asset-count"].to_numpy(dtype=float),
            summary["mean"].to_numpy(dtype=float),
            summary["lower"].to_numpy(dtype=float),
            summary["upper"].to_numpy(dtype=float),
            label,
        )

    plt.plot(
        greedy_aggregate["asset-count"].to_numpy(dtype=float),
        greedy_aggregate["greedy-mean-optimality-gap"].to_numpy(dtype=float),
        marker="o",
        label="Greedy-only",
    )

    plt.xlabel("Number of assets")
    plt.ylabel("Mean optimality gap")
    plt.title(title(prefix, "Optimality gap by problem size"))
    plt.yscale("log")
    plt.xticks(greedy_aggregate["asset-count"].to_numpy(dtype=int))
    plt.grid(True, which="both", alpha=0.3)
    plt.legend()

    return save_figure(
        output_dir,
        "figure_02_optimality_gap_log",
        formats,
        dpi,
    )


def figure_runtime_by_size(
    size_seed: pd.DataFrame,
    greedy_aggregate: pd.DataFrame,
    output_dir: Path,
    formats: Sequence[str],
    dpi: int,
    prefix: str,
) -> list[Path]:
    summaries = {
        "Random search": grouped_t_summary(
            size_seed, "asset-count", "random-mean-runtime-ms"
        ),
        "Standard GA": grouped_t_summary(
            size_seed, "asset-count", "ga-mean-runtime-ms"
        ),
        "Evolved DSL": grouped_t_summary(
            size_seed, "asset-count", "final-dsl-mean-runtime-ms"
        ),
    }

    plt.figure(figsize=(8.4, 5.2))

    for label, summary in summaries.items():
        add_error_line(
            summary["asset-count"].to_numpy(dtype=float),
            summary["mean"].to_numpy(dtype=float),
            summary["lower"].to_numpy(dtype=float),
            summary["upper"].to_numpy(dtype=float),
            label,
        )

    plt.plot(
        greedy_aggregate["asset-count"].to_numpy(dtype=float),
        greedy_aggregate["greedy-mean-runtime-ms"].to_numpy(dtype=float),
        marker="o",
        label="Greedy-only",
    )

    plt.xlabel("Number of assets")
    plt.ylabel("Mean deployment runtime (ms)")
    plt.title(title(prefix, "Deployment runtime by problem size"))
    plt.yscale("log")
    plt.xticks(greedy_aggregate["asset-count"].to_numpy(dtype=int))
    plt.grid(True, which="both", alpha=0.3)
    plt.legend()

    return save_figure(
        output_dir,
        "figure_03_deployment_runtime_log",
        formats,
        dpi,
    )


def figure_quality_runtime_tradeoff(
    size_seed: pd.DataFrame,
    greedy_aggregate: pd.DataFrame,
    output_dir: Path,
    formats: Sequence[str],
    dpi: int,
    prefix: str,
) -> list[Path]:
    runtime_columns = {
        "Random search": "random-mean-runtime-ms",
        "Standard GA": "ga-mean-runtime-ms",
        "Evolved DSL": "final-dsl-mean-runtime-ms",
    }
    score_columns = {
        "Random search": "random-score-mean",
        "Standard GA": "ga-score-mean",
        "Evolved DSL": "evolved-score-mean",
    }

    runtime_means = {
        label: size_seed.groupby("asset-count")[column].mean().sort_index()
        for label, column in runtime_columns.items()
    }

    score_lookup = greedy_aggregate.set_index("asset-count")
    asset_counts = greedy_aggregate["asset-count"].to_numpy(dtype=int)

    plt.figure(figsize=(8.4, 6.0))

    for label in ("Random search", "Standard GA", "Evolved DSL"):
        runtimes = runtime_means[label].reindex(asset_counts).to_numpy(dtype=float)
        scores = score_lookup.loc[asset_counts, score_columns[label]].to_numpy(
            dtype=float
        )
        plt.scatter(runtimes, scores, label=label)
        for runtime, score, asset_count in zip(runtimes, scores, asset_counts):
            plt.annotate(
                f"N={asset_count}",
                (runtime, score),
                xytext=(4, 4),
                textcoords="offset points",
                fontsize=8,
            )

    greedy_runtimes = score_lookup.loc[
        asset_counts, "greedy-mean-runtime-ms"
    ].to_numpy(dtype=float)
    greedy_scores = score_lookup.loc[asset_counts, "greedy-score"].to_numpy(
        dtype=float
    )
    plt.scatter(greedy_runtimes, greedy_scores, label="Greedy-only")
    for runtime, score, asset_count in zip(
        greedy_runtimes, greedy_scores, asset_counts
    ):
        plt.annotate(
            f"N={asset_count}",
            (runtime, score),
            xytext=(4, 4),
            textcoords="offset points",
            fontsize=8,
        )

    plt.xlabel("Mean deployment runtime (ms, log scale)")
    plt.ylabel("Mean normalized score")
    plt.title(title(prefix, "Quality-runtime trade-off"))
    plt.xscale("log")
    plt.grid(True, which="both", alpha=0.3)
    plt.legend()

    return save_figure(
        output_dir,
        "figure_04_quality_runtime_tradeoff",
        formats,
        dpi,
    )


def figure_outer_improvements(
    outer_seed: pd.DataFrame,
    output_dir: Path,
    formats: Sequence[str],
    dpi: int,
    prefix: str,
) -> list[Path]:
    plt.figure(figsize=(7.4, 5.0))
    plt.boxplot(
        [
            outer_seed["fitness-improvement"].to_numpy(dtype=float),
            outer_seed["training-score-improvement"].to_numpy(dtype=float),
        ],
        tick_labels=["Fitness improvement", "Training-score improvement"],
        showmeans=True,
    )
    plt.axhline(0, linewidth=1)
    plt.ylabel("Seed-level change")
    plt.title(title(prefix, "Outer-evolution quality changes"))
    plt.grid(True, axis="y", alpha=0.3)

    return save_figure(
        output_dir,
        "figure_05_outer_quality_changes",
        formats,
        dpi,
    )


def paired_before_after_plot(
    before: pd.Series,
    after: pd.Series,
    before_label: str,
    after_label: str,
    y_label: str,
    chart_title: str,
    output_dir: Path,
    stem: str,
    formats: Sequence[str],
    dpi: int,
    prefix: str,
) -> list[Path]:
    before_values = before.to_numpy(dtype=float)
    after_values = after.to_numpy(dtype=float)

    plt.figure(figsize=(7.0, 5.2))

    for before_value, after_value in zip(before_values, after_values):
        plt.plot(
            [0, 1],
            [before_value, after_value],
            marker="o",
            alpha=0.35,
        )

    plt.plot(
        [0, 1],
        [before_values.mean(), after_values.mean()],
        marker="o",
        linewidth=3,
        label="Mean",
    )

    plt.xticks([0, 1], [before_label, after_label])
    plt.ylabel(y_label)
    plt.title(title(prefix, chart_title))
    plt.grid(True, axis="y", alpha=0.3)
    plt.legend()

    return save_figure(output_dir, stem, formats, dpi)


def figure_ablation_by_size(
    ablation_aggregate: pd.DataFrame,
    greedy_aggregate: pd.DataFrame,
    output_dir: Path,
    formats: Sequence[str],
    dpi: int,
    prefix: str,
) -> list[Path]:
    x = ablation_aggregate["asset-count"].to_numpy(dtype=float)

    plt.figure(figsize=(8.4, 5.2))

    for label, column in (
        ("Full evolved optimizer", "full-score-mean"),
        ("Without greedy initialization", "no-greedy-score-mean"),
        ("Simple body", "simple-body-score-mean"),
    ):
        plt.plot(
            x,
            ablation_aggregate[column].to_numpy(dtype=float),
            marker="o",
            label=label,
        )

    plt.plot(
        greedy_aggregate["asset-count"].to_numpy(dtype=float),
        greedy_aggregate["greedy-score"].to_numpy(dtype=float),
        marker="o",
        label="Greedy-only",
    )

    plt.xlabel("Number of assets")
    plt.ylabel("Mean normalized score")
    plt.title(title(prefix, "Ablation results by problem size"))
    plt.xticks(x.astype(int))
    plt.ylim(0.60, 1.01)
    plt.grid(True, alpha=0.3)
    plt.legend()

    return save_figure(
        output_dir,
        "figure_08_ablation_by_size",
        formats,
        dpi,
    )


def figure_evolved_minus_greedy(
    greedy_seed: pd.DataFrame,
    output_dir: Path,
    formats: Sequence[str],
    dpi: int,
    prefix: str,
) -> list[Path]:
    asset_counts = sorted(greedy_seed["asset-count"].unique())
    data = [
        greedy_seed.loc[
            greedy_seed["asset-count"] == asset_count,
            "evolved-minus-greedy",
        ].to_numpy(dtype=float)
        for asset_count in asset_counts
    ]

    plt.figure(figsize=(9.0, 5.2))
    plt.boxplot(
        data,
        tick_labels=[f"N={asset_count}" for asset_count in asset_counts],
        showmeans=True,
    )
    plt.axhline(0, linewidth=1)
    plt.xlabel("Problem size")
    plt.ylabel("Evolved DSL − Greedy-only score")
    plt.title(title(prefix, "Marginal benefit over the greedy heuristic"))
    plt.grid(True, axis="y", alpha=0.3)

    return save_figure(
        output_dir,
        "figure_09_evolved_minus_greedy",
        formats,
        dpi,
    )


def figure_outer_runtime(
    outer_seed: pd.DataFrame,
    output_dir: Path,
    formats: Sequence[str],
    dpi: int,
    prefix: str,
) -> list[Path]:
    runtime_seconds = outer_seed["outer-runtime-ms"].to_numpy(dtype=float) / 1000.0
    seeds = outer_seed["seed"].to_numpy(dtype=int)

    plt.figure(figsize=(9.0, 5.0))
    plt.plot(seeds, runtime_seconds, marker="o")
    plt.axhline(runtime_seconds.mean(), linewidth=1.5, label="Mean")
    plt.xlabel("Master seed")
    plt.ylabel("Outer evolution runtime (s)")
    plt.title(title(prefix, "Offline optimizer-design cost by seed"))
    plt.xticks(seeds[:: max(1, len(seeds) // 10)])
    plt.grid(True, alpha=0.3)
    plt.legend()

    return save_figure(
        output_dir,
        "figure_10_outer_runtime_by_seed",
        formats,
        dpi,
    )


def write_manifest(output_dir: Path, rows: list[dict[str, str]]) -> Path:
    manifest_path = output_dir / "figure_manifest.csv"
    pd.DataFrame(rows, columns=MANIFEST_COLUMNS).to_csv(
        manifest_path, index=False
    )
    return manifest_path


def main() -> int:
    args = parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)

    final = {
        key: read_required_csv(args.final_dir, filename)
        for key, filename in FINAL_FILES.items()
    }
    ablation = {
        key: read_required_csv(args.ablation_dir, filename)
        for key, filename in ABLATION_FILES.items()
    }
    greedy = {
        key: read_required_csv(args.greedy_dir, filename)
        for key, filename in GREEDY_FILES.items()
    }

    require_columns(
        final["outer_seed"],
        FINAL_FILES["outer_seed"],
        {
            "seed",
            "fitness-improvement",
            "training-score-improvement",
            "initial-nodes",
            "final-nodes",
            "initial-depth",
            "final-depth",
            "outer-runtime-ms",
        },
    )
    require_columns(
        final["size_seed"],
        FINAL_FILES["size_seed"],
        {
            "asset-count",
            "random-mean-gap",
            "ga-mean-gap",
            "final-dsl-mean-gap",
            "random-mean-runtime-ms",
            "ga-mean-runtime-ms",
            "final-dsl-mean-runtime-ms",
        },
    )
    require_columns(
        ablation["aggregate"],
        ABLATION_FILES["aggregate"],
        {
            "asset-count",
            "full-score-mean",
            "no-greedy-score-mean",
            "simple-body-score-mean",
        },
    )
    require_columns(
        greedy["aggregate"],
        GREEDY_FILES["aggregate"],
        {
            "asset-count",
            "random-score-mean",
            "random-score-ci95-lower",
            "random-score-ci95-upper",
            "ga-score-mean",
            "ga-score-ci95-lower",
            "ga-score-ci95-upper",
            "greedy-score",
            "greedy-mean-optimality-gap",
            "greedy-mean-runtime-ms",
            "evolved-score-mean",
            "evolved-score-ci95-lower",
            "evolved-score-ci95-upper",
        },
    )
    require_columns(
        greedy["seed"],
        GREEDY_FILES["seed"],
        {"asset-count", "evolved-minus-greedy"},
    )

    generated: list[Path] = []
    manifest: list[dict[str, str]] = []

    def record(
        paths: list[Path],
        figure_title: str,
        description: str,
    ) -> None:
        generated.extend(paths)
        for path in paths:
            manifest.append(
                {
                    "figure": path.name,
                    "title": figure_title,
                    "description": description,
                }
            )

    record(
        figure_quality_by_size(
            greedy["aggregate"],
            args.output_dir,
            args.formats,
            args.dpi,
            args.title_prefix,
        ),
        "Solution quality by problem size",
        "Mean normalized score for Random Search, Standard GA, Greedy-only and Evolved DSL.",
    )

    record(
        figure_gap_by_size(
            final["size_seed"],
            greedy["aggregate"],
            args.output_dir,
            args.formats,
            args.dpi,
            args.title_prefix,
        ),
        "Optimality gap by problem size",
        "Mean optimality gap on a logarithmic scale; lower is better.",
    )

    record(
        figure_runtime_by_size(
            final["size_seed"],
            greedy["aggregate"],
            args.output_dir,
            args.formats,
            args.dpi,
            args.title_prefix,
        ),
        "Deployment runtime by problem size",
        "Mean runtime on a logarithmic scale. Outer evolution cost is reported separately.",
    )

    record(
        figure_quality_runtime_tradeoff(
            final["size_seed"],
            greedy["aggregate"],
            args.output_dir,
            args.formats,
            args.dpi,
            args.title_prefix,
        ),
        "Quality-runtime trade-off",
        "Mean normalized score against mean deployment runtime for each method and problem size.",
    )

    record(
        figure_outer_improvements(
            final["outer_seed"],
            args.output_dir,
            args.formats,
            args.dpi,
            args.title_prefix,
        ),
        "Outer-evolution quality changes",
        "Seed-level fitness and training-score changes from generation 0 to the final selected program.",
    )

    record(
        paired_before_after_plot(
            final["outer_seed"]["initial-nodes"],
            final["outer_seed"]["final-nodes"],
            "Generation 0",
            "Final",
            "Program node count",
            "Program-size change during outer evolution",
            args.output_dir,
            "figure_06_program_node_reduction",
            args.formats,
            args.dpi,
            args.title_prefix,
        ),
        "Program-size change during outer evolution",
        "Paired initial and final node counts for each master seed.",
    )

    record(
        paired_before_after_plot(
            final["outer_seed"]["initial-depth"],
            final["outer_seed"]["final-depth"],
            "Generation 0",
            "Final",
            "Program depth",
            "Program-depth change during outer evolution",
            args.output_dir,
            "figure_07_program_depth_reduction",
            args.formats,
            args.dpi,
            args.title_prefix,
        ),
        "Program-depth change during outer evolution",
        "Paired initial and final depths for each master seed.",
    )

    record(
        figure_ablation_by_size(
            ablation["aggregate"],
            greedy["aggregate"],
            args.output_dir,
            args.formats,
            args.dpi,
            args.title_prefix,
        ),
        "Ablation results by problem size",
        "Full optimizer, no-greedy initializer, simple body and Greedy-only comparison.",
    )

    record(
        figure_evolved_minus_greedy(
            greedy["seed"],
            args.output_dir,
            args.formats,
            args.dpi,
            args.title_prefix,
        ),
        "Marginal benefit over the greedy heuristic",
        "Seed-level difference between Evolved DSL and Greedy-only.",
    )

    record(
        figure_outer_runtime(
            final["outer_seed"],
            args.output_dir,
            args.formats,
            args.dpi,
            args.title_prefix,
        ),
        "Offline optimizer-design cost by seed",
        "Wall-clock runtime of the outer evolutionary design process for each master seed.",
    )

    manifest_path = write_manifest(args.output_dir, manifest)

    print("Generated figures:")
    for path in generated:
        print(f"  {path}")
    print(f"Manifest: {manifest_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
