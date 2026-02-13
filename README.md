# JARVIS-R Validation Scripts

This repository contains R scripts used to **test and validate JARVIS behavior** on retrospective systematic-review datasets.

Important scope note:
- This codebase is an evaluation/simulation harness used to measure how efficient JARVIS-style screening would be under different settings.

## What these scripts do

Across topics (for example anticoagulation, cocaine, hypertension), the scripts:
- load topic-specific `.rds` datasets,
- clean and parse LLM-derived PICOS-style responses,
- engineer text and structured features (`recipes`, tokenization, TF-IDF, PCA, one-hot encoding),
- run iterative H2O deep-learning screening loops with hyperparameter sweeps,
- output metrics that quantify screening efficiency and error trade-offs over rounds.

## Outputs and how they show JARVIS efficiency

These scripts produce metrics specifically to evaluate efficiency:
- How many records can be safely excluded.
- How many records still need human reading.
- How many relevant records are retained or missed.
- The ranking quality of probabilities (AUC-PR).

Primary output objects:
- `results` (saved as `.rds`): round-level predictions and metadata for each configuration/repeat.
- `EXCINC`: records incorrectly excluded (safety check).
- `sumtextPICOS`: aggregated performance summary by round/configuration.

## Key metrics in `sumtextPICOS`

- `inc.correct`: true positives (relevant records correctly included).
- `inc.incorrect`: false positives (irrelevant records incorrectly included).
- `exc.correct`: true negatives (irrelevant records correctly excluded).
- `exc.incorrect`: false negatives (relevant records incorrectly excluded).
- `incpred`: cumulative records reviewed as include by that round.
- `excpred`: cumulative records reviewed as exclude by that round.
- `percread`: percentage of total records read by humans.
- `percsave`: proportion of total records correctly excluded (workload saved proxy).
- `recall`: sensitivity at that iteration.
- `recall_cumm`: cumulative recall proxy used in the scripts.
- `specificity`: true-negative rate.
- `aucpr`: precision-recall AUC from model scores.
- `foundft`: percentage indicator related to full-text includes retained/found in the simulation.
- `wss95`: work-saved-over-sampling style statistic at the 95% reference used in the code.

Together, these metrics quantify whether JARVIS-style screening is both:
- efficient (lower `percread`, higher `percsave`), and
- safe (high `recall`, low `exc.incorrect`).

## Repository structure

- `scripts/`: validation scripts (one per dataset/topic).
- `data/`: input `.rds` files.
- `baked/`: cached engineered datasets.
- `results/`: saved experiment outputs (`.rds`).
- `requirements.R`: package installation/loading helper.

## Running a validation script

1. Open the project in RStudio or set working directory to repo root.
2. Install/load dependencies:

```r
source("requirements.R")
```

3. Run one validation script, for example:

```r
source("scripts/Anticoag 2 h2o.R")
```

Or from terminal:

```bash
Rscript "scripts/Anticoag 2 h2o.R"
```

4. Review generated outputs in `results/` and in-session objects/plots (`EXCINC`, `sumtextPICOS`).

## Notes

- H2O requires Java on `PATH`.
- Scripts are compute-intensive; runtime and memory usage can be high.
- Parallel mode (`*_parallel`) needs substantially more RAM.
- Some scripts use older dplyr idioms and may emit warnings.
- Results vary by dataset and hyperparameters; compare metrics before drawing conclusions.
