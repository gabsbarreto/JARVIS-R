# JARVIS-R

JARVIS-R is a machine-learning decision support workflow for **title/abstract screening in systematic reviews**.

Its goal is to reduce manual screening workload while maintaining high sensitivity for relevant studies. In this repository, JARVIS:
- converts review metadata and abstract text into model-ready features (including PICOS-derived signals),
- trains and tests H2O models across parameter settings,
- estimates screening trade-offs (for example recall, specificity, and AUC-PR),
- outputs ranked/thresholded predictions so reviewers can prioritize likely includes and safely exclude low-priority records faster.


## Repository structure

- `scripts/`: main modeling pipelines (one script per review/topic).
- `data/`: input `.rds` files used by scripts.
- `baked/`: intermediate processed datasets created by most scripts.
- `requirements.R`: installs/loads packages needed by scripts.
- project root: output files (`results*.rds`, plots, some `*final.rds`).

## Step-by-step

1. Open the project in RStudio (recommended) or set working directory to repo root.

2. Install and load dependencies:

```r
source("requirements.R")
```

3. Check input data files exist:

```r
list.files("data")
```

4. Run one screening workflow script from repo root. Example:

```r
source("scripts/Sedat behaviour children h2o.R")
```

Or from terminal:

```bash
Rscript "scripts/Sedat behaviour children h2o.R"
```

5. Review outputs:
- final model results are saved as `results*.rds` in project root,
- most scripts also save baked datasets in `baked/`.
- plots are produced in-session to visualize trade-offs.

## Intended use

JARVIS is intended as a **screening prioritization tool**, not a replacement for reviewer judgment.

- Use model outputs to rank studies and choose practical read thresholds.
- Prioritize settings that maintain high recall for likely includes.
- Keep a human-in-the-loop process for final include/exclude decisions, especially near model thresholds.

## Methods overview

The scripts follow a common modeling pattern:

1. Data preparation
- Load topic-specific `.rds` data.
- Clean text fields (normalize case, punctuation, numbers, and encoding artifacts).
- Parse PICOS-style fields and derive numeric features (for example `Pn`, `In`, `Cn`, `On`, `Sn`, `totalscore`).

2. Feature engineering (recipes)
- Tokenize abstract text.
- Remove stopwords.
- Build n-grams (up to 3-grams).
- Compute TF-IDF features.
- Reduce dimensionality with PCA on TF-IDF space (retain ~90% variance).
- One-hot encode structured categorical variables (P/I/C/O/S/review).

3. Model training (H2O deep learning)
- Train Bernoulli classification models (`Include` vs `Exclude`) with weighted classes.
- Use cross-validation folds and average predictions from CV models.
- Sweep hyperparameters across epochs, architecture, activation, regularization, learning rate, stopping tolerance, and mini-batch size.

4. Iterative screening simulation
- Start from a small mixed sample of likely includes/excludes.
- Retrain and rescore unscreened records in rounds.
- Use probability thresholds to simulate what would be read next vs excluded.
- Track workload and error trade-offs over rounds.

5. Evaluation
- Main metrics include recall, specificity, percentage read (`percread`), and AUC-PR.
- Additional summaries quantify potentially missed full-text includes (`foundft`) under each operating point.

## How to interpret outputs

- `results*.rds`: run-level outputs with predictions and metrics across configurations.
- `recall`: sensitivity to included studies (higher is safer for screening).
- `specificity`: ability to exclude irrelevant studies.
- `percread`: percentage of records still needing manual review.
- `aucpr`: ranking quality for imbalanced include/exclude classes.

In practice, choose operating points that preserve recall while reducing `percread`.

## Script inputs and outputs

- `scripts/Sedat behaviour children h2o.R`
  - input: `data/sedatbehav.completeDEEPSEEK.rds`
  - outputs: `Philippa 3 final.rds`, `resultsphil3.rds`

- `scripts/PO-UR h2o.R`
  - input: `data/PO-URcompleteDEEPSEEK.rds`
  - outputs: `baked/Philippa 1 final.rds`, `resultsphil1.rds`

- `scripts/Anticoag atrial fib h2o.R`
  - input: `data/anticoag.completeDEEPSEEK.rds`
  - outputs: `baked/Philippa 2 final.rds`, `resultsphil2.rds`

- `scripts/Hypertension h2o.R`
  - input: `data/dfgpt.stress.complete.rds`
  - outputs: `baked/hypertension final.rds`, `resultshypertension.rds`

- `scripts/CYP1A2 h2o.R`
  - input: `data/CYP1A2O-newpipe.rds`
  - outputs: `baked/CYP1A2 final.rds`, `resultsCYP.rds`

- `scripts/Anticoag 2 h2o.R`
  - input: `data/dfGPT.anticoag2.complete.rds`
  - outputs: `baked/Philippa 5 final.rds`, `resultsphil5.rds`

- `scripts/cocaine h2o.R`
  - input: `data/dfgpt.cocaine.complete.rds`
  - outputs: `baked/Cocaine final.rds`, `resultscocaine.rds`

## Notes

- H2O requires Java; if `h2o.init()` fails, verify Java is installed and on `PATH`.
- H2O in these scripts is configured for local execution (`localhost:54321`) with multi-threading.
- Scripts can take substantial time depending on hyperparameter loops and available cores.
- Some plotting and dplyr warnings are expected (deprecated syntax in older code paths).
- Results can vary by dataset/topic; validate performance per review before operational use.

## Recommended next additions

1. Reproducibility details: R version, OS tested, and fixed random seeds used.
2. Runtime benchmarks: approximate duration per script and suggested CPU/RAM.
3. Data dictionary: definitions of key columns (`FTscreening`, `P/I/C/O/S`, `totalscore`, etc.).
4. Validation policy: minimum acceptable recall/safety threshold before deployment.
5. Changelog: tracking script updates and major modeling changes over time.
