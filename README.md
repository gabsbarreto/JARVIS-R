# JARVIS-R

JARVIS-R is a machine-learning decision support workflow for **title/abstract screening in systematic reviews**.

Its goal is to reduce manual screening workload while maintaining high sensitivity for relevant studies. In this repository, JARVIS:
- converts review metadata and abstract text into model-ready features (including PICOS-derived signals),
- trains and tests H2O models across parameter settings,
- estimates screening trade-offs (for example recall, specificity, and AUC-PR),
- outputs ranked/thresholded predictions so reviewers can prioritize likely includes and safely exclude low-priority records faster.

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

## Notes

- H2O requires Java; if `h2o.init()` fails, verify Java is installed and on `PATH`.
- H2O in these scripts is configured for local execution (`localhost:54321`) with multi-threading.
- Scripts can take substantial time depending on hyperparameter loops and available cores.
- Some plotting and dplyr warnings are expected (deprecated syntax in older code paths).
- Results can vary by dataset/topic; validate performance per review before operational use.

## Repository structure

- `scripts/`: main modeling pipelines (one script per review/topic).
- `data/`: input `.rds` files used by scripts.
- `baked/`: intermediate processed datasets created by most scripts.
- `results/`: saves results so the tester can visualise and summarise results without having to run the script multiple times.
- `requirements.R`: installs/loads packages needed by scripts.

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
- final model results are saved as `results*.rds` in the `/results` folder.
- scripts also save the baked datasets in `baked/`, reducing time spent when only reproducing tests.
- plots are produced in-session to visualize trade-offs.
- the DF called `EXCINC` will show every study incorrectly excluded by JARVIS at each round.
- the DF called `sumtextPICOS` will show a summary of the metrics obtained. 

## How to interpret outputs in `sumtextPICOS`

- `excpred`: number of records EXCLUDED by the human reviewer up to that point.
- `incpred`: number of records tagged as RELEVANT by the human reviewer up to that point.
- `inc.correct`: records correctly classified by JARVIS as RELEVANT (TRUE POSITIVES).
- `inc.incorrect`: records incorrectly classified by JARVIS as RELEVANT (FALSE POSITIVES).
- `exc.correct`: records correctly classified by JARVIS as EXCLUDE (TRUE NEGATIVES).
- `exc.incorrect`: records incorrectly classified by JARVIS as EXCLUDE (FALSE NEGATIVES).
- `recall`: sensitivity of JARVIS at a single iteration.
- `recall_cumm`: sensitivity of JARVIS when considering human decisions.
- `specificity`: ability to exclude irrelevant studies.
- `percread`: percentage of records human reviewers had seen at that point.
- `percsave`: percentage of time saved if process stopped at that point.
- `aucpr`: ranking quality for imbalanced include/exclude classes.


FOR MORE INFORMATION, READ (link to publication).