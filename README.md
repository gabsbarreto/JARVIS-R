# JARVIS-R

H2O-based simulation workflows for **abstract screening in systematic reviews**.

This repository contains R scripts that:
- clean and tokenize title/abstract text,
- build prediction features (including PICOS-derived variables),
- train H2O models,
- evaluate screening performance (e.g., recall, specificity, AUC-PR),
- save intermediate baked datasets and final results (`.rds`).

## Repository structure

- `scripts/`: main modeling pipelines (one script per review/topic).
- `data/`: input `.rds` files used by scripts.
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

4. Run a script from repo root. Example:

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
- Scripts can take substantial time depending on hyperparameter loops and available cores.
- Some plotting and dplyr warnings are expected (deprecated syntax in older code paths).

## Suggestions for what else to add

1. Reproducibility section: R version, OS tested, and fixed random seeds used.
2. Runtime expectations: approximate duration per script and recommended CPU/RAM.
3. Data dictionary: definitions of key columns (`FTscreening`, `P/I/C/O/S`, `totalscore`, etc.).
4. Results interpretation guide: what each metric means for screening decisions.
5. Changelog: track script updates and major modeling changes over time.
