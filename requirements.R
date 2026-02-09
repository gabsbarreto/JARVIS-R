options(repos = c(CRAN = "https://cloud.r-project.org"))

# Packages required by scripts in ./scripts
required_packages <- c(
  "tidyverse",
  "recipes",
  "textrecipes",
  "h2o",
  "future",
  "future.apply",
  "tibble",
  "dplyr",
  "ggpubr",
  "PRROC"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))
