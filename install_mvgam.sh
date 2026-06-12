#!/bin/bash -l
# One-time interactive install of mvgam + cmdstanr into the persistent ~/R library.
# Run this ONCE on the SCC login node before submitting run_dgam.sh:
#   bash install_mvgam.sh
#
# The rocker-neon4cast container binds your home directory by default,
# so packages install into ~/R/... and persist across jobs.

cd /projectnb/dietzelab/cwebb16/FRP/Forecast_submissions

mkdir -p /projectnb3/dietzelab/cwebb16/tmp
export TMPDIR=/projectnb3/dietzelab/cwebb16/tmp

singularity exec \
  --bind /projectnb/dietzelab/cwebb16 \
  ~/rocker-neon4cast_latest.sif \
  R -e '
    if (!requireNamespace("mvgam", quietly = TRUE)) {
      install.packages("mvgam", repos = "https://cloud.r-project.org")
    }
    if (!requireNamespace("cmdstanr", quietly = TRUE)) {
      install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", "https://cloud.r-project.org"))
    }
    cmdstanr::install_cmdstan()
    library(mvgam)
    cat("mvgam version:", as.character(packageVersion("mvgam")), "\n")
    cat("cmdstan path:", cmdstanr::cmdstan_path(), "\n")
  '
