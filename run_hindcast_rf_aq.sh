#!/bin/bash -l
#$ -P dietzelab
#$ -l buyin
#$ -l h_rt=12:00:00
#$ -l mem_per_core=16G
#$ -M cwebb16@bu.edu
#$ -m be
#$ -j y
#$ -o /projectnb/dietzelab/cwebb16/FRP/Forecast_submissions/hindcast_rf_aq.log

cd /projectnb/dietzelab/cwebb16/FRP/Forecast_submissions

export OSN_KEY="84erZCYP4-87cVRQ"
export OSN_SECRET="K3et80GjvZxSw7ZehcYiRr-BMgjsTN-l"
export AWS_ACCESS_KEY_ID="84erZCYP4-87cVRQ"
export AWS_SECRET_ACCESS_KEY="K3et80GjvZxSw7ZehcYiRr-BMgjsTN-l"

# Install ranger and daymetr into local lib if not already present
singularity exec \
  --bind /projectnb/dietzelab/cwebb16 \
  ~/rocker-neon4cast_latest.sif \
  Rscript -e '
    lib <- "/projectnb/dietzelab/cwebb16/x86_64-pc-linux-gnu-library/4.5/"
    for (pkg in c("ranger", "daymetr")) {
      if (!requireNamespace(pkg, quietly=TRUE))
        install.packages(pkg, lib=lib)
    }
  '

singularity exec \
  --bind /projectnb/dietzelab/cwebb16 \
  ~/rocker-neon4cast_latest.sif \
  Rscript ./Generate_forecasts/tg_randfor/hindcast_rf_aq_daymet.R
