# Hindcast O3 (May-Sept) and NO2_P1H (full year) for 2006-2009
# for 8 Boston urban AQ sites using a fresh RF trained on Daymet + EPA AQS targets.
#
# Workflow:
#   1. Download Daymet for 8 sites, 2006-2021
#   2. Load urban AQ targets (O3, NO2_P1H) from S3, 2010-2021; aggregate hourly -> daily mean
#   3. Per site × variable: tune + fit RF on 2010-2021, predict 2006-2009
#   4. Write one EFI-format CSV per date into OUTPUT_DIR
#
# Run from repo root: Rscript ./Generate_forecasts/tg_randfor/hindcast_rf_aq_daymet.R

library(tidyverse)
library(lubridate)
library(arrow)
library(ranger)
library(daymetr)
library(yaml)

# ---- Config -----------------------------------------------------------------
config <- yaml::read_yaml("Generate_forecasts/config.yaml")

OUTPUT_DIR <- "/projectnb/dietzelab/cwebb16/FRP/Urban/Tree/data/raw/aq_hindcasts"
MODEL_ID   <- "tg_randfor"

HINDCAST_YEARS <- 2006:2009
TRAIN_YEARS    <- 2010:2021
GROW_MONTHS    <- 5:9   # May-Sept for O3

DAYMET_VARS <- c("tmax", "tmin", "tmean", "prcp", "srad", "vp", "dayl")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Site metadata ----------------------------------------------------------
site_meta <- tibble::tribble(
  ~site_id,       ~lat,       ~lon,
  "25-021-3003",  42.211774, -71.113970,
  "25-025-0002",  42.348873, -71.097163,
  "25-025-0040",  42.340251, -71.038350,
  "25-025-0042",  42.329500, -71.082600,
  "25-025-0043",  42.363100, -71.054300,
  "25-025-0044",  42.325186, -71.056061,
  "25-025-0045",  42.349954, -71.059194,
  "25-025-1004",  42.387222, -71.026111
)

# ---- 1. Download Daymet (2006-2021) -----------------------------------------
message("Downloading Daymet for ", nrow(site_meta), " sites (2006-2021)...")

daymet_all <- purrr::map_dfr(seq_len(nrow(site_meta)), function(i) {
  message("  ", site_meta$site_id[i])
  dm <- daymetr::download_daymet(
    site     = paste0("site_", i),   # daymetr needs a name, not used further
    lat      = site_meta$lat[i],
    lon      = site_meta$lon[i],
    start    = min(HINDCAST_YEARS),
    end      = max(TRAIN_YEARS),
    internal = TRUE,
    silent   = TRUE
  )$data

  dm |>
    dplyr::mutate(
      site_id = site_meta$site_id[i],
      datetime = as.Date(paste(year, yday), "%Y %j"),
      tmax  = `tmax..deg.c.`,
      tmin  = `tmin..deg.c.`,
      tmean = (tmax + tmin) / 2,
      prcp  = `prcp..mm.day.`,
      srad  = `srad..W.m.2.`,
      vp    = `vp..Pa.`,
      dayl  = `dayl..s.`
    ) |>
    dplyr::select(site_id, datetime, all_of(DAYMET_VARS))
})

daymet_train    <- daymet_all |> dplyr::filter(year(datetime) %in% TRAIN_YEARS)
daymet_hindcast <- daymet_all |> dplyr::filter(year(datetime) %in% HINDCAST_YEARS)
message("Daymet ready: ", nrow(daymet_train), " training rows, ",
        nrow(daymet_hindcast), " hindcast rows.")

# ---- 2. Load AQ targets from S3 and aggregate to daily ----------------------
message("Loading urban AQ targets from S3...")
s3_read <- arrow::s3_bucket(
  config$s3_bucket_read,
  endpoint_override = config$endpoint,
  access_key        = Sys.getenv("OSN_KEY"),
  secret_key        = Sys.getenv("OSN_SECRET"),
  scheme            = "https"
)

target_daily <- arrow::read_csv_arrow(
  s3_read$path(paste0(config$targets_path, "/urban-targets.csv"))
) |>
  dplyr::filter(
    site_id  %in% site_meta$site_id,
    variable %in% c("O3", "NO2_P1H")
  ) |>
  dplyr::mutate(
    datetime    = as.Date(datetime),
    observation = suppressWarnings(as.numeric(observation))
  ) |>
  dplyr::filter(year(datetime) %in% TRAIN_YEARS) |>
  dplyr::group_by(datetime, site_id, variable) |>
  dplyr::summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop")

message("Targets ready: ", nrow(target_daily), " site-day-variable rows.")

# ---- Helper: tune + fit a ranger RF ------------------------------------------
fit_rf <- function(train_data, var, available_met) {
  p          <- length(available_met)
  mtry_grid  <- unique(pmax(1L, c(floor(sqrt(p)), floor(p / 3L), floor(p / 2L))))
  min_n_grid <- c(5L, 10L, 20L)
  rf_formula <- as.formula(paste(var, "~ ."))

  best_oob <- Inf; best_mtry <- mtry_grid[1]; best_min_n <- min_n_grid[1]
  for (m in mtry_grid) {
    for (mn in min_n_grid) {
      fit <- ranger::ranger(rf_formula, data = train_data,
                            num.trees = 200, mtry = m, min.node.size = mn)
      if (fit$prediction.error < best_oob) {
        best_oob <- fit$prediction.error; best_mtry <- m; best_min_n <- mn
      }
    }
  }
  list(
    model    = ranger::ranger(rf_formula, data = train_data, num.trees = 500,
                              mtry = best_mtry, min.node.size = best_min_n),
    best_oob = best_oob, best_mtry = best_mtry, best_min_n = best_min_n
  )
}

# ---- 3. Train RF per site × variable; predict 2006-2009 ---------------------
var_specs <- list(
  O3      = list(pred_months = GROW_MONTHS),   # May-Sept only
  NO2_P1H = list(pred_months = 1:12)           # full year
)

available_met <- intersect(DAYMET_VARS, names(daymet_train))
all_hindcasts <- list()

for (var in names(var_specs)) {
  pred_months <- var_specs[[var]]$pred_months

  hindcast_dates <- daymet_hindcast |>
    dplyr::filter(month(datetime) %in% pred_months) |>
    dplyr::pull(datetime) |>
    unique() |>
    sort()

  message("\n== Variable: ", var, " | ", length(hindcast_dates), " hindcast days ==")

  sites_no_data <- character(0)

  # ---- Pass 1: per-site models for sites with their own training data --------
  for (site in site_meta$site_id) {

    site_target <- target_daily |>
      dplyr::filter(site_id == site, variable == var) |>
      tidyr::pivot_wider(names_from = variable, values_from = observation)

    if (!var %in% names(site_target)) {
      sites_no_data <- c(sites_no_data, site)
      next
    }

    train_data <- site_target |>
      dplyr::left_join(
        daymet_train |>
          dplyr::filter(site_id == site) |>
          dplyr::select(datetime, all_of(available_met)),
        by = "datetime"
      ) |>
      dplyr::select(all_of(c(var, available_met))) |>
      tidyr::drop_na()

    if (nrow(train_data) < 10) {
      message("  ", site, " [per-site]: only ", nrow(train_data), " rows — deferring to pooled.")
      sites_no_data <- c(sites_no_data, site)
      next
    }

    rf <- fit_rf(train_data, var, available_met)
    message("  ", site, " [per-site]: mtry=", rf$best_mtry, " min.node.size=", rf$best_min_n,
            " OOB_MSE=", round(rf$best_oob, 4), " n_train=", nrow(train_data))

    pred_met <- daymet_hindcast |>
      dplyr::filter(site_id == site, datetime %in% hindcast_dates) |>
      dplyr::select(datetime, all_of(available_met)) |>
      tidyr::drop_na()

    if (nrow(pred_met) == 0) { message("  ", site, ": no hindcast met — skipping."); next }

    preds <- pmax(0, predict(rf$model,
                             data = pred_met |> dplyr::select(all_of(available_met)))$predictions)

    all_hindcasts[[paste(var, site, sep = "-")]] <- tibble::tibble(
      reference_datetime = pred_met$datetime,
      datetime           = format(as.POSIXct(pred_met$datetime, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      site_id            = site,
      depth              = 1L,
      family             = "ensemble",
      parameter          = 0L,
      obs_flag           = 0L,
      variable           = var,
      prediction         = preds
    )
  }

  # ---- Pass 2: pooled model for sites with no training data ------------------
  if (length(sites_no_data) > 0) {
    message("  Building pooled model for ", length(sites_no_data), " sites without data: ",
            paste(sites_no_data, collapse = ", "))

    sites_with_data <- setdiff(site_meta$site_id, sites_no_data)

    pooled_train <- purrr::map_dfr(sites_with_data, function(s) {
      st <- target_daily |>
        dplyr::filter(site_id == s, variable == var) |>
        tidyr::pivot_wider(names_from = variable, values_from = observation)
      if (!var %in% names(st)) return(NULL)
      st |>
        dplyr::left_join(
          daymet_train |>
            dplyr::filter(site_id == s) |>
            dplyr::select(datetime, all_of(available_met)),
          by = "datetime"
        ) |>
        dplyr::select(all_of(c(var, available_met))) |>
        tidyr::drop_na()
    })

    if (nrow(pooled_train) < 10) {
      message("  Not enough pooled training data for ", var, " — skipping missing sites.")
    } else {
      rf_pooled <- fit_rf(pooled_train, var, available_met)
      message("  [pooled]: mtry=", rf_pooled$best_mtry, " min.node.size=", rf_pooled$best_min_n,
              " OOB_MSE=", round(rf_pooled$best_oob, 4), " n_train=", nrow(pooled_train))

      for (site in sites_no_data) {
        pred_met <- daymet_hindcast |>
          dplyr::filter(site_id == site, datetime %in% hindcast_dates) |>
          dplyr::select(datetime, all_of(available_met)) |>
          tidyr::drop_na()

        if (nrow(pred_met) == 0) { message("  ", site, ": no hindcast met — skipping."); next }

        preds <- pmax(0, predict(rf_pooled$model,
                                 data = pred_met |> dplyr::select(all_of(available_met)))$predictions)

        all_hindcasts[[paste(var, site, sep = "-")]] <- tibble::tibble(
          reference_datetime = pred_met$datetime,
          datetime           = format(as.POSIXct(pred_met$datetime, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
          site_id            = site,
          depth              = 1L,
          family             = "ensemble",
          parameter          = 0L,
          obs_flag           = 0L,
          variable           = var,
          prediction         = preds
        )
        message("  ", site, " [pooled]: predictions added.")
      }
    }
  }
}

hindcasts_all <- dplyr::bind_rows(all_hindcasts)
message("\nTotal hindcast rows: ", nrow(hindcasts_all))

# ---- 4. Write one EFI CSV per date ------------------------------------------
all_dates <- sort(unique(as.Date(hindcasts_all$reference_datetime)))
message("Writing ", length(all_dates), " files to ", OUTPUT_DIR, "...")

for (d in as.character(all_dates)) {
  day_data <- hindcasts_all |>
    dplyr::filter(as.character(reference_datetime) == d) |>
    dplyr::mutate(reference_datetime = as.Date(reference_datetime)) |>
    dplyr::select(reference_datetime, datetime, site_id,
                  depth, family, parameter, obs_flag, variable, prediction)

  fname <- file.path(OUTPUT_DIR, paste0("urban-", d, "-", MODEL_ID, ".csv"))
  readr::write_csv(day_data, fname)
}

message("Done. ", length(all_dates), " hindcast files written.")
