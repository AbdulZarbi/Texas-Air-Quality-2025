# 02_clean.R
# Turn the raw Texas EPA daily files into one tidy row per monitor per day.
#
# Input  : data/raw/large/tx_daily_88101_2025.csv (PM2.5), tx_daily_44201_2025.csv (ozone)
#          -> made by scripts/01_download.R
# Output : data/clean/tx_pm25_daily_2025.csv.gz   one row per monitor (site + POC) x day
#          data/clean/tx_ozone_daily_2025.csv.gz  one row per monitor (site + POC) x day
#          data/clean/tx_monitor_summary_2025.csv one row per monitor: coverage + exceedance days
#          (daily files are gzipped: ~0.3 MB instead of ~6 MB in git; read_csv() reads .gz directly)
#
# Decisions (documented so 03_analysis.R and the README can cite them):
#   PM2.5 daily value = `Arithmetic Mean` of rows with Pollutant Standard "PM25 24-hour 2012"
#         (drops the 1-HOUR rows, which have no standard). Units: ug/m3.
#   Ozone daily value = `1st Max Value` = the day's highest 8-hour average (the 8-hour standard
#         applies to the daily max, not the daily mean). Units: ppm.
#   Exceedance day    = AQI above 100 ("Unhealthy for Sensitive Groups"):
#         PM2.5 >= 35.5 ug/m3 (24-hour standard 35), ozone >= 0.071 ppm (8-hour standard 0.070).
#         The script checks these thresholds agree with EPA's own AQI column.
#   Event Type        = keep "None" and "Events Included" (wildfire/dust days count: the health
#         impact is real). "Events Excluded" rows are regulatory duplicates and are dropped.
#   Completeness      = NOT filtered here. Filter samplers (PM2.5 method 145) run every 3rd/6th
#         day by design, so a "% of 365 days" rule would wrongly drop them. Coverage is reported
#         in the monitor summary; 03_analysis.R decides what to exclude.
#   Ozone day_complete = Observation Percent >= 75. EPA still counts an incomplete day if it
#         exceeds, so 03_analysis.R should count: exceed == TRUE (any completeness).
#   Negative PM2.5    = kept. Continuous monitors can report small negatives near zero; EPA
#         keeps them. Counted in the monitor summary.
#   PM2.5 obs_pct     = set to NA. It is 100 on every 24-hour row; real hourly completeness
#         lives on the dropped 1-HOUR rows. (EPA keeps short PM2.5 days anyway.)
#
# Notes for 03_analysis.R (from r-reviewer, 2026-11-24):
#   * 12 PM2.5 sites have >1 monitor (POC). Collapse to site-day (any exceed / max value) before
#     ranking counties: 113 exceedance monitor-days = 93 exceedance site-days.
#   * Top 12 PM2.5 days are all El Paso, Mar-Apr 2025 (max 288.5 ug/m3, Chamizal filter sampler,
#     2025-04-01, AQI 426): likely high-wind dust. Not flagged as an exceptional event (yet).
#   * pct_of_year counts mid-year starts and 1-in-3/1-in-6 filter samplers as "low coverage";
#     use pct_of_active_period + method_codes to tell them apart.
#
# Run from the project root: Rscript scripts/02_clean.R

library(readr)
library(dplyr)

if (!file.exists("Texas_Air_Polution2025.Rproj")) {
  stop("Run this from the project root (the folder with the .Rproj file).")
}

year       <- 2025
raw_dir    <- file.path("data", "raw", "large")
clean_dir  <- file.path("data", "clean")
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

read_raw <- function(code) {
  path <- file.path(raw_dir, sprintf("tx_daily_%s_%d.csv", code, year))
  if (!file.exists(path)) stop("Missing ", path, " - run scripts/01_download.R first.")
  read_csv(path, col_types = cols(.default = col_character()), progress = FALSE)
}

# Shared tidy-up: readable names, proper types, a single site ID.
tidy_daily <- function(df, value_col) {
  df |>
    filter(`Event Type` %in% c("None", "Events Included")) |>
    transmute(
      site_id     = paste0(`State Code`, `County Code`, `Site Num`),  # 9-digit AQS site ID
      poc         = as.integer(POC),                                  # monitor number at the site
      county_fips = paste0(`State Code`, `County Code`),              # 5-digit, joins to CDC PLACES
      county_name = `County Name`,
      site_name   = `Local Site Name`,
      latitude    = as.numeric(Latitude),
      longitude   = as.numeric(Longitude),
      date        = as.Date(`Date Local`, format = "%Y-%m-%d"),
      value       = as.numeric(.data[[value_col]]),
      aqi         = as.integer(AQI),
      obs_pct     = as.numeric(`Observation Percent`),
      sample_duration = `Sample Duration`,
      method_code = `Method Code`,
      event_type  = `Event Type`
    )
}

# Stop if anything breaks the one-row-per-monitor-day grain or has missing values.
check_clean <- function(df, label) {
  stopifnot("exceed" %in% names(df), "aqi" %in% names(df))   # else the AQI check silently compares nothing
  dup <- nrow(df) - nrow(distinct(df, site_id, poc, date))
  if (dup > 0) stop(label, ": ", dup, " duplicate monitor-days")
  na_cols <- names(df)[colSums(is.na(select(df, site_id, poc, date, value, exceed))) > 0]
  if (length(na_cols) > 0) stop(label, ": NA values in ", paste(na_cols, collapse = ", "))
  # Our threshold must agree with EPA's AQI (rows where EPA gives an AQI).
  has_aqi <- !is.na(df$aqi)
  if (sum(has_aqi) == 0) stop(label, ": no AQI values to cross-check against")
  mismatch <- sum(df$exceed[has_aqi] != (df$aqi[has_aqi] > 100))
  if (mismatch > 0) stop(label, ": ", mismatch, " days where exceed disagrees with AQI > 100")
  message(label, ": ", nrow(df), " monitor-days OK; AQI cross-check on ", sum(has_aqi), " rows, 0 mismatches")
  invisible(df)
}

# ---- PM2.5 ------------------------------------------------------------------
pm25 <- read_raw("88101") |>
  # 1-HOUR rows have Pollutant Standard = NA, so this keeps only the daily rows.
  filter(`Pollutant Standard` == "PM25 24-hour 2012") |>
  tidy_daily("Arithmetic Mean") |>
  rename(pm25_ugm3 = value) |>
  mutate(exceed = pm25_ugm3 >= 35.5,
         obs_pct = NA_real_) |>                    # meaningless for PM2.5 daily rows (see header)
  arrange(site_id, poc, date)

check_clean(rename(pm25, value = pm25_ugm3), "PM2.5")

# ---- Ozone ------------------------------------------------------------------
ozone <- read_raw("44201") |>
  filter(`Pollutant Standard` == "Ozone 8-hour 2015") |>
  tidy_daily("1st Max Value") |>
  rename(ozone_ppm = value) |>
  mutate(exceed = ozone_ppm >= 0.071,
         day_complete = obs_pct >= 75) |>
  arrange(site_id, poc, date)

check_clean(rename(ozone, value = ozone_ppm), "Ozone")
if (anyNA(ozone$day_complete)) stop("Ozone: NA Observation Percent, so day_complete is NA")

# ---- Monitor summary --------------------------------------------------------
days_in_year <- as.integer(as.Date(sprintf("%d-12-31", year)) - as.Date(sprintf("%d-01-01", year))) + 1L

summarise_monitors <- function(df, value_col, pollutant) {
  df |>
    summarise(
      pollutant     = pollutant,
      county_fips   = first(county_fips),
      county_name   = first(county_name),
      site_name     = first(site_name),
      method_codes  = paste(sort(unique(method_code)), collapse = ";"),
      first_date    = min(date),
      last_date     = max(date),
      days_reported = n(),
      pct_of_year   = round(100 * n() / days_in_year, 1),
      # Share of days between this monitor's first and last report (mid-year starts look fine here;
      # 1-in-3 / 1-in-6 filter samplers still show ~33% / ~17% by design).
      pct_of_active_period = round(100 * n() / (as.integer(max(date) - min(date)) + 1L), 1),
      exceed_days   = sum(exceed),
      max_value     = max(.data[[value_col]]),
      negative_days = sum(.data[[value_col]] < 0),
      .by = c(site_id, poc)
    )
}

monitors <- bind_rows(
  summarise_monitors(pm25,  "pm25_ugm3", "pm25"),
  summarise_monitors(ozone, "ozone_ppm", "ozone")
) |> arrange(pollutant, site_id, poc)

# ---- Save + print a short report -------------------------------------------
# A .gz file name makes readr compress automatically.
write_csv(pm25,     file.path(clean_dir, sprintf("tx_pm25_daily_%d.csv.gz", year)))
write_csv(ozone,    file.path(clean_dir, sprintf("tx_ozone_daily_%d.csv.gz", year)))
write_csv(monitors, file.path(clean_dir, sprintf("tx_monitor_summary_%d.csv", year)))

report <- monitors |>
  summarise(monitors = n(), sites = n_distinct(site_id), counties = n_distinct(county_fips),
            monitor_days = sum(days_reported), exceed_monitor_days = sum(exceed_days),
            monitors_under_75pct_of_year = sum(pct_of_year < 75),
            negative_days = sum(negative_days), max_value = max(max_value),
            .by = pollutant)
print(as.data.frame(report))
cat("Ozone days with obs_pct < 75:", sum(!ozone$day_complete),
    "| of those exceeding:", sum(!ozone$day_complete & ozone$exceed), "\n")
