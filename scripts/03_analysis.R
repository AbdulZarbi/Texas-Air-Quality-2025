# 03_analysis.R
# Which Texas counties had the most bad-air days in 2025, and when in the year did they happen?
#
# Input  : data/clean/tx_pm25_daily_2025.csv.gz, tx_ozone_daily_2025.csv.gz (from 02_clean.R)
# Output : output/tables/county_exceedance_days_2025.csv   one row per county x pollutant
#          output/tables/monthly_exceedance_days_2025.csv  one row per month x pollutant
#
# Definitions (cite these in the README):
#   Exceedance day = a day with AQI above 100 at a monitor (PM2.5 >= 35.5 ug/m3, ozone >= 0.071 ppm),
#                    as flagged in 02_clean.R.
#   County-day     = one county on one date. A county-day "exceeds" if ANY monitor in the county
#                    exceeded that day. This is the unit we count, so a site with two monitors, or a
#                    county with ten sites, still counts each calendar day at most once.
#   Observed days  = county-days with at least one valid reading. Counties with filter samplers only
#                    (1-in-3 / 1-in-6 days) or monitors that started/stopped mid-year have fewer
#                    observed days and will UNDERCOUNT exceedances. They are kept, but flagged:
#                    limited_coverage = observed_days < 75% of the year (274 of 365 days).
#   exceed_pct     = exceed_days / observed_days. Headline ranking uses exceed_days; exceed_pct is
#                    secondary and misleading for limited_coverage counties (see coverage_reason).
#   coverage_reason = why a county is limited: "filter sampler schedule" (all monitors are 1-in-3 /
#                    1-in-6 filter samplers, method 145: undercounts) or "partial year" (monitor
#                    started/stopped mid-year: share can be inflated or deflated by season).
#
# Wording for the README (from r-reviewer, 2026-11-26):
#   * "days when at least one monitor in the county was above AQI 100" - counties with more sites
#     have more chances to exceed, so always show `sites` next to the count.
#   * "monitored counties" (29 PM2.5, 34 ozone of Texas's 254), not "Texas counties".
#   * An AQI > 100 day is NOT a violation of the federal standard (violations use 3-year design
#     values). Say "exceedance day" / "unhealthy-for-sensitive-groups day".
#   * PM2.5 days are event-driven (Mar 14-15 dust; El Paso Feb-Mar) and are included.
#
# Run from the project root (open the .Rproj in RStudio, then Source this file).

library(readr)
library(dplyr)

if (!file.exists("Texas_Air_Polution2025.Rproj")) {
  stop("Run this from the project root (the folder with the .Rproj file).")
}

year       <- 2025
clean_dir  <- file.path("data", "clean")
table_dir  <- file.path("output", "tables")

days_in_year     <- as.integer(as.Date(sprintf("%d-12-31", year)) - as.Date(sprintf("%d-01-01", year))) + 1L
coverage_cutoff  <- ceiling(0.75 * days_in_year)   # 274 days

read_clean <- function(file) {
  path <- file.path(clean_dir, file)
  if (!file.exists(path)) stop("Missing ", path, " - run scripts/02_clean.R first.")
  # Explicit types for the columns we use: readr would guess method_code as a number for PM2.5
  # ("145") but text for ozone ("047"), and bind_rows() refuses to stack the two.
  read_csv(path, show_col_types = FALSE,
           col_types = cols(site_id = "c", county_fips = "c", county_name = "c", poc = "i",
                            date = "D", exceed = "l", method_code = "c", .default = col_guess()))
}

# Keep the same 9 columns from both files, then stack, so every step below runs once for both.
keep_cols <- c("pollutant", "county_fips", "county_name", "site_id", "poc", "date",
               "value", "exceed", "method_code")
pm25  <- read_clean(sprintf("tx_pm25_daily_%d.csv.gz", year)) |>
  mutate(pollutant = "pm25", value = pm25_ugm3) |> select(all_of(keep_cols))
ozone <- read_clean(sprintf("tx_ozone_daily_%d.csv.gz", year)) |>
  mutate(pollutant = "ozone", value = ozone_ppm) |> select(all_of(keep_cols))
daily <- bind_rows(pm25, ozone)

stopifnot(!anyNA(daily$exceed), !anyNA(daily$value), nchar(daily$county_fips) == 5)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Collapse monitor-days -> county-days -----------------------------------
# One step does both collapses (monitors at a site, sites in a county): any exceed, max value.
county_day <- daily |>
  summarise(exceed    = any(exceed),
            max_value = max(value),
            .by = c(pollutant, county_fips, county_name, date))

# Grain check: exactly one row per pollutant x county x date.
stopifnot(nrow(county_day) == nrow(distinct(county_day, pollutant, county_fips, date)))

# ---- County table -----------------------------------------------------------
# Site/monitor facts per county, from the monitor-level data (not county-days).
county_sites <- daily |>
  summarise(sites               = n_distinct(site_id),             # sites reporting at any time in the year
            all_filter_samplers = all(method_code == "145"),        # 1-in-3 / 1-in-6 schedule only
            .by = c(pollutant, county_fips))

county_table <- county_day |>
  summarise(
    observed_days    = n(),
    first_date       = min(date),
    last_date        = max(date),
    exceed_days      = sum(exceed),
    exceed_share     = exceed_days / observed_days,                 # unrounded, used to break ties
    # Must come BEFORE max_value: summarise() overwrites max_value with a single number once it runs.
    max_value_date   = min(date[max_value == max(max_value)]),      # earliest date of the county max
    max_value        = max(max_value),
    .by = c(pollutant, county_fips, county_name)
  ) |>
  left_join(county_sites, by = c("pollutant", "county_fips"), relationship = "one-to-one") |>
  mutate(
    exceed_pct       = round(100 * exceed_share, 1),
    limited_coverage = observed_days < coverage_cutoff,
    coverage_reason  = case_when(
      !limited_coverage   ~ NA_character_,
      all_filter_samplers ~ "filter sampler schedule",
      TRUE                ~ "partial year"
    )
  ) |>
  arrange(pollutant, desc(exceed_days), desc(exceed_share)) |>
  select(pollutant, county_fips, county_name, sites, observed_days, first_date, last_date,
         exceed_days, exceed_pct, max_value, max_value_date, limited_coverage, coverage_reason)

# ---- Monthly table ----------------------------------------------------------
monthly_table <- county_day |>
  mutate(month = as.integer(format(date, "%m"))) |>
  summarise(
    county_exceed_days = sum(exceed),                    # county-days over the standard
    statewide_days     = n_distinct(date[exceed]),       # calendar days with >= 1 county over
    counties_affected  = n_distinct(county_fips[exceed]),
    .by = c(pollutant, month)
  ) |>
  arrange(pollutant, month)

# Every month should appear for both pollutants (12 x 2 rows).
stopifnot(nrow(monthly_table) == 24)

# ---- Save + print a short report -------------------------------------------
write_csv(county_table,  file.path(table_dir, sprintf("county_exceedance_days_%d.csv", year)))
write_csv(monthly_table, file.path(table_dir, sprintf("monthly_exceedance_days_%d.csv", year)))

statewide <- county_day |>
  summarise(counties            = n_distinct(county_fips),
            counties_any_exceed = n_distinct(county_fips[exceed]),
            county_exceed_days  = sum(exceed),
            statewide_days      = n_distinct(date[exceed]),
            .by = pollutant)

cat("\n== Statewide, 2025 ==\n");             print(as.data.frame(statewide))
cat("\n== Top 5 counties per pollutant (ties at #5 all shown) ==\n")
print(as.data.frame(county_table |>
  slice_max(exceed_days, n = 5, by = pollutant, with_ties = TRUE) |>
  select(pollutant, county_name, sites, observed_days, exceed_days, exceed_pct,
         max_value, max_value_date, limited_coverage)))
cat("\n== Limited-coverage counties ==\n")
print(as.data.frame(county_table |> filter(limited_coverage) |>
  select(pollutant, county_name, observed_days, first_date, last_date, exceed_days, coverage_reason)))
cat("\n== Worst month per pollutant ==\n")
print(as.data.frame(monthly_table |> slice_max(county_exceed_days, n = 1, by = pollutant, with_ties = TRUE)))
