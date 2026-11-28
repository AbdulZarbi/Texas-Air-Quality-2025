# 07_tableau_export.R
# One map-ready table for Tableau Public: every Texas county (254) x pollutant (2) = 508 rows.
#
# Input  : output/tables/county_exceedance_days_2025.csv   (03_analysis.R: monitored counties only)
#          data/raw/places_tx_county_2025release.csv       (04_download_places.R: all 254 counties)
# Output : output/tables/tableau_county_map_2025.csv
#
# Why a separate table (not county_air_health_2025.csv):
#   * That file has only the 42 monitored counties. On a map the other 212 would be blank, and a
#     blank county looks like clean air. Here they are kept and labeled monitor_status = "No monitor"
#     (exceed_days = NA, not 0: no monitor means no measurement, not zero bad days).
#   * Long format (one row per county x pollutant) so a single `pollutant` field drives the
#     Tableau filter / tabs.
#   * county_fips is the map key: Tableau matches it to its built-in county shapes (county names can
#     be ambiguous, FIPS is not). A CSV has no column types, so in Tableau set county_fips to String,
#     then Geographic Role > County. All Texas codes are 5 digits starting with 48 (no leading zeros).
#   * Two rows per county: in Tableau keep `pollutant` a single-value filter, or SUMs add ozone and
#     PM2.5 together (and double total_population).
#
# Columns (one row = one county x pollutant):
#   county_fips, county_name, state     5-digit FIPS; use county_fips as the Tableau County role
#   pollutant / pollutant_label         "ozone" / "pm25" and display labels "Ozone" / "PM2.5"
#   monitor_status                      Monitored | Limited coverage (see coverage_reason) | No monitor
#   sites, observed_days, exceed_days, exceed_pct, max_value, max_value_date   from 03 (NA if no monitor)
#   exceed_band, band_order             color bands 0 / 1-4 / 5-9 / 10-19 / 20+ days / No monitor
#   max_units                           ppm (8-hour) for ozone, ug/m3 (24-hour) for PM2.5 (plain ASCII
#                                       so Tableau can't garble the text encoding)
#   asthma_pct, copd_pct, smoking_pct, adults_18plus, total_population   CDC PLACES
# Health columns are the same AGE-ADJUSTED CDC PLACES values (adults 18+, 2023 estimates) used in 05.
# Loving County's PLACES values are suppressed by CDC (population 43, under its 50 cut-off) and stay NA.
# The CSV carries no source/date fields: put them in the Tableau caption (EPA AirData file version
# 2026-07-13, data through 2025-12-31; CDC PLACES 2025 release, 2023 age-adjusted, adults 18+).
#
# Run from the project root (open the .Rproj in RStudio, then Source this file).

library(readr)
library(dplyr)
library(tidyr)

if (!file.exists("Texas_Air_Polution2025.Rproj")) {
  stop("Run this from the project root (the folder with the .Rproj file).")
}

year        <- 2025
table_dir   <- file.path("output", "tables")
air_path    <- file.path(table_dir, sprintf("county_exceedance_days_%d.csv", year))
places_path <- file.path("data", "raw", "places_tx_county_2025release.csv")
out_path    <- file.path(table_dir, sprintf("tableau_county_map_%d.csv", year))
for (p in c(air_path, places_path)) if (!file.exists(p)) stop("Missing ", p, " - run scripts 03 and 04 first.")

# ---- All 254 counties + age-adjusted health (from PLACES) -------------------
places <- read_csv(places_path, show_col_types = FALSE,
                   col_types = cols(.default = "c", data_value = "d",
                                    totalpopulation = "d", totalpop18plus = "d"))

counties <- places |>
  distinct(county_fips = locationid, county_name = locationname,
           total_population = totalpopulation, adults_18plus = totalpop18plus)
stopifnot(nrow(counties) == 254, !anyDuplicated(counties$county_fips))   # one row per county

health <- places |>
  filter(datavaluetypeid == "AgeAdjPrv") |>
  mutate(measure = case_match(measureid, "CASTHMA" ~ "asthma_pct", "COPD" ~ "copd_pct",
                              "CSMOKING" ~ "smoking_pct")) |>
  select(county_fips = locationid, measure, data_value) |>
  pivot_wider(names_from = measure, values_from = data_value)

# ---- Air (monitored counties only, from 03) ---------------------------------
air <- read_csv(air_path, show_col_types = FALSE,
                col_types = cols(pollutant = "c", county_fips = "c", county_name = "c",
                                 first_date = "D", last_date = "D", max_value_date = "D",
                                 limited_coverage = "l", coverage_reason = "c",
                                 .default = col_double())) |>
  select(-county_name)                                  # names come from PLACES for all 254
stopifnot(all(air$county_fips %in% counties$county_fips))                # every monitor county matches

# ---- County x pollutant grid: fill unmonitored counties ---------------------
map <- counties |>
  cross_join(tibble(pollutant = c("ozone", "pm25"))) |>  # 254 x 2; one row even with no monitor
  left_join(air,    by = c("county_fips", "pollutant"), relationship = "one-to-one") |>
  left_join(health, by = "county_fips",                 relationship = "many-to-one") |>
  mutate(
    pollutant_label = if_else(pollutant == "ozone", "Ozone", "PM2.5"),
    monitor_status  = case_when(is.na(sites)       ~ "No monitor",
                                limited_coverage   ~ "Limited coverage",
                                TRUE               ~ "Monitored"),
    # Discrete color bands. band_order lets Tableau sort the legend (0 days first, No monitor last).
    exceed_band = case_when(is.na(exceed_days) ~ "No monitor",
                            exceed_days == 0   ~ "0 days",
                            exceed_days <= 4   ~ "1-4 days",
                            exceed_days <= 9   ~ "5-9 days",
                            exceed_days <= 19  ~ "10-19 days",
                            TRUE               ~ "20+ days"),
    band_order  = match(exceed_band, c("0 days", "1-4 days", "5-9 days", "10-19 days",
                                       "20+ days", "No monitor")),
    max_units   = case_when(is.na(sites)        ~ NA_character_,
                            pollutant == "ozone" ~ "ppm (8-hour)",
                            TRUE                 ~ "ug/m3 (24-hour)"),
    state       = "Texas"
  ) |>
  select(county_fips, county_name, state, pollutant, pollutant_label, monitor_status,
         sites, observed_days, exceed_days, exceed_pct, exceed_band, band_order,
         max_value, max_units, max_value_date, coverage_reason,
         asthma_pct, copd_pct, smoking_pct, adults_18plus, total_population) |>
  arrange(pollutant, county_fips)

# ---- Checks: the map must reproduce 03's totals -----------------------------
stopifnot(
  nrow(map) == 254 * 2,
  !anyDuplicated(map[c("county_fips", "pollutant")]),
  !anyNA(map$band_order),
  # monitored counties and exceedance totals unchanged from 03 (labels checked first, so a
  # renamed pollutant can't slip through as "no monitor" everywhere)
  all(air$pollutant %in% c("ozone", "pm25")),
  sum(map$monitor_status != "No monitor") == nrow(air),
  sum(map$exceed_days, na.rm = TRUE) == sum(air$exceed_days),
  # "No monitor" means the same thing in every column, and monitored rows are complete
  identical(is.na(map$exceed_days), map$monitor_status == "No monitor"),
  !anyNA(map[map$monitor_status != "No monitor", c("observed_days", "max_value", "max_value_date")]),
  # health is missing only where CDC suppressed it (all three measures)
  all(map$county_name[!complete.cases(map[c("asthma_pct", "copd_pct", "smoking_pct")])] == "Loving")
)

write_csv(map, out_path, na = "")                       # empty cells read as NULL in Tableau

# ---- Short report -----------------------------------------------------------
cat("\nWrote", out_path, "-", nrow(map), "rows (", n_distinct(map$county_fips), "counties x 2 pollutants )\n")
cat("\n== Counties by monitor status ==\n")
print(as.data.frame(count(map, pollutant_label, monitor_status)))
cat("\n== Counties by color band (check against the Tableau legend) ==\n")
print(as.data.frame(map |> count(pollutant_label, band_order, exceed_band) |> select(-band_order)))
cat("\n== Exceedance county-days (should match 03: ozone 190, PM2.5 64) ==\n")
print(as.data.frame(map |> group_by(pollutant_label) |>
  summarise(county_days = sum(exceed_days, na.rm = TRUE), top_county = county_name[which.max(exceed_days)],
            top_days = max(exceed_days, na.rm = TRUE))))
