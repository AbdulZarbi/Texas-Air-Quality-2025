# 05_health_compare.R
# Do monitored Texas counties with more bad-air days also have higher adult asthma or COPD rates?
#
# Input  : output/tables/county_exceedance_days_2025.csv   (03_analysis.R: county x pollutant)
#          data/raw/places_tx_county_2025release.csv       (04_download_places.R: CDC PLACES)
# Output : output/tables/county_air_health_2025.csv        one row per monitored county
#          output/tables/air_health_correlations_2025.csv  one row per pollutant x air metric x health test
#
# Method (cite in the README):
#   * Health = CDC PLACES AGE-ADJUSTED prevalence, adults 18+, 2023 estimates (2025 release).
#   * Air    = exceedance days per county, 2025 (days when >= 1 monitor was above AQI 100).
#              Main metric: exceed_days. Sensitivity: exceed_share (exceed days / observed days,
#              unrounded) - matters for PM2.5, where counties with 1-in-3/1-in-6 samplers have
#              fewer chances to record an exceedance.
#   * Counties flagged limited_coverage in 03_analysis.R are EXCLUDED from that pollutant's tests.
#   * Spearman rank correlation, p from cor.test(exact = FALSE) (many tied 0-day counties).
#     95% CI by Fisher z with the Bonett-Wright SE sqrt((1 + rho^2/2) / (n - 3)); approximate
#     under heavy ties (PM2.5: 13 of 27 counties at 0 days) - a bootstrap gave similar intervals.
#   * Partial Spearman ("x | z"): rank x, y and z, remove the linear effect of rank(z) from both,
#     correlate the residuals. p from t with n - 3 df. Adjusted for smoking OR adult population.
#   * Multiple testing: Benjamini-Hochberg q across the main family (all exceed_days tests).
#
# How to read the results (r-reviewer, 2026-11-26):
#   * The raw correlations are NEGATIVE and are confounding, not a protective effect: ozone days
#     correlate with SMOKING as strongly as with COPD, and air pollution does not make people
#     smoke less. Big metro counties have more monitors and more ozone days (any-monitor rule)
#     and younger, lower-smoking populations. See the confounder checks printed at the end.
#   * "copd | smoking" is NOT interpretable: across these counties COPD and smoking rank almost
#     identically (rho ~0.95), so nothing is left to correlate. Flagged in the `note` column.
#   * Adjusted results move with the adjustment choice, and n is 27-34 with model-based outcomes:
#     conclude "no clear association detected", never "no association".
#   * Ecological comparison: county patterns say nothing about individuals. Health is 2023, air 2025.
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
for (p in c(air_path, places_path)) if (!file.exists(p)) stop("Missing ", p, " - run scripts 03 and 04 first.")

# ---- Health: one row per county, age-adjusted % ----------------------------
places <- read_csv(places_path, show_col_types = FALSE,
                   col_types = cols(.default = "c", data_value = "d", low_confidence_limit = "d",
                                    high_confidence_limit = "d", totalpop18plus = "d"))

health <- places |>
  filter(datavaluetypeid == "AgeAdjPrv") |>
  mutate(measure = case_match(measureid, "CASTHMA" ~ "asthma", "COPD" ~ "copd", "CSMOKING" ~ "smoking")) |>
  select(county_fips = locationid, measure, pct = data_value,
         lcl = low_confidence_limit, ucl = high_confidence_limit) |>
  pivot_wider(names_from = measure, values_from = c(pct, lcl, ucl), names_glue = "{measure}_{.value}")

adults <- places |> distinct(county_fips = locationid, adults_18plus = totalpop18plus)
stopifnot(nrow(adults) == n_distinct(places$locationid))           # one adult count per county
health <- health |> left_join(adults, by = "county_fips", relationship = "one-to-one")

# ---- Air: one row per county, columns per pollutant -------------------------
air_long <- read_csv(air_path, show_col_types = FALSE,
                     col_types = cols(county_fips = "c", pollutant = "c", county_name = "c",
                                      limited_coverage = "l", .default = col_guess()))

air <- air_long |>
  mutate(exceed_share = exceed_days / observed_days) |>            # unrounded (exceed_pct is rounded)
  select(pollutant, county_fips, county_name, sites, observed_days, exceed_days, exceed_share,
         limited_coverage) |>
  pivot_wider(names_from = pollutant,
              values_from = c(sites, observed_days, exceed_days, exceed_share, limited_coverage),
              names_glue = "{pollutant}_{.value}")

# ---- Join: monitored counties only ------------------------------------------
county <- air |>
  left_join(health, by = "county_fips", relationship = "one-to-one") |>
  arrange(county_fips)

no_health <- with(county, is.na(asthma_pct) | is.na(copd_pct) | is.na(smoking_pct))
if (any(no_health)) stop("Monitored county missing PLACES values: ",
                         paste(county$county_name[no_health], collapse = ", "))

# ---- Correlation helpers ----------------------------------------------------
spearman_ci <- function(rho, n, k = 0, level = 0.95) {
  # Bonett-Wright (2000) SE on the Fisher z scale; k = number of variables adjusted for.
  se <- sqrt((1 + rho^2 / 2) / (n - 3 - k))
  tanh(atanh(rho) + c(-1, 1) * qnorm(1 - (1 - level) / 2) * se)
}

partial_spearman <- function(x, y, z) {
  rx <- resid(lm(rank(x) ~ rank(z)))
  ry <- resid(lm(rank(y) ~ rank(z)))
  cor(rx, ry)                                     # Pearson on residual ranks
}

# Counties used for one pollutant: monitors it and has full-year coverage.
counties_for <- function(pollutant) {
  county[!is.na(county[[paste0(pollutant, "_exceed_days")]]) &
         !county[[paste0(pollutant, "_limited_coverage")]] %in% TRUE, ]
}

# One test for one row of the grid. A plain function (not rowwise()) so the column names are
# ordinary strings - nested dplyr verbs inside rowwise() can't see the grid's columns.
run_correlation <- function(pollutant, air_metric, health) {
  d <- counties_for(pollutant)
  x <- d[[paste0(pollutant, "_", air_metric)]]
  parts  <- strsplit(health, " | ", fixed = TRUE)[[1]]     # e.g. "copd | smoking"
  y      <- d[[paste0(parts[1], "_pct")]]
  n      <- nrow(d)
  if (length(parts) == 1) {
    rho <- cor(x, y, method = "spearman")
    p   <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE)$p.value)  # ties warning
    k   <- 0
  } else {
    z   <- if (parts[2] == "adults") d$adults_18plus else d[[paste0(parts[2], "_pct")]]
    rho <- partial_spearman(x, y, z)
    k   <- 1
    t   <- rho * sqrt((n - 3) / (1 - rho^2))
    p   <- 2 * pt(-abs(t), df = n - 3)
  }
  ci <- spearman_ci(rho, n, k)
  tibble(n = n, counties_with_exceed = sum(x > 0), rho = rho, ci_low = ci[1], ci_high = ci[2], p = p)
}

grid <- expand_grid(pollutant  = c("pm25", "ozone"),
                    air_metric = c("exceed_days", "exceed_share"),
                    health     = c("asthma", "copd", "smoking",
                                   "asthma | smoking", "copd | smoking",
                                   "asthma | adults", "copd | adults"))

correlations <- grid |>
  mutate(res = purrr::pmap(list(pollutant, air_metric, health), run_correlation)) |>
  unnest(res) |>
  mutate(clear_association = sign(ci_low) == sign(ci_high))          # before rounding

# BH q-values within the main family (exceed_days); exceed_share is a robustness check, not new tests.
correlations <- correlations |>
  mutate(q_bh = if_else(air_metric == "exceed_days",
                        p.adjust(if_else(air_metric == "exceed_days", p, NA_real_), method = "BH"),
                        NA_real_),
         note = case_when(
           health == "copd | smoking" ~ "not interpretable: COPD and smoking rank almost identically",
           health == "smoking"        ~ "negative control: air cannot change smoking -> confounding",
           TRUE                       ~ NA_character_)) |>
  mutate(across(c(rho, ci_low, ci_high), \(v) round(v, 2)),
         across(c(p, q_bh), \(v) signif(v, 2)))

# ---- Confounder checks (why the raw correlations are negative) -------------
confounders <- bind_rows(lapply(c("pm25", "ozone"), function(pol) {
  d <- counties_for(pol)
  x <- d[[paste0(pol, "_exceed_days")]]
  tibble(pollutant = pol, n = nrow(d),
         exceed_vs_adults  = cor(x, d$adults_18plus, method = "spearman"),
         exceed_vs_sites   = cor(x, d[[paste0(pol, "_sites")]], method = "spearman"),
         adults_vs_copd    = cor(d$adults_18plus, d$copd_pct, method = "spearman"),
         copd_vs_smoking   = cor(d$copd_pct, d$smoking_pct, method = "spearman"))
})) |> mutate(across(where(is.double), \(v) round(v, 2)))

# ---- Save + print a short report -------------------------------------------
write_csv(county,       file.path(table_dir, sprintf("county_air_health_%d.csv", year)))
write_csv(correlations, file.path(table_dir, sprintf("air_health_correlations_%d.csv", year)))

epa <- read_csv(file.path("data", "raw", "download_manifest.csv"), show_col_types = FALSE)
cat("\nData: EPA AirData through", format(max(as.Date(epa$data_through))),
    "| CDC PLACES estimate year", unique(places$year), "\n")
cat("Monitored counties:", nrow(county),
    "| PM2.5 (full coverage):", nrow(counties_for("pm25")),
    "| ozone (full coverage):", nrow(counties_for("ozone")), "\n")

cat("\n== Main results: Spearman, exceed_days ==\n")
print(as.data.frame(correlations |> filter(air_metric == "exceed_days") |>
  select(pollutant, health, n, rho, ci_low, ci_high, p, q_bh, clear_association)))

cat("\n== Sensitivity: exceed_share gives the same picture? (rho) ==\n")
print(as.data.frame(correlations |>
  select(pollutant, health, air_metric, rho) |>
  pivot_wider(names_from = air_metric, values_from = rho)))

cat("\n== Confounder checks (Spearman rho) ==\n")
print(as.data.frame(confounders))
