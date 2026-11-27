# 06_figures.R
# Three README figures: which counties, which months, and why the air-health link is confounded.
#
# Input  : output/tables/county_exceedance_days_2025.csv   (03_analysis.R)
#          output/tables/monthly_exceedance_days_2025.csv  (03_analysis.R)
#          output/tables/county_air_health_2025.csv        (05_health_compare.R)
#          output/tables/air_health_correlations_2025.csv  (05_health_compare.R)
#          data/clean/tx_pm25_daily_2025.csv.gz            (02_clean.R, for the March note only)
#          data/raw/download_manifest.csv, places_manifest.csv (data versions for the captions)
# Output : output/figures/fig1_county_exceedance_days_2025.png
#          output/figures/fig2_monthly_exceedance_days_2025.png
#          output/figures/fig3_ozone_copd_smoking_2025.png
#          The tables above are each figure's "table view": every plotted value is in a CSV.
#
# Design choices (dataviz skill):
#   * Color = identity, fixed per pollutant in every figure: ozone blue, PM2.5 orange
#     (reference palette slots 1-2, light mode; both pass the colorblind checks as a pair).
#   * One y-axis per panel, never two scales on one plot. Fig 3 uses three side-by-side panels.
#   * Thin marks, hairline grid, labels only where they carry the story (top 5, peaks, metros).
#   * Text is ink (black/gray), never the series color.
#
# Run from the project root (open the .Rproj in RStudio, then Source this file).

library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)   # combines the fig 3 panels
library(ggrepel)     # county labels that don't overlap points

if (!file.exists("Texas_Air_Polution2025.Rproj")) {
  stop("Run this from the project root (the folder with the .Rproj file).")
}

year      <- 2025
table_dir <- file.path("output", "tables")
fig_dir   <- file.path("output", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

in_path <- function(name) {
  path <- file.path(table_dir, sprintf(name, year))
  if (!file.exists(path)) stop("Missing ", path, " - run scripts 03 and 05 first.")
  path
}

# ---- Look: palette + theme --------------------------------------------------
col_pollutant <- c(ozone = "#2a78d6", pm25 = "#eb6834")
lab_pollutant <- c(ozone = "Ozone (8-hour)", pm25 = "PM2.5 (24-hour)")
ink <- c(surface = "#fcfcfb", primary = "#0b0b0b", secondary = "#52514e",
         grid = "#e1e0d9", axis = "#c3c2b7")

theme_project <- function() {
  theme_minimal(base_size = 11) +
    theme(plot.background   = element_rect(fill = ink[["surface"]], colour = NA),
          panel.background  = element_rect(fill = ink[["surface"]], colour = NA),
          panel.grid.minor  = element_blank(),
          panel.grid.major  = element_line(colour = ink[["grid"]], linewidth = 0.3),
          text              = element_text(colour = ink[["primary"]]),
          axis.text         = element_text(colour = ink[["secondary"]]),
          axis.title        = element_text(colour = ink[["secondary"]], size = 10),
          plot.title        = element_text(face = "bold", size = 13),
          plot.subtitle     = element_text(colour = ink[["secondary"]], lineheight = 1.15),
          plot.caption      = element_text(colour = ink[["secondary"]], size = 8, hjust = 0,
                                           lineheight = 1.15),
          plot.title.position   = "plot",
          plot.caption.position = "plot",
          legend.position   = "top",
          legend.justification = "left",
          legend.title      = element_blank())
}

save_fig <- function(plot, file, width, height) {
  path <- file.path(fig_dir, file)
  ggsave(path, plot, width = width, height = height, dpi = 200,
         device = ragg::agg_png, bg = ink[["surface"]])
  message("Saved ", path)
}

# Typographic minus sign (\u2212) so negative numbers read cleanly. Symbols are written as \u escapes
# (rho \u03c1, dagger \u2020) so they render whatever the R session's text encoding.
fmt <- function(x) sub("^-", "\u2212", sprintf("%.2f", x))

# ---- Data versions for the captions ----------------------------------------
epa    <- read_csv(file.path("data", "raw", "download_manifest.csv"), show_col_types = FALSE)
places <- read_csv(file.path("data", "raw", "places_manifest.csv"), show_col_types = FALSE)
# "Mon, 13 Jul 2026 15:47:43 GMT" -> "2026-07-13". English month names whatever the computer's
# language; the two files differ by a second, so dedupe the date.
epa_version  <- unique(format(parse_date(substr(epa$epa_last_modified, 6, 16), "%d %b %Y",
                                         locale = locale("en"))))
data_through <- format(max(as.Date(epa$data_through)))
stopifnot(length(epa_version) == 1, !is.na(epa_version), nrow(places) == 1)

src_air    <- sprintf("Source: EPA AirData daily files (version %s), data %d-01-01 to %s.",
                      epa_version, year, data_through)
src_health <- sprintf("CDC PLACES county data (CDC update %s), age-adjusted %% of adults 18+, %s estimates.",
                      places$cdc_rows_updated, places$estimate_year)
def_exceed <- "Exceedance day = a day when at least one monitor in the county was above AQI 100."

# ---- Figure 1: ranked counties ---------------------------------------------
county <- read_csv(in_path("county_exceedance_days_%d.csv"), show_col_types = FALSE,
                   col_types = cols(pollutant = "c", county_fips = "c", county_name = "c",
                                    limited_coverage = "l", .default = col_guess()))

monitored <- count(county, pollutant, name = "monitored")

fig1_data <- county |>
  filter(exceed_days > 0) |>
  # Ties: limited-coverage counties last (their share is unreliable), then by share as in 03.
  arrange(pollutant, desc(exceed_days), limited_coverage, desc(exceed_days / observed_days)) |>
  mutate(rank  = min_rank(desc(exceed_days)), .by = pollutant) |>   # ties share a rank (PM2.5 #5 is a 4-way tie)
  mutate(label = paste0(county_name, if_else(limited_coverage, "\u2020", ""),
                        " (", sites, if_else(sites == 1, " site", " sites"), ")"),
         key   = paste(pollutant, county_fips, sep = "__"))   # unique per panel (Tarrant is in both)

x_max <- max(fig1_data$exceed_days)

bar_panel <- function(pol) {
  d <- filter(fig1_data, pollutant == pol)
  n <- monitored$monitored[monitored$pollutant == pol]
  ggplot(d, aes(exceed_days, factor(key, levels = rev(key)))) +
    geom_col(fill = col_pollutant[[pol]], width = 0.72) +
    geom_text(data = filter(d, rank <= 5), aes(label = exceed_days),   # label the top 5 (+ ties) only
              hjust = -0.3, size = 3.2, colour = ink[["primary"]]) +
    scale_y_discrete(labels = \(k) d$label[match(k, d$key)]) +        # look up each label by key
    scale_x_continuous(limits = c(0, x_max * 1.08), expand = expansion(mult = c(0, 0)),
                       breaks = seq(0, 40, 10)) +                        # same x scale both panels
    labs(subtitle = sprintf("%s: %d of %d monitored counties had at least one day",
                            lab_pollutant[[pol]], nrow(d), n),
         x = if (pol == "pm25") "Days above AQI 100" else NULL, y = NULL) +
    theme_project() +
    theme(panel.grid.major.y = element_blank(),
          plot.subtitle = element_text(face = "bold", colour = ink[["primary"]]))
}

# All limited-coverage counties, including any with 0 days (hidden from the bars but still counted).
limited_note <- county |> filter(limited_coverage) |>
  summarise(txt = paste0(county_name, " (", sub(" .*", "", lab_pollutant[pollutant]), ", ",
                         coverage_reason, ", ", observed_days, " days observed): ", exceed_days,
                         if_else(exceed_days == 1, " day", " days"),
                         if_else(exceed_days == 0, ", not shown", ""), collapse = ";\n   ")) |>   # one per line
  pull(txt)

fig1 <- (bar_panel("ozone") / bar_panel("pm25")) +
  plot_layout(heights = c(sum(fig1_data$pollutant == "ozone"), sum(fig1_data$pollutant == "pm25"))) +
  plot_annotation(
    title    = sprintf("Exceedance days by Texas county, %d", year),
    subtitle = paste("Days above AQI 100 (unhealthy for sensitive groups). Counties with more monitoring sites have",
                     "\nmore chances to record one, so each name shows its number of sites. Counties with zero days are not shown."),
    caption  = paste0(def_exceed, " Not a violation of the federal standard (those use 3-year averages).\n",
                      if (length(limited_note) && nzchar(limited_note))
                        paste0("\u2020 Limited coverage, may undercount: ", limited_note, ".\n") else "",
                      src_air),
    theme    = theme_project())

save_fig(fig1, sprintf("fig1_county_exceedance_days_%d.png", year), width = 8, height = 9.5)

# ---- Figure 2: months -------------------------------------------------------
monthly <- read_csv(in_path("monthly_exceedance_days_%d.csv"), show_col_types = FALSE,
                    col_types = cols(pollutant = "c", .default = col_integer()))
stopifnot(nrow(monthly) == 24, setequal(monthly$month, 1:12))

peaks <- monthly |>
  slice_max(county_exceed_days, n = 1, by = pollutant, with_ties = FALSE) |>
  mutate(txt = sprintf("%s peak: %s, %d county-days", sub(" .*", "", lab_pollutant[pollutant]),
                       month.abb[month], county_exceed_days))

# How much of March's PM2.5 total came from the two-day event on Mar 14-15 (county-day grain, as in 03).
pm25_daily <- read_csv(file.path("data", "clean", sprintf("tx_pm25_daily_%d.csv.gz", year)),
                       show_col_types = FALSE,
                       col_types = cols_only(county_fips = "c", date = "D", exceed = "l"))
mar_event <- pm25_daily |>
  summarise(exceed = any(exceed), .by = c(county_fips, date)) |>
  filter(exceed, date %in% as.Date(sprintf("%d-03-%d", year, 14:15))) |>
  nrow()
mar_total <- monthly$county_exceed_days[monthly$pollutant == "pm25" & monthly$month == 3]

dodge <- position_dodge(width = 0.15)    # nudge the two series apart so 0-0 months don't hide a point

fig2 <- ggplot(monthly, aes(month, county_exceed_days, colour = pollutant, fill = pollutant)) +
  geom_hline(yintercept = 0, colour = ink[["axis"]], linewidth = 0.4) +
  geom_line(linewidth = 0.8, position = dodge) +
  geom_point(shape = 21, colour = ink[["surface"]], stroke = 0.6, size = 2.8, position = dodge) +  # surface ring
  geom_text(data = peaks, aes(label = txt), vjust = -1.1, size = 3.3, colour = ink[["primary"]]) +
  scale_colour_manual(values = col_pollutant, labels = lab_pollutant) +
  scale_fill_manual(values = col_pollutant, labels = lab_pollutant) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  labs(title    = sprintf("When Texas counties had exceedance days, %d", year),
       subtitle = sprintf("County-days above AQI 100 per month (each county counts once per day). Monitored counties: ozone %d, PM2.5 %d",
                          monitored$monitored[monitored$pollutant == "ozone"],
                          monitored$monitored[monitored$pollutant == "pm25"]),
       x = NULL, y = "County-days",
       caption  = paste0(def_exceed, " Includes days EPA may flag as dust or wildfire events.\n",
                         sprintf("PM2.5: %d of March's %d county-days fell on just two days, Mar 14-15.\n",
                                 mar_event, mar_total),
                         src_air)) +
  theme_project() +
  theme(panel.grid.major.x = element_blank())

save_fig(fig2, sprintf("fig2_monthly_exceedance_days_%d.png", year), width = 8, height = 4.8)

# ---- Figure 3: the confounding picture (ozone) -----------------------------
county_health <- read_csv(in_path("county_air_health_%d.csv"), show_col_types = FALSE,
                          col_types = cols(county_fips = "c", county_name = "c",
                                           ozone_limited_coverage = "l", pm25_limited_coverage = "l",
                                           .default = col_guess()))
correlations  <- read_csv(in_path("air_health_correlations_%d.csv"), show_col_types = FALSE)

# Same counties as the ozone tests in 05: monitors ozone, full-year coverage.
oz <- county_health |> filter(!is.na(ozone_exceed_days), !ozone_limited_coverage %in% TRUE)
# Label the five largest counties plus the high-ozone points that drive the correlation (Collin, Denton).
metros <- oz |> filter(rank(-adults_18plus) <= 5 | ozone_exceed_days >= 18)

# Same CI method as 05 (Bonett-Wright SE on the Fisher z scale).
spearman_ci <- function(rho, n) tanh(atanh(rho) + c(-1, 1) * qnorm(0.975) * sqrt((1 + rho^2 / 2) / (n - 3)))

ozone_result <- function(health) {
  r <- correlations |> filter(pollutant == "ozone", air_metric == "exceed_days", health == !!health)
  stopifnot(nrow(r) == 1, r$n == nrow(oz))
  r
}

rho_text <- function(health) {
  r <- ozone_result(health)
  # Cross-check: recompute from the county table, so the figure can't drift from the CSV.
  recheck <- cor(oz$ozone_exceed_days, oz[[paste0(health, "_pct")]], method = "spearman")
  stopifnot(abs(recheck - r$rho) < 0.006)
  sprintf("Spearman \u03c1 = %s (95%% CI %s to %s)", fmt(r$rho), fmt(r$ci_low), fmt(r$ci_high))
}

copd_adj   <- ozone_result("copd | adults")
asthma_adj <- ozone_result("asthma | adults")
stopifnot(!copd_adj$clear_association, !asthma_adj$clear_association)   # the subtitle below says so

rho_adults <- cor(oz$ozone_exceed_days, oz$adults_18plus, method = "spearman")
ci_adults  <- spearman_ci(rho_adults, nrow(oz))

scatter_panel <- function(y, title, subtitle, ylab, log_y = FALSE) {
  p <- ggplot(oz, aes(ozone_exceed_days, .data[[y]])) +
    geom_point(shape = 21, fill = col_pollutant[["ozone"]], colour = ink[["surface"]],
               stroke = 0.6, size = 2.8, alpha = 0.75) +      # see-through: stacked points look darker
    geom_text_repel(data = metros, aes(label = county_name), size = 3, colour = ink[["secondary"]],
                    seed = 1, min.segment.length = 0, segment.colour = ink[["axis"]],
                    box.padding = 0.4) +
    labs(title = title, subtitle = subtitle, x = "Ozone exceedance days", y = ylab) +
    theme_project() +
    theme(plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 9))
  if (log_y) p <- p + scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale()))
  p
}

fig3 <- scatter_panel("copd_pct", "COPD", rho_text("copd"), "% of adults (age-adjusted)") +
  scatter_panel("smoking_pct", "Smoking (negative control)", rho_text("smoking"), "% of adults (age-adjusted)") +
  scatter_panel("adults_18plus", "County size",
                sprintf("Spearman \u03c1 = %s (95%% CI %s to %s)", fmt(rho_adults), fmt(ci_adults[1]), fmt(ci_adults[2])),
                "Adults 18+ (log scale)", log_y = TRUE) +
  plot_annotation(
    title    = "Counties with more ozone days have less COPD, and also less smoking",
    subtitle = paste0("Ozone can't make people smoke less, so both links point to a third factor: ",
                      "the big metro counties have the most ozone days\nand lower COPD and smoking rates. ",
                      sprintf("Adjusted for county size, no clear link was detected (COPD \u03c1 %s, 95%% CI %s to %s; asthma \u03c1 %s, not shown).",
                              fmt(copd_adj$rho), fmt(copd_adj$ci_low), fmt(copd_adj$ci_high), fmt(asthma_adj$rho))),
    caption  = paste0(sprintf("%d Texas counties with full-year ozone monitoring. ", nrow(oz)),
                      "County-level comparison: says nothing about individuals. Health estimates are 2023, air data is 2025.\n",
                      src_air, " ", src_health),
    theme    = theme_project())

save_fig(fig3, sprintf("fig3_ozone_copd_smoking_%d.png", year), width = 11, height = 4.8)
