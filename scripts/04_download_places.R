# 04_download_places.R
# Download CDC PLACES county estimates of adult asthma, COPD and smoking for Texas.
#
# Source : CDC PLACES: Local Data for Better Health, County Data, 2025 release
#          https://data.cdc.gov/d/swc5-untb   (public domain)
#          Estimates are for 2023 (model-based from BRFSS survey data), not measured counts.
# Grain  : one row per county x measure x value type
#          measures: CASTHMA (current asthma, adults 18+), COPD (adults 18+),
#                    CSMOKING (current smoking, adults 18+: main driver of COPD, needed as context)
#          value types: AgeAdjPrv (age-adjusted %, use to COMPARE counties), CrdPrv (crude %;
#                    crude x totalpop18plus = estimated number of adults)
# Output : data/raw/places_tx_county_2025release.csv  (small, ~250 KB: committed)
#          data/raw/places_manifest.csv               (what was downloaded, when)
#
# Run from the project root (open the .Rproj in RStudio, then Source this file).
#
# Limitations to state in the README (from r-reviewer, 2026-11-26):
#   * Health estimates are 2023, air data is 2025: a comparison between PLACES, not over time.
#   * PLACES values are model-based small-area estimates (BRFSS survey + demographics), not
#     counted cases; small counties lean on demographics. Keep the confidence limits.
#   * Adults only: children (most ozone-sensitive) are not covered.
#   * Suppressed: Loving County (48301, pop. ~43) for all measures - it has no monitor anyway.

library(readr)
library(dplyr)

if (!file.exists("Texas_Air_Polution2025.Rproj")) {
  stop("Run this from the project root (the folder with the .Rproj file).")
}

# ---- Settings ---------------------------------------------------------------
dataset_id  <- "swc5-untb"                    # PLACES County Data, 2025 release
state       <- "TX"
measures    <- c("CASTHMA", "COPD", "CSMOKING")
value_types <- c("AgeAdjPrv", "CrdPrv")
n_counties  <- 254                            # Texas has 254 counties
out_path    <- file.path("data", "raw", "places_tx_county_2025release.csv")

# Columns to keep (the API has 22; geolocation etc. are not needed).
keep <- c("year", "stateabbr", "locationid", "locationname", "measureid", "measure",
          "datavaluetypeid", "data_value_type", "data_value", "low_confidence_limit",
          "high_confidence_limit", "data_value_footnote", "totalpopulation", "totalpop18plus")

# ---- Build the query (Socrata SoQL) ----------------------------------------
# $where filters on CDC's server, so only the Texas rows we need are sent (~1,500 of ~229,000).
where <- sprintf("stateabbr='%s' AND measureid in(%s)",
                 state, paste0("'", measures, "'", collapse = ","))
query <- paste0(
  "$select=", paste(keep, collapse = ","),
  "&$where=", where,
  "&$order=locationid,measureid,datavaluetypeid",   # stable row order -> clean git diffs
  "&$limit=5000"                                    # API default is only 1,000 rows
)
url <- paste0("https://data.cdc.gov/resource/", dataset_id, ".csv?",
              utils::URLencode(query, reserved = FALSE))

# ---- Download (to temp first, keep only if it reads correctly) -------------
tmp <- tempfile(fileext = ".csv")
message("Downloading CDC PLACES (", dataset_id, ") for ", state, ": ", paste(measures, collapse = ", "))
download.file(url, tmp, mode = "wb", quiet = TRUE)

# Explicit types: text by default, numbers only where we expect them. A guessed type could
# silently turn a column into NA if CDC changes the file.
places <- read_csv(tmp, show_col_types = FALSE,
                   col_types = cols(.default = "c", data_value = "d",
                                    low_confidence_limit = "d", high_confidence_limit = "d",
                                    totalpopulation = "d", totalpop18plus = "d"))

# ---- Checks: stop rather than save something wrong -------------------------
expected_rows <- n_counties * length(measures) * length(value_types)
if (nrow(places) != expected_rows) stop("Expected ", expected_rows, " rows, got ", nrow(places))
stopifnot(
  nrow(problems(places)) == 0,                              # nothing failed to parse
  setequal(names(places), keep),
  setequal(unique(places$measureid), measures),
  setequal(unique(places$datavaluetypeid), value_types),
  n_distinct(places$locationid) == n_counties,
  all(nchar(places$locationid) == 5), all(startsWith(places$locationid, "48")),  # Texas FIPS
  n_distinct(places$year) == 1,                             # one estimate year
  all(between(places$data_value, 0, 100), na.rm = TRUE),    # percentages
  nrow(distinct(places, locationid, measureid, datavaluetypeid)) == nrow(places)  # grain
)

# Fetch CDC's metadata BEFORE saving, so the data file and manifest are always updated together.
meta <- jsonlite::fromJSON(paste0("https://data.cdc.gov/api/views/", dataset_id, ".json"))

stopifnot(file.copy(tmp, out_path, overwrite = TRUE))      # file.copy returns FALSE, not an error
unlink(tmp)

# ---- Manifest: version + what's in the file --------------------------------
manifest <- tibble(
  source          = meta$name,
  dataset_id      = dataset_id,
  source_url      = url,
  cdc_rows_updated = format(as.POSIXct(meta$rowsUpdatedAt, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"),
  license         = meta$license$name,
  downloaded_on   = format(Sys.Date(), "%Y-%m-%d"),
  estimate_year   = unique(places$year),
  counties        = n_distinct(places$locationid),
  rows            = nrow(places),
  suppressed_values = sum(is.na(places$data_value)),      # CDC withholds counties with pop. < 50
  file_md5        = unname(tools::md5sum(out_path))
)
write_csv(manifest, file.path("data", "raw", "places_manifest.csv"))
print(as.data.frame(manifest))

cat("\nAge-adjusted prevalence (%), Texas counties:\n")
print(as.data.frame(places |>
  filter(datavaluetypeid == "AgeAdjPrv") |>
  summarise(counties = n(), suppressed = sum(is.na(data_value)),
            min = min(data_value, na.rm = TRUE), median = median(data_value, na.rm = TRUE),
            max = max(data_value, na.rm = TRUE), .by = measureid)))
