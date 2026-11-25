# 01_download.R
# Download EPA AirData daily summary files for 2025 and keep Texas only.
#
# Source : EPA AirData pre-generated files, https://aqs.epa.gov/aqsweb/airdata/download_files.html
# Grain  : one row per monitor (County Code + Site Num + POC) x Date Local x Sample Duration
#          x Pollutant Standard x Event Type.
#          NOTE: on exceptional-event days (wildfire smoke, Saharan dust) EPA publishes BOTH an
#          "Events Included" and an "Events Excluded" row. 02_clean.R must pick one Event Type
#          before counting exceedance days, or those days are double-counted.
#          PM2.5 also has a "1 HOUR" row (no Pollutant Standard) next to the daily "24-HR BLK AVG"
#          / "24 HOUR" row for the same monitor-day. Keep Pollutant Standard == "PM25 24-hour 2012"
#          for daily values. (2025 Texas check: all Event Type = "None"; ozone is 1 row/monitor-day.)
# Output : data/raw/large/tx_daily_<parameter>_<year>.csv   (gitignored, rebuild with this script)
#          data/raw/download_manifest.csv                   (committed: what was downloaded, when)
#
# Run from the project root (open Texas_Air_Polution2025.Rproj in RStudio, or `Rscript scripts/01_download.R`).
# If a later file fails, earlier Texas CSVs are already rewritten but the manifest is not:
# just rerun the script once the problem is fixed.

library(readr)
library(dplyr)

if (!file.exists("Texas_Air_Polution2025.Rproj")) {
  stop("Run this from the project root (the folder with the .Rproj file).")
}

# ---- Settings ---------------------------------------------------------------
year        <- 2025
state_code  <- "48"                                # Texas FIPS code
parameters  <- c(pm25 = "88101", ozone = "44201")  # AQS parameter codes
base_url    <- "https://aqs.epa.gov/aqsweb/airdata"
out_dir     <- file.path("data", "raw", "large")
redownload  <- FALSE                               # TRUE to fetch a fresh copy from EPA

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
old_opts <- options(timeout = 600)                 # default 60 s is too short on slow links;
                                                   # restored at the end of the script

# Key that should identify one row (see Grain above); used for the duplicate check.
grain_cols <- c("County Code", "Site Num", "POC", "Date Local",
                "Sample Duration", "Pollutant Standard", "Event Type")

# ---- Download, filter to Texas, save ---------------------------------------
manifest <- list()

for (name in names(parameters)) {
  code     <- parameters[[name]]
  zip_name <- sprintf("daily_%s_%d.zip", code, year)
  zip_path <- file.path(out_dir, zip_name)
  url      <- paste(base_url, zip_name, sep = "/")

  # EPA's own "last updated" date for this file = which version of the data we have.
  hdr <- curlGetHeaders(url)
  epa_last_modified <- sub("(?i)^last-modified:\\s*", "",
                           trimws(grep("(?i)^last-modified:", hdr, value = TRUE, perl = TRUE)[1]),
                           perl = TRUE)

  if (redownload || !file.exists(zip_path)) {
    message("Downloading ", url)
    # Download to a temp file and only keep it if it opens as a zip,
    # so a cut-off download never gets reused on the next run.
    tmp <- tempfile(fileext = ".zip")
    download.file(url, tmp, mode = "wb")           # mode = "wb": binary-safe on every OS
    stopifnot(nrow(unzip(tmp, list = TRUE)) == 1)
    file.copy(tmp, zip_path, overwrite = TRUE)
    unlink(tmp)
  } else {
    message("Using existing ", zip_path, " (set redownload <- TRUE for a fresh copy)")
  }

  # The zip should hold exactly one national CSV.
  csv_name <- unzip(zip_path, list = TRUE)$Name
  stopifnot(length(csv_name) == 1, grepl("\\.csv$", csv_name))

  # All columns as text: keeps codes like "48" and "001" from losing leading zeros.
  national <- read_csv(unz(zip_path, csv_name),
                       col_types = cols(.default = col_character()),
                       progress  = FALSE)

  missing_cols <- setdiff(c("State Code", "Parameter Code", "Local Site Name", "Address", grain_cols),
                          names(national))
  if (length(missing_cols) > 0) stop("Columns missing from ", zip_name, ": ",
                                     paste(missing_cols, collapse = ", "))

  texas <- national |> filter(`State Code` == state_code)

  # Fail loudly if the layout changed or the wrong file came back.
  if (nrow(texas) == 0) stop("No Texas rows in ", zip_name)
  stopifnot(all(texas$`Parameter Code` == code))

  tx_path <- file.path(out_dir, sprintf("tx_daily_%s_%d.csv", code, year))
  write_csv(texas, tx_path)

  dates <- as.Date(texas$`Date Local`, format = "%Y-%m-%d")
  if (anyNA(dates)) stop(sum(is.na(dates)), " Texas rows have an unreadable Date Local in ", zip_name)

  # Last reporting date per site: shows whether data_through holds for the whole state.
  site_last <- texas |>
    mutate(date = dates) |>
    summarise(last_date = max(date), .by = c(`County Code`, `Site Num`))

  manifest[[name]] <- tibble(
    pollutant            = name,
    parameter_code       = code,
    source_url           = url,
    epa_last_modified    = epa_last_modified,
    downloaded_on        = format(file.mtime(zip_path), "%Y-%m-%d"),  # local date; as.Date() would use UTC
    zip_md5              = unname(tools::md5sum(zip_path)),  # changes if EPA republishes
    national_rows        = nrow(national),
    texas_rows           = nrow(texas),                     # file rows, NOT monitor-days
    texas_duplicate_keys = nrow(texas) - nrow(distinct(texas, across(all_of(grain_cols)))),
    texas_counties       = n_distinct(texas$`County Code`),
    texas_sites          = n_distinct(texas$`County Code`, texas$`Site Num`),
    texas_monitors       = n_distinct(texas$`County Code`, texas$`Site Num`, texas$POC),
    first_date           = as.character(min(dates)),
    data_through         = as.character(max(dates)),        # latest date at ANY site
    sites_through_dec    = sum(site_last$last_date >= as.Date(sprintf("%d-12-01", year))),
    earliest_site_last   = as.character(min(site_last$last_date)),
    all_text_utf8        = all(validUTF8(c(texas$`Local Site Name`, texas$Address)))
  )

  rm(national)                                     # free memory before the next file
}

manifest <- bind_rows(manifest)
write_csv(manifest, file.path("data", "raw", "download_manifest.csv"))
print(as.data.frame(manifest))                     # data.frame prints every column

options(old_opts)
