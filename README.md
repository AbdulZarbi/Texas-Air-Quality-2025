# Texas Air Quality 2025

> Work in progress. Which Texas counties exceeded PM2.5 and ozone standards most often in 2025, and how does that line up with asthma and COPD burden?

## Data

| File | Source | Coverage | License |
|---|---|---|---|
| `data/raw/ad_viz_plotval_data.csv` | [EPA AirData — Download Daily Data](https://www.epa.gov/outdoor-air-quality-data/download-daily-data) | Daily PM2.5 (parameter 88101), site 484530014 *Austin North Hills Drive*, Travis County, 2025 | U.S. government work, public domain |

Download parameters: pollutant = PM2.5, year = 2025, state = Texas, site = Austin North Hills Drive. (Planned: all Texas monitors, plus ozone.)

Large downloads go in `data/raw/large/`, which is gitignored. Re-download them from the EPA link above using the parameters listed.

## Repository layout

```
data/raw/        original downloads, unedited
data/clean/      cleaned tables produced by scripts
scripts/         numbered R / SQL steps (01_download, 02_clean, ...)
output/figures/  charts
output/tables/   summary tables
reports/         Quarto write-up
```

## Status

- [x] Project set up and on GitHub
- [x] Repo structure (`data/`, `scripts/`, `output/`, `reports/`)
- [ ] Download all Texas monitors; add CDC PLACES county health estimates
- [ ] Analysis and Tableau Public map

## Tools

R · MySQL · Tableau Public

## License

Code: [MIT](LICENSE). EPA data: U.S. government work, public domain.
