# Texas Air Quality 2025

> Work in progress. Which Texas counties exceeded PM2.5 and ozone standards most often in 2025, and how does that line up with asthma and COPD burden?

## Data

| File | Source | Coverage | License |
|---|---|---|---|
| `ad_viz_plotval_data.csv` | [EPA AirData — Download Daily Data](https://www.epa.gov/outdoor-air-quality-data/download-daily-data) | Daily PM2.5 (parameter 88101), site 484530014 *Austin North Hills Drive*, Travis County, 2025 | U.S. government work, public domain |

Download parameters: pollutant = PM2.5, year = 2025, state = Texas, site = Austin North Hills Drive. (Planned: all Texas monitors, plus ozone.)

## Status

- [x] Project set up and on GitHub
- [ ] Repo structure (`data/`, `scripts/`, `output/`, `reports/`)
- [ ] Download all Texas monitors; add CDC PLACES county health estimates
- [ ] Analysis and Tableau Public map

## Tools

R · MySQL · Tableau Public
