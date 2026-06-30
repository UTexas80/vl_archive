# vl_archive — ValueLine Options Analysis

An R-based analytics platform for processing, analyzing, and modeling **ValueLine options data**. The project focuses on options strategies — primarily **butterfly spreads** and **iron condors** — by ingesting daily ValueLine export files, cleaning and structuring the data, identifying near-the-money strike prices, computing option Greeks, and building machine-learning models to rank opportunities.

---

## Table of Contents

- [Overview](#overview)
- [Key Technologies](#key-technologies)
- [Project Structure](#project-structure)
- [Data Flow & Processing Pipeline](#data-flow--processing-pipeline)
- [Key Data Objects](#key-data-objects)
- [Configuration](#configuration)
- [Options Strategies Covered](#options-strategies-covered)
- [Machine Learning](#machine-learning)
- [Reference & Documentation](#reference--documentation)

---

## Overview

`vl_archive` is a [ProjectTemplate](http://projecttemplate.net/)-based R project. Running `load.project()` (via `vl_archive.r`) automatically:

1. Loads all libraries declared in `config/global.dcf`
2. Sources helper and utility functions from `lib/`
3. Executes data-munging scripts in `munge/`
4. Makes the cleaned datasets available in the global environment

The project consumes **ValueLine weekly option screening ZIP files** stored in a Microsoft OneDrive folder, extracts the embedded CSVs, merges them into a single master data table (`dx_blob`), then derives strike-selection and butterfly-spread tables used for trade evaluation.

---

## Key Technologies

| Category | Packages / Tools |
|---|---|
| **Project framework** | `ProjectTemplate` |
| **Data manipulation** | `data.table`, `dplyr`, `dtplyr`, `tidyr`, `tidyverse` |
| **Financial data** | `quantmod` (live risk-free rate via FRED), `tidyquant`, `xts`, `zoo` |
| **Options pricing** | `RQuantLib`, `fOptions`, `ragtop` |
| **Machine learning** | `mlr3`, `mlr3verse`, `caret`, `parsnip`, `rsample` |
| **Visualization** | `ggplot2`, `highcharter`, `gganimate`, `heatmaply`, `treemap` |
| **Cloud / I/O** | `Microsoft365R` (OneDrive), `readr`, `readxl`, `openxlsx`, `writexl` |
| **Date / calendar** | `lubridate`, `timeDate`, `timetk` |
| **Utilities** | `here`, `janitor`, `skimr`, `DataExplorer`, `SmartEDA`, `pointblank` |
| **Reporting** | `rmarkdown`, `knitr`, `gt`, `DT`, `summarytools` |
| **Serialization** | `qs`, `fst` |

---

## Project Structure

```
vl_archive/
├── vl_archive.r            # Entry point — calls load.project()
├── vl_archive.Rproj        # RStudio project file
│
├── config/
│   ├── global.dcf          # ProjectTemplate settings: libraries, munging flags
│   └── globals.R           # Runtime globals: dates, OneDrive paths, risk-free rate
│
├── lib/
│   ├── helpers.R           # Utility functions (dirCheck, lsos, clean_names, …)
│   ├── globals.R           # Additional global configuration helpers
│   └── 000_fun_archive.r   # Legacy function archive
│
├── src/                    # Analysis scripts (sourced manually or via ProjectTemplate)
│   ├── 000-mungle.R        # Data ingestion and dx_blob construction
│   ├── 000_fun_archive.r   # Core processing functions (pipeline orchestration)
│   ├── 000-top200.R        # Top-200 calls/puts processing
│   ├── 002-StealthCurve.R  # Stealth Curve technical indicator analysis
│   ├── 030-A.R             # Strike price selection (nearest ±1/±2 strikes)
│   ├── 090-ml.r            # Machine learning model input preparation
│   ├── 090_fun_bfly.r      # Butterfly spread construction and cost calculation
│   ├── 099-Z.R             # Session cleanup / teardown
│   ├── 00_Ms365.R          # Microsoft 365 / OneDrive integration helpers
│   ├── eda.R               # Exploratory data analysis
│   └── nearest_strike.r    # Rolling-join strike-selection logic
│
├── munge/
│   ├── 000-main.R          # Main munge entry point — calls fun_0000_archive_main()
│   └── 999-Z.R             # End-of-munge cleanup
│
├── data/
│   ├── ALLNEW.CSV          # Raw ValueLine options export (primary input)
│   ├── Top200CallsBuy.csv  # Top-200 calls reference file
│   └── dt_archive.r        # OneDrive download helper for data files
│
├── ml/                     # Machine learning artefacts
│   ├── dt_bfly.qs          # Serialized butterfly dataset (qs format)
│   ├── dt_bfly_211130.xlsx # Butterfly training data snapshot
│   └── dx_3000.xlsx        # Strike processing snapshot
│
├── reports/
│   ├── dx_condor.roi.csv/xlsm   # Iron condor return-on-investment analysis
│   ├── dx_condor_roi_2.csv      # Extended condor ROI data
│   └── dx_condor_key.csv        # Condor key/lookup table
│
├── docs/
│   ├── dx_condor_file_layout.txt  # ALLNEW.CSV column layout reference
│   ├── readmeMenu.md              # dx_blob column name reference
│   ├── readmeMetaData.md          # Table metadata reference
│   └── 000_fun_archive.r          # Archived function reference
│
├── analysis/               # Excel what-if and spread analysis workbooks
├── xls/                    # Reference Excel files (Black-Scholes, CCALC, TRAKREC)
├── graphs/                 # Generated chart output
├── logs/                   # Application / run logs
├── cache/                  # ProjectTemplate object cache
├── profiling/              # Code profiling results
├── screener/               # Raw weekly screener text files
├── pdf/                    # Reference PDFs (Fidelity options guides)
├── sit/                    # System integration test scripts
├── tests/                  # Unit / regression tests
├── zip/                    # Temporary landing zone for downloaded ValueLine ZIPs
├── applescript/            # macOS automation scripts
└── renv/                   # renv lock file for reproducible package management
```

---

## Data Flow & Processing Pipeline

```
OneDrive (ValueLine ZIP files)
        │
        ▼
fun_1000_download_zip()         ← downloads & extracts ALLNEW.CSV from ZIP
        │
        ▼
fun_2000_archive_mungle()       ← parses ALLNEW.CSV → dx_blob (master options table)
  • Strips header rows, renames 54 columns
  • Converts percent strings and character fields to numeric
  • De-duplicates; assigns sequential Record.Number
  • Filters to next 3 monthly expiration dates
  • Builds dx_date_exp, dt_date_exp_mth, dx_company, dx_ticker, dx_industry
        │
        ▼
fun_3000_strike_processing()    ← identifies nearest strikes relative to current price
  • dx_s_minus_0/1/2  — in-the-money strikes (puts ≤ stock price)
  • dx_s_plus_0/1/2   — out-of-the-money strikes (calls ≥ stock price)
        │
        ▼
fun_4000_bfly_main()            ← constructs butterfly spread candidates (dt_bfly)
  • Links four legs: s_minus_1 (put), s_minus_0 (call), s_plus_0 (put), s_plus_1 (call)
  • Calculates net cost = -ASK(s−1) + BID(s−0) + BID(s+0) − ASK(s+1)
  • Appends Greeks: DLTA, GAMMA, THETA, VEGA, I/OTM, OI, PctDble, ROWRT
```

---

## Key Data Objects

| Object | Description |
|---|---|
| `dx_blob` | Master options data table (~55 columns). One row per option contract per run date. Key columns: `Company`, `TKR`, `C/P`, `STRIKE`, `EXPDAY`, `CMPRICE`, `BID`, `ASK`, `DLTA`, `GAMMA`, `THETA`, `VEGA`, `HISTVO`, `VOLF`, `date_run` |
| `dt_bfly` | Butterfly spread candidates. One row per ticker. Columns: `TKR`, `CMRK`, `TechRank`, `CMPRICE`, `HISTVO`, `VOLF`, `vol_diff`, `EXPDAY`, `id_minus_1/0`, `id_plus_0/1`, `cost` |
| `dx_date_exp` | All expiration dates found in `dx_blob` with day-of-week and week-of-month |
| `dt_date_exp_mth` | Monthly (3rd-Friday) expiration calendar from 2020–2030 |
| `dt_top_3_exp` | Next three upcoming monthly expiration dates |
| `dx_ticker` | Unique ticker → current stock price lookup |
| `dx_company` | Unique company list |
| `dx_industry` | Unique industry classifications |
| `dx_s_minus_*/dx_s_plus_*` | Near-the-money strike tables used for butterfly leg selection |

---

## Configuration

**`config/global.dcf`** controls ProjectTemplate behaviour:

| Setting | Value |
|---|---|
| `data_loading` | `TRUE` — auto-loads CSV files from `data/` |
| `munging` | `TRUE` — auto-runs scripts in `munge/` |
| `load_libraries` | `TRUE` — loads all libraries listed in `libraries:` |
| `tables_type` | `data_table` — all loaded tables are `data.table` objects |
| `cache_loaded_data` | `FALSE` |
| `logging` | `TRUE` (INFO level) |

**`lib/globals.R`** sets runtime globals including:

- Current / last year variables
- Risk-free interest rate (`DGS3MO` pulled live from FRED via `quantmod`)
- OneDrive directory paths for data and ZIP files
- Local directory paths for downloads

---

## Options Strategies Covered

### Butterfly Spread
A four-leg neutral strategy combining puts and calls around the current stock price:
- **Leg 1** (`s_minus_1`): Buy put one strike below current price
- **Leg 2** (`s_minus_0`): Sell call at-the-money (below)
- **Leg 3** (`s_plus_0`): Buy put at-the-money (above)
- **Leg 4** (`s_plus_1`): Sell call one strike above current price

Net cost is computed from bid/ask prices. Probability-of-profit and "rule of 16" volatility targets are planned enhancements (see `TODO`).

### Iron Condor
ROI data and file layouts are stored in `reports/` and `docs/`. Reference PDFs in `docs/` document the iron condor structure, profit/loss diagrams, and position management guidelines.

---

## Machine Learning

`src/090-ml.r` and `src/090_fun_bfly.r` prepare `dt_bfly` for modelling. The project includes:

- **Mean reversion screening** (`mean_reversion()` in `lib/helpers.R`): computes SMA-150, ATR-10, and RSI-3 for any ticker loaded via `quantmod`
- **Monte Carlo payoff simulation** (`fun_payoff()`): simulates stock price paths using GBM and computes expected P&L across scenarios
- **MLR3 / caret** modelling pipeline: uses serialized `dt_bfly.qs` as training data

---

## Reference & Documentation

| File | Purpose |
|---|---|
| `docs/dx_condor_file_layout.txt` | Column-by-column layout of the ALLNEW.CSV ValueLine export |
| `docs/readmeMenu.md` | Full `dx_blob` column name index |
| `docs/readmeMetaData.md` | All table schemas (dx_blob, and historical FAFSA/aid tables) |
| `xls/BlackScholesModel.xls` | Black-Scholes option pricing spreadsheet |
| `xls/CCALC.xls` / `CCPUT.xls` | Covered call / cash-secured put calculators |
| `pdf/210622Fidelity_Options-trade-management.pdf` | Fidelity options trade management guide |
| `TODO` | Outstanding feature backlog for `dt_bfly` Greeks and field additions |
