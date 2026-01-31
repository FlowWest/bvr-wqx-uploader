# BVR WQX Uploader

A Shiny application for uploading water quality data to EPA's WQX (Water Quality Exchange) system for the Buena Vista Reservoir (BVR) project.

## Project Overview

- **Type**: R Shiny application
- **Purpose**: Upload water quality lab data from three sources to WQX
  - **Hydro Lab**: Field sensor data (temperature, pH, DO, conductivity, etc.)
  - **Alpha Lab**: Nutrient and coliform data
  - **Bend Genetics**: HAB toxin data (microcystin, anatoxin, etc.)

## Project Structure

```
app/              # Shiny application
├── ui.R          # Main UI (uses bslib navbar)
├── server.R      # Main server logic
├── global.R      # Global setup, libraries, validation rules
├── cdx-api.R     # Pure R CDX/WQX API client (no Python)
├── modules/      # Shiny modules for each lab type
│   ├── hydro_lab_module.R
│   ├── alpha_lab_module.R
│   ├── bend_genetics_module.R
│   └── user_account_module.R
├── hydro-lab.R           # Hydro Lab data processing
├── alpha-lab.R           # Alpha Lab data processing
├── bend-genetics-macro.R # Bend Genetics data processing
└── install-deps.R        # Dependency installer script

data/             # Processed lookup data (RData files)
data-raw/         # Raw data files and cleaning scripts
```

## Running the App

```r
# From the app/ directory
shiny::runApp()
```

Or use the batch file for desktop launch:
```
WQX Uploader (Desktop).bat
```

## Dependencies

### R Packages
Key packages (see `app/install-deps.R` for full list):
- `shiny`, `bslib`, `shinyWidgets`, `spsComps`, `shinycssloaders`
- `tidyverse` (dplyr, tidyr, readr, etc.)
- `validate` - Data validation rules
- `httr2`, `digest`, `base64enc`, `jsonlite` - CDX API client (pure R)
- `readxl` - Excel file reading

## Code Conventions

- **Shiny Modules**: Follow pattern with `*_ui()` and `*_server()` functions
- **Validation**: Use `validate` package with named rules in `global.R`
- **Data Processing**: Separate R files per lab type (`hydro-lab.R`, etc.)
- **User Account**: CDX credentials stored at `~/Documents/CDX_Account/cdx-account-info.csv`

## Data Flow

1. User uploads Excel/CSV from lab
2. App validates data against rules in `global.R`
3. Data transformed to WQX format
4. Uploaded to WQX via pure R `cdx-api.R` using CDX credentials

## Notes

- Lookup tables loaded from `data/lookup_objects.rdata`
- Project uses `project_id_lookup` for location ID validation
- Data cleaning scripts in `data-raw/` generate lookup data
