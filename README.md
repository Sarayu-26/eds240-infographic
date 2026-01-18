# eds240-infographic
eds 240 final project infographic repo 

# CHNMS Fleet Activity Data (2017–2024)

## Overview
This repository contains a processed dataset of Global Fishing Watch (GFW) fleet activity within Chumash Heritage National Marine Sanctuary (CHNMS) from 2017 to 2024. The goal is to provide a baseline for analyzing fishing activity, gear types, and vessel effort within the sanctuary, which can be used for visualization, this project infographic and future management decisions.

---

## Repository Structure
- `data/` – Contains the processed CSV dataset (`fleet_chnms_2017_2024.csv`) and CHNMS boundary shapefile.
- `scripts/` – R scripts for processing and aggregating raw GFW fleet data.
- `README.md` – This file.

---

## Data Access
- **Processed data:** `fleet_chnms_2017_2024.csv` is included in the repository and ready for analysis. The file is currently held in .gitignore, please reach out sramnath@bren.ucsb.edu for the copy. 
- **Raw data:** Monthly GFW fleet CSVs (v3, 2017–2024) are **not included** to reduce repository size. They can be re-downloaded from the [Global Fishing Watch data portal](https://globalfishingwatch.org/datasets-and-code/) if needed.
- **Boundary shapefile:** Included in `data/chnms_designated_boundary/CHNMS_py.shp`. and available publicly at 

---

## Data Processing
1. Monthly CSVs were combined into a single dataset.  
2. Fleet points were converted to spatial (`sf`) objects using latitude and longitude coordinates.  
3. Points outside the CHNMS boundary were removed using a spatial join.  
4. The resulting dataset contains 5,896 observations over 10 variables and is geometry-free for easy analysis.

### Columns in `fleet_chnms_2017_2024.csv`
| Column | Description |
|--------|------------|
| date | Observation date |
| year | Year of observation |
| month | Month of observation |
| cell_ll_lat | Latitude of fleet cell |
| cell_ll_lon | Longitude of fleet cell |
| flag | Vessel flag (country) |
| geartype | Type of fishing gear |
| hours | Total hours observed |
| fishing_hours | Hours actively fishing |
| mmsi_present | Indicator if vessel is present in AIS data |

---

## Usage
You can load the processed dataset in R:

```r
library(readr)
fleet <- read_csv("data/fleet_chnms_2017_2024.csv")

---
## Contact
sramnath@bren.ucsb.edu 
