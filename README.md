---
editor_options: 
  markdown: 
    wrap: 72
---

# Downstream Dashboard

## Overview

R Shiny app visualizing EMP water quality and SR JPE genetics data.
Built with: R, Shiny, Leaflet, Plotly, ggplot2, sf, shinyWidgets

## Project Structure

-   `global.R` — data loading, shared objects
-   `server.R` — reactive logic, plots, maps, downloads
-   `ui.R` — layout and UI components
-   `www/`
    -   `overview.html` — Overview tab content

## Data Sources

Both datasets are pulled **live from EDI (Environmental Data Initiative)** 
at app startup via the PASTA REST API. No local data files are needed.

| Dataset | EDI Package ID | Notes |
|---|---|---|
| Genetics | `edi.2335` | Always pulls latest revision automatically |
| Water Quality | `edi.458` | Always pulls latest revision automatically |

## How Data Loading Works (`global.R`)
A shared helper function `fetch_data_from_api(url)` handles all EDI requests:
1. Queries the PASTA API for all revisions of a package
2. Automatically selects the newest revision number
3. Fetches the file list for that revision
4. Downloads the target file by its EDI file ID

### Key Data Objects (created in global.R)

| Object                     | Description                                    |
|---------------------------|---------------------------------------------|
| `wq_data`                  | All WQ observations including unmappable sites |
| `wq_metadata`              | Mappable sites only (used for map markers)     |
| `wq_metadata_variable`     | Sites with no lat/long (dropdown/plot only)    |
| `run_designation`          | Genetics run assignment data                   |
| `rst_sites`                | RST monitoring site locations                  |
| `salmonid_habitat_extents` | River polylines for map                        |


### EDI File IDs (hardcoded in global.R)
| File | EDI ID | Description |
|---|---|---|
| Genetics run designation | `c1174bbf130272bf4124905c2ff73c66` | Main genetics dataset |
| Water Quality data | `72c6b8cfbeca84df5086e721fcff1757` | Main WQ dataset |
| Water Quality metadata | `ac44e8bf5f7a8afce67ba0d6cbfbc228` | Station locations and descriptions |

## Updating Data
Data updates automatically on each app restart — no code changes needed 
as long as the EDI package ID and file ID stay the same.

**If EDI file IDs change** (e.g. after a major data revision):
- Find the new file ID in the EDI package page
- Update the hardcoded file ID in `global.R`
- Example: `"/c1174bbf130272bf4124905c2ff73c66"` for genetics run designation file


