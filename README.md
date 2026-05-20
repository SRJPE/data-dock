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

Both Water Quality and Genetics datasets are pulled **live from EDI
(Environmental Data Initiative)** at app startup via the PASTA REST API.
Three additional local files are used for shiny app.

| Dataset                       | Source                                                                            | Notes                                                                |
|------------------------|------------------------|------------------------|
| Genetics                      | `edi.2335`                                                                        | Always pulls latest revision automatically                           |
| Water Quality                 | `edi.458`                                                                         | Always pulls latest revision automatically                           |
| RST monitoring site locations | Local file `data-raw/rst_sites.Rds` + `data-raw/sample_locations_20220830.xlsx`  **see TODO | RST monitoring site locations (coordinates are jittered for privacy) |
| Genetics sample location names    | Local file `data-raw/grunid_sample_location.csv` | Lookup table linking 3-letter site codes to full location names. Used to join site labels onto genetics data |
| River Polylines               | Locat file sourced from [CVPIA](https://cvpia-osc.github.io/DSMhabitat/articles/habitat-extents-map.html) | River polylines displayed on both maps                               |

## How Data Loading Works (`global.R`)

A shared helper function `fetch_data_from_api(url)` handles all EDI
requests:

1.   Queries the PASTA API for all revisions of a package

2.  Automatically selects the newest revision number

3.  Fetches the file list for that revision 4. Downloads the target file
    by its EDI file ID

## Key Data Objects (created in `global.R`)

### Water Quality

| Object                 | Source                         | Description                                                                                                          |
|------------------------|------------------------|------------------------|
| `wq_data_raw`          | EDI `edi.458`                  | Raw WQ observations as pulled from EDI                                                                               |
| `wq_metadata_raw`      | EDI `edi.458`                  | Raw station metadata as pulled from EDI                                                                              |
| `wq_quality_weather`   | EDI `edi.458`                  | Raw weather observations (Rain, Sky Conditions, Weather Observations, Wave Scale) . Pulled separately from `wq_data` |
| `wq_data`              | Derived from `wq_data_raw`     | Cleaned WQ observations joined to metadata. Excludes Latitude, Longitude, Rain, Sky Conditions analytes              |
| `wq_metadata`          | Derived from `wq_metadata_raw` | Mappable stations only (excludes `latitude == "variable"`) . Used for map markers, converted to `sf` object          |
| `wq_metadata_variable` | Derived from `wq_metadata_raw` | Stations with no fixed lat/long, included in dropdowns and plots but not shown on map                                |

### Genetics

| Object              | Source                           | Description                                                                         |
|------------------------|------------------------|------------------------|
| `genetics_data_raw` | EDI `edi.2335`                   | Raw genetics data as pulled from EDI                                                |
| `run_designation`   | Derived from `genetics_data_raw` | Cleaned genetics data with run assignments, monitoring year, month, and site labels |

### Spatial

`rst_sites` and `salmonid_habitat_extents`

### Notes

-   All EDI data is pulled **live at app startup** via the PASTA REST
    API. No local copies of WQ or genetics data are stored
-   `rst_sites` and `salmonid_habitat_extents` are the only datasets
    loaded from local files

### EDI File IDs

| File                     | EDI ID                             | Description                        |
|------------------------|------------------------|------------------------|
| Genetics run designation | `c1174bbf130272bf4124905c2ff73c66` | Main genetics dataset              |
| Water Quality data       | `72c6b8cfbeca84df5086e721fcff1757` | Main WQ dataset                    |
| Water Quality metadata   | `ac44e8bf5f7a8afce67ba0d6cbfbc228` | Station locations and descriptions |

## Updating Data

Data updates automatically on each app restart — no code changes needed
as long as the EDI package ID and file ID stay the same.

**If EDI file IDs change** (e.g. after a major data revision):

-   Find the new file ID in the EDI package page
-   Update the hardcoded file ID in `global.R`
-   Example: `"/c1174bbf130272bf4124905c2ff73c66"` for genetics run
    designation file

# Data Structure Reference

This document describes the expected structure of raw data pulled from
EDI. If columns are renamed, added, or removed in EDI, the app will need
to be updated accordingly. See `global.R` for all transformations
applied to raw data.

------------------------------------------------------------------------

## 1. Water Quality Data (`wq_data_raw`)

**EDI Package:** `edi.458`

**Rows:** 379,346 (as of latest pull)

**One row per:** single analyte measurement at a station on a given date

| Column (EDI)       | Column (app)       | Type | Required | Definition                                                                                                | Valid Values / Range                         |
|------------|------------|------------|------------|------------|------------|
| `Station`          | `station_id`       | chr  | ✅       | Station name                                                                                              | e.g. `"D4"`                                  |
| `Date`             | `date`             | date | ✅       | Date of sample collection                                                                                 | Format: `YYYY-MM-DD`                         |
| `Time`             | `time`             | chr  | ❌       | Time of sample collection in Pacific Standard Time                                                        | Format: `hh:mm:ss`                           |
| `Sampling_Depth`   | `sampling_depth`   | dbl  | ❌       | Depth at which parameter was measured                                                                     | Numeric, units: feet                         |
| `Analyte`          | `analyte`          | chr  | ✅       | Measured or observed parameter during sampling                                                            | e.g. `"Chlorophyll a"`, `"Dissolved Oxygen"` |
| `Result_Value`     | `value`            | chr  | ✅       | Value of measured or observed analyte — stored as character in EDI, converted to numeric in app           | e.g. `"1.05"`                                |
| `Result_Unit`      | `unit`             | chr  | ✅       | Unit associated with measured or observed analyte                                                         | e.g. `"ug/L"`, `"mg/L"`, `"degF"`            |
| `Detection_Status` | `detection_status` | chr  | ✅       | Whether results were above or below the method's minimum reporting limit                                  | `"Detected"` or `"Not detected"`             |
| `MRL`              | `mrl`              | dbl  | ❌       | Lowest quantified level within an analytical method's operational range considered reliable for reporting | Numeric                                      |
| `Analysis_Method`  | `analysis_method`  | chr  | ❌       | Analytical method used to measure particular analyte                                                      | e.g. `"EPA 360.2"`                           |
| `QC_Flag`          | `qc_flag`          | chr  | ❌       | Quality control flag assigned to the sample                                                               | e.g. `NA`, `"Q"`                             |
| `Field_Comment`    | `field_comment`    | chr  | ❌       | Field comments recorded during sampling                                                                   | Free text or `NA`                            |

**App dependencies:**

-   `Station` (EDI) is read as `station_id` in the app – must match
    `StationID` in `wq_metadata_raw` for the join to work

-   `Result_Value` is stored as character in EDI – app converts to
    numeric via `as.numeric(value)` - `Detection_Status` must be exactly
    `"Detected"` or `"Not detected"` – app uses `tolower()` matching so
    case is handled, but spelling must match exactly

-   `Rain`, `Sky Conditions`, `Weather Observations` and `Wave Scale`
    analytes are filtered out of `wq_data` and handled separately as
    weather data

-   `Latitude` and `Longitude` analytes are also filtered out of
    `wq_data`

------------------------------------------------------------------------

## 2. Water Quality Station Metadata (`wq_metadata_raw`)

**EDI Package:** `edi.458`

**One row per:** monitoring station

| Column (EDI)          | Column (app)          | Type | Required | Definition                                                                               | Valid Values / Range                             |
|------------|------------|------------|------------|------------|------------|
| `LocationID`          | `location_id`         | chr  | ❌       | Broad water body number, light channel marker number, or reference area                  | e.g. `"D4"`                                      |
| `StationID`           | `station_id`          | chr  | ✅       | Specific location within the broad water body — must match `station_id` in `wq_data_raw` | e.g. `"D4"`, `"D14A"`                            |
| `Station Type`        | `station_type`        | chr  | ❌       | Location of station relative to channel — vessel (mid-channel) or vehicle (shore-based)  | `"Mid-channel"` or `"Shore-based"`               |
| `Station Description` | `station_description` | chr  | ✅       | Descriptive name for geographic area                                                     | e.g. `"Sacramento River above Point Sacramento"` |
| `Latitude`            | `latitude`            | chr  | ✅       | North latitude coordinate of the station — some stations have no fixed location          | Decimal degrees or `"variable"`                  |
| `Longitude`           | `longitude`           | chr  | ✅       | West longitude coordinate of the station — some stations have no fixed location          | Decimal degrees or `"variable"`                  |
| `Start Date`          | `start_date`          | date | ❌       | First date of collection for the station                                                 | Format: `YYYY-MM-DD`                             |
| `End Date`            | `end_date`            | date | ❌       | Last date of collection for the station                                                  | Format: `YYYY-MM-DD` or `NA` if still active     |
| `Status`              | `status`              | chr  | ✅       | Status of the station as of December 2024                                                | `"Active"` or `"Inactive"`                       |

**App dependencies:**

-   `StationID` must match `Station` in `wq_data_raw` for the join to
    work

-   Rows where `latitude == "variable"` are excluded from the map but
    their station data still appears in dropdowns and plots via
    `wq_metadata_variable`

-   `status` must be exactly `"Active"` or `"Inactive"` – controls map
    marker color (black = active, gray = inactive) and highlight
    behavior

-   `station_id` and `station_description` are combined in the app to
    create the display label:
    `paste(station_id, "-", station_description)`

-   Inactive stations get `" - Historical"` appended to
    `station_description` in `global.R`

------------------------------------------------------------------------

## 3. Genetics Data (`genetics_data_raw`)

**EDI Package:** `edi.2335`\
**Rows:** 7,477 (as of latest pull)

| Column                  | Type | Required | Description                                                                                         | Valid Values / Range                                                                  |
|---------------|---------------|---------------|---------------|---------------|
| `sample_id`             | chr  | ✅       | Unique sample ID, composed of location code, season, sample event, bin, and number separated by `_` | e.g. `"BTC22_2_A_1"`                                                                  |
| `datetime_collected`    | dttm | ✅       | Date and time of sample collection                                                                  | Format: `YYYY-MM-DD hh:mm:ss`                                                         |
| `fork_length_mm`        | dbl  | ❌       | Fork length of fish in millimeters                                                                  | `23` – `176`                                                                          |
| `field_run_type`        | chr  | ❌       | Run type assigned in the field using length-at-date                                                 | `FALL`, `LATEFALL`, `SPRING`, `WINTER`, `UNKNOWN`, `NA`                               |
| `final_run_designation` | chr  | ✅       | Final run type from combined SHERLOCK and GT-seq analysis                                           | `FALL OR LATE FALL`, `SPRING`, `GREB1L HETEROZYGOTE`, `WINTER`                        |
| `shlk_chr28_genotype`   | chr  | ❌       | SHERLOCK Greb1L (OTS28) genotype result                                                             | `EARLY`, `LATE`, `HETEROZYGOTE`, `NA`                                                 |
| `shlk_chr16_genotype`   | chr  | ❌       | SHERLOCK OTS16 genotype result                                                                      | `SPRING`, `WINTER`, `INDETERMINATE`, `NA`                                             |
| `shlk_run_designation`  | chr  | ❌       | Run designation from SHERLOCK assays and logic                                                      | `SPRING`, `EARLY/LATE HETEROZYGOUS`, `SPRING/WINTER`, `FALL/LATEFALL`, `WINTER`, `NA` |
| `gtseq_chr28_geno`      | chr  | ❌       | GT-seq Greb1L genotype                                                                              | `EARLY`, `LATE`, `HETEROZYGOTE`, `NA`                                                 |
| `pop_structure_id`      | chr  | ❌       | Central Valley salmonid population structure                                                        | `FALL`, `LATEFALL`, `SPRING`, `WINTER`, `NA`                                          |
| `cv_fall`               | dbl  | ❌       | Summed PofZ, CV Fall reporting unit                                                                 | `0` – `1`                                                                             |
| `cv_late_fall`          | dbl  | ❌       | Summed PofZ, CV Late Fall reporting unit                                                            | `0` – `1`                                                                             |
| `cv_spring`             | dbl  | ❌       | Summed PofZ, CV Spring reporting unit                                                               | `0` – `1`                                                                             |
| `cv_winter`             | dbl  | ❌       | Summed PofZ, CV Winter reporting unit                                                               | `0` – `1`                                                                             |
| `tributary`             | chr  | ❌       | Spring run tributary assignment                                                                     | `ButteSp`, `Feather River Lineage Spring`, `MillDeerSp`, `NA`                         |
| `buttefall`             | dbl  | ❌       | PofZ – Butte River Fall                                                                             | `0` – `1`                                                                             |
| `frh_fall`              | dbl  | ❌       | PofZ – Feather River Hatchery Fall                                                                  | `0` – `1`                                                                             |
| `frh_sp`                | dbl  | ❌       | PofZ – Feather River Hatchery Spring                                                                | `0` – `1`                                                                             |
| `mill_deer_fall`        | dbl  | ❌       | PofZ – Mill/Deer Creek Fall                                                                         | `0` – `0.999`                                                                         |
| `san_joaquin_fall`      | dbl  | ❌       | PofZ – San Joaquin Fall                                                                             | `0` – `1`                                                                             |
| `butte_sp`              | dbl  | ❌       | PofZ – Butte River Spring                                                                           | `0` – `1`                                                                             |
| `mill_deer_sp`          | dbl  | ❌       | PofZ – Mill/Deer Creek Spring                                                                       | `0` – `1`                                                                             |
| `coleman_f`             | dbl  | ❌       | PofZ – Coleman Hatchery Fall                                                                        | `0` – `1`                                                                             |
| `sac_win`               | dbl  | ❌       | PofZ – Sacramento River Winter                                                                      | `0` – `1`                                                                             |

**App dependencies:**

-   `final_run_designation` is renamed to `run_name` in `global.R` and
    converted to lowercase — valid values become: `"spring"`,
    `"winter"`, `"fall or late fall"`, `"greb1l heterozygote"`
-   `greb1l heterozygote` entries are excluded from Run Type plots per
    program guidance but retained in Greb 1L RoSA Genotype plots
-   `datetime_collected` is used to extract `year` and `month` for
    filtering and plotting
-   `shlk_chr28_genotype` is used as the genotype field — values
    converted to lowercase: `"early"`, `"late"`, `"heterozygote"`

------------------------------------------------------------------------

## What to Check After an EDI Data Update

-   [ ] Column names unchanged in all raw files
-   [ ] `station_id` values consistent between `wq_data_raw` and
    `wq_metadata_raw`
-   [ ] `detection_status` values still `"Detected"` / `"Not detected"`
-   [ ] `status` values still `"Active"` / `"Inactive"` in metadata
-   [ ] `run_name` values in genetics still match expected lowercase
    format
-   [ ] New stations in metadata have valid lat/long or `"variable"` —
    no other strings
-   [ ] App date ranges update correctly (driven by `min`/`max` of
    `wq_data$date`)
-   [ ] `final_run_designation` column name unchanged in genetics (app
    maps this to `run_name`)
-   [ ] Weather analyte names still match `"Rain"`, `"Sky Conditions"`,
    `"Weather Observations"`, `"Wave Scale"` exactly — used in
    `filter_wq_data` to separate weather from WQ observations
