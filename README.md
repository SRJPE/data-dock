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

-   Water Quality: EDI package [link when available]
-   Genetics: EDI package [link when available]
-   Both pulled via `contentid` in global.R

## Key Data Objects (created in global.R)

| Object                     | Description                                    |
|---------------------------|---------------------------------------------|
| `wq_data`                  | All WQ observations including unmappable sites |
| `wq_metadata`              | Mappable sites only (used for map markers)     |
| `wq_metadata_variable`     | Sites with no lat/long (dropdown/plot only)    |
| `run_designation`          | Genetics run assignment data                   |
| `rst_sites`                | RST monitoring site locations                  |
| `salmonid_habitat_extents` | River polylines for map                        |
