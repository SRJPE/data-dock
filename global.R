library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(lubridate)
library(plotly)
library(shinycssloaders)
library(waterYearType)
# remotes::install_github("cvpia-osc/DSMhabitat")
library(DSMhabitat)
library(ggplot2)
library(mapdata)
library(ggrepel)
library(bayesplot)
# remotes::install_github("SRJPE/SRJPEmodel@wip", force = TRUE)
# remotes::install_github("SRJPE/SRJPEdata")
# library(SRJPEmodel)


# data from jpe
rst_raw <- readRDS("data-raw/rst_sites.Rds")

rst <- rst_raw |>
  filter(site %in% c("lcc", "ubc", "mill creek", "deer creek", "okie dam",
                     "tisdale", "knights landing", "hallwood", "herringer riffle"))

# adding Delta Entry and Lower Feather Rm 17
additional_rst <- readxl::read_xlsx("data-raw/sample_locations_20220830.xlsx") |>
  filter(code %in% c("F17", "DEL")) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  mutate(stream = tolower(stream_name),
         site = tolower(location_name),
         watershed_name = stream_name, #TODO assuming there is 1 site on these two, check
         popup = case_when(stream == "feather river" ~ "<p><em>Rotary Screw Trap</em></p><p><strong>Feather River</strong></p><p>Number of Subsites: 1</p>",
                           T ~ "<p><em>Rotary Screw Trap</em></p><p><strong>Sacramento River</strong></p><p>Number of Subsites: 1</p>")) |>
  select(stream, site, geometry, popup, watershed_name, stream_name)

rst_sites <- bind_rows(additional_rst, rst)

# habitat extents
salmonid_habitat_extents <- readRDS("data-raw/salmonid_habitat_extents.Rds")

