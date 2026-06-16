library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(lubridate)
library(plotly)
# library(shinycssloaders)
library(sf)
library(janitor)
library(EDIutils)
library(readr)
library(leaflet)
library(bslib)


# ======================== HELPERS =====================
## Colors ----------------------------------------------

tol_muted <- c("#2E2585", "#337538", "#5DA899", "#94CBEC","#DCCD7D", "#C26A77", "#9F4A96","#7E2954")

run_col <- c("spring" = "#337538",
             "fall or late fall" = "#DCCD7D",
             "winter" = "#94CBEC",
             "early" = "#5DA899",
             "late" =  "#DCCD7D",
             "heterozygote" = "gray")

# ------------------ GENETICS DATA ------------------------------------------------------

## ================== rst sites data  ============
# reading csv
rst_csv <- read.csv("data-raw/rst_trap_locations.csv")

rst_sites <- rst_csv |>
  select(stream, site, latitude, longitude) |>
  mutate(label = case_when(site == "knights landing" ~ "Sacramento River - Knights Landing",
                           site == "tisdale" ~ "Sacramento River - Tisdale",
                           site == "eye riffle" ~ "Feather River - RM 61",
                           site == "delta entry" ~ "Sacramento River - Delta Entry",
                           site == 'rm 17 (new "lower" RST)'~ "Feather River - RM 17",
                           T ~ str_to_title(stream))) |>
  select(stream, site, latitude, longitude, label)

# Jitter coordinates for privacy (~500m offset)
# set.seed() ensures jitter is identical on every app restart
set.seed(42)  # jitter consistent every time app loads
rst_sites <- rst_sites |>
  mutate(longitude = longitude + runif(n(), min = -0.005, max = 0.005),
         latitude  = latitude  + runif(n(), min = -0.005, max = 0.005))

# habitat extents
# River polylines displayed on both the WQ and genetics maps
# Source: CVPIA (https://cvpia-osc.github.io/DSMhabitat/)
salmonid_habitat_extents <- readRDS("data-raw/salmonid_habitat_extents.Rds")

## ================ genetics data pull from edi ============
tryCatch({
  # Package edi.2335 — SR JPE genetics data
  # Always pulls the latest revision automatically
  scope      <- "edi"
  identifier <- "2335"
  revision   <- EDIutils::list_data_package_revisions(scope, identifier, filter = "newest")
  package_id <- paste(scope, identifier, revision, sep = ".")

  # List all data entities in the package
  res <- EDIutils::read_data_entity_names(package_id)

  # Pull genetics data — matches file starting with "genetic_identification_data"
  genetics_data_raw <- EDIutils::read_data_entity(
    package_id,
    res$entityId[stringr::str_detect(res$entityName, "^genetic_identification_data")]
  ) |>
    read_csv(show_col_types = FALSE) |>
    clean_names() |>
    rename(run_name          = final_run_designation,
           field_run_type_id = field_run_type) |>  # renaming for now to keep consistency with previous sample query
    mutate(run_name = tolower(run_name))

  # Save genetics backup on successful pull
  if (!dir.exists("data-raw/backup")) dir.create("data-raw/backup", recursive = TRUE)
  save(genetics_data_raw, file = "data-raw/backup/genetics_data_raw.Rda")
  message("Genetics data pulled successfully — backup saved.")

  # adding error message in case API fails to notify that backup data is being used
}, error = function(e) {
  warning(paste("EDI API unavailable for Genetics data. Loading from local backup.\nError:", e$message))
  if (file.exists("data-raw/backup/genetics_data_raw.Rda")) {
    load("data-raw/backup/genetics_data_raw.Rda", envir = .GlobalEnv)
    message("Backup Genetics data loaded successfully.")
  } else {
    stop("EDI API unavailable and no local backup found for Genetics data.")
  }
})

## ================ Sample location lookup ============
# Local file linking 3-letter site codes (from sample_id) to location names.
# If new sites are added to genetics_data_raw, add their code here.
# If new location_name values are added, update the map_label case_when below.
sample_location <- read_csv(here::here("data-raw","grunid_sample_location.csv"))

## --- Run Designation ---
# Derived from genetics_data_raw + sample_location join.
run_designation <- genetics_data_raw |>
  mutate(code = substr(sample_id, 1, 3),
         year= paste0(20,substr(sample_id, 4,5)),
         month = month(datetime_collected),
         sample_event = sub("^[^_]+_([^_]+)_.*$", "\\1", sample_id),
         sample_event = as.numeric(sample_event),
         gtseq_chr28_geno = tolower(gtseq_chr28_geno),
         shlk_chr28_genotype = tolower(shlk_chr28_genotype),
         genotype = ifelse(is.na(gtseq_chr28_geno), shlk_chr28_genotype, gtseq_chr28_geno)) |>
  left_join(select(sample_location, code, location_name)) |>
  mutate(map_label = case_when(location_name %in% c("Battle", "Clear", "Mill", "Deer", "Butte") ~ paste0(location_name, " Creek"),
                               location_name == "Sac-KNL" ~ "Sacramento River - Knights Landing",
                               location_name == "Sac-Tisdale" ~ "Sacramento River - Tisdale",
                               location_name == "Feather-RM61" ~ "Feather River - RM 61",
                               location_name == "Feather-RM17" ~ "Feather River - RM 17",
                               location_name == "Sac-Delta Entry" ~ "Sacramento River - Delta Entry",
                               location_name == "Yuba" ~ "Yuba River")) |>
  filter(!is.na(month), !is.na(genotype))


# ------------------ WATER QUALITY DATA ------------------------------------------------------

# ==================== data pull from edi ===============

tryCatch({
  # Package edi.458 — EMP discrete water quality data
  # Always pulls the latest revision automatically
  scope      <- "edi"
  identifier <- "458"
  revision   <- EDIutils::list_data_package_revisions(scope, identifier, filter = "newest")
  package_id <- paste(scope, identifier, revision, sep = ".")

  # List all data entities in the package
  res <- EDIutils::read_data_entity_names(package_id)

  # Pull WQ data — matches file starting with "EMP_DWQ_1"
  wq_data_raw <- EDIutils::read_data_entity(
    package_id,
    res$entityId[stringr::str_detect(res$entityName, "^EMP_DWQ_1")]
  ) |>
    read_csv(show_col_types = FALSE) |>
    clean_names() |>
    rename(station_id = station,
           value      = result_value,
           unit       = result_unit)

  # Pull WQ station metadata — matches file starting with "EMP_DWQ_Stations_"
  wq_metadata_raw <- EDIutils::read_data_entity(
    package_id,
    res$entityId[stringr::str_detect(res$entityName, "^EMP_DWQ_Stations_")]
  ) |>
    read_csv(show_col_types = FALSE) |>
    clean_names()

  # Save backups on successful pull
  if (!dir.exists("data-raw/backup")) dir.create("data-raw/backup", recursive = TRUE)
  save(wq_data_raw,     file = "data-raw/backup/wq_data_raw.Rda")
  save(wq_metadata_raw, file = "data-raw/backup/wq_metadata_raw.Rda")
  message("Water Quality data pulled successfully — backup saved.")

}, error = function(e) {
  warning(paste("EDI API unavailable for Water Quality data. Loading from local backup.\nError:", e$message))
  if (file.exists("data-raw/backup/wq_data_raw.Rda") &&
      file.exists("data-raw/backup/wq_metadata_raw.Rda")) {
    load("data-raw/backup/wq_data_raw.Rda",     envir = .GlobalEnv)
    load("data-raw/backup/wq_metadata_raw.Rda", envir = .GlobalEnv)
    message("Backup Water Quality data loaded successfully.")
  } else {
    stop("EDI API unavailable and no local backup found for Water Quality data.")
  }
})
# =======================================================

# cleaning metadata to: exclude invalid lat/longs, converts to sf object, adds "historical" label
wq_metadata <- wq_metadata_raw |>
  filter(latitude != "variable") |> # these data entries are associated with LSZ locations
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  group_by(station_description) |>
  mutate(station_description = case_when(
    status == "Inactive" ~ paste0(station_description, " - Historical"),
    TRUE ~ station_description)) |>
  ungroup() |>
  mutate(station_id_name = paste(station_id, "-", station_description))
# Extract lat/long back as numeric columns for Leaflet marker placement
coords <- sf::st_coordinates(wq_metadata)
wq_metadata$longitude <- coords[, 1]
wq_metadata$latitude <- coords[, 2]

# =================== combine metadata with data (main dataset used by the app) ===============
# Joins raw data to metadata to get station_description.
# Excludes: coordinate analytes, weather analytes (handled separately below),
#           and rows with no matching station in metadata.
# LSZ stations have no fixed location — their descriptions are hardcoded here.
wq_data_joined <- wq_data_raw |>
left_join(wq_metadata |>  st_drop_geometry() |> select(station_id, station_description),
          by = "station_id") |>
  st_drop_geometry()

wq_data <- wq_data_joined |>
  mutate(date = as.Date(date),
         value = as.numeric(value),
         year = year(date),
         station_description = case_when(station_id == "LSZ2" ~ "Low Salinity Zone (2000 uS/cm boundary)",
                                         station_id == "LSZ2-SJR" ~ "Low Salinity Zone in San Joaquin River (2000 uS/cm boundary)",
                                         station_id == "LSZ6" ~ "Low Salinity Zone (6000 uS/cm boundary)",
                                         station_id == "LSZ6-SJR" ~ "Low Salinity Zone in San Joaquin River (6000 uS/cm boundary)",
                                         T ~ station_description)) |>
  filter(analyte != "Latitude",
         analyte != "Longitude",
         analyte != "Rain",
         analyte != "Sky Conditions",
         analyte != "Weather Observations",
         analyte != "Wave Scale") |>
  mutate(station_id_name = paste(station_id, "-", station_description))


# ==================== weather analytes ==========================
wq_quality_weather <- wq_data_joined |>
  filter(analyte %in% c("Rain", "Sky Conditions", "Weather Observations", "Wave Scale")) |>
  mutate(station_id_name = paste(station_id, "-", station_description))

# ==================== station labels for dropdowns ==========================
stations <- sort(unique(wq_data$station_id_name))
stations_label <- stringr::str_replace(
  stations,
  "^([A-Za-z]+)0*(\\d+[A-Za-z]?)\\s*-\\s*(.+)$",
  "\\3 (\\1\\2)"
)
ord <- order(stations_label)

