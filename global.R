library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(lubridate)
library(plotly)
library(shinycssloaders)
library(sf)
library(janitor)
library(EDIutils)
library(readr)


# ======================== HELPERS =====================
## Colors ----------------------------------------------

tol_muted <- c("#2E2585", "#337538", "#5DA899", "#94CBEC","#DCCD7D", "#C26A77", "#9F4A96","#7E2954")


## function for reading data directly from EDI ---------
fetch_data_from_api <- function(url) {
  tryCatch({
    response <- httr::GET(url)
    data <- httr::content(response, as = "raw")
    return(data)
  }, error = function(e) {
    stop(paste("Error fetching data from API:", e$message))
  })
}

# ------------------ GENETICS DATA ------------------------------------------------------

## ================== rst sites data  ============
rst_raw <- readRDS("data-raw/rst_sites.Rds")

rst <- rst_raw |>
  filter(site %in% c("lcc", "ubc", "mill creek", "deer creek", "okie dam",
                     "tisdale", "knights landing", "hallwood", "eye riffle")) |>
  select(stream, site, geometry) |>
  mutate(label = case_when(site == "knights landing" ~ "Sacramento River - Knights Landing",
                           site == "tisdale" ~ "Sacramento River - Tisdale",
                           site == "eye riffle" ~ "Feather River - RM 61",
                           T ~ str_to_title(stream)))

# adding Delta Entry and Lower Feather Rm 17
additional_rst <- readxl::read_xlsx("data-raw/sample_locations_20220830.xlsx") |>
  filter(code %in% c("F17", "DEL")) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  mutate(stream = tolower(stream_name),
         site = tolower(location_name),
         label = case_when(site == "sac-delta entry" ~ "Sacramento River - Delta Entry",
                           site == "feather-rm17" ~ "Feather River - RM 17")
        ) |>
  select(stream, site, geometry, label)

rst_sites <- bind_rows(additional_rst, rst)

# adding lat/long fields for zooming functionality
coords <- sf::st_coordinates(rst_sites)
rst_sites$longitude <- coords[, 1]
rst_sites$latitude <- coords[, 2]

# habitat extents
salmonid_habitat_extents <- readRDS("data-raw/salmonid_habitat_extents.Rds")

## ================== data pull from edi ============
genetics_identifier <- "2335"
genetics_version <- "1"
edi_file_base_url <- paste0(
  "https://pasta.lternet.edu/package/data/eml/edi/", genetics_identifier, "/", genetics_version
)
file_ids_bytes <- fetch_data_from_api(edi_file_base_url)
#file_ids contains the list of files from the package in the form of ids
file_ids_genetics <- read_csv(file_ids_bytes,
                     col_names = c("table_id"),
                     show_col_types = FALSE)

# file_ids_genetics |> View()
edi_file_url <- paste0(edi_file_base_url, "/c1174bbf130272bf4124905c2ff73c66")
file_data_genetics <- fetch_data_from_api(edi_file_url)

genetics_data_raw <- read_csv(
  I(rawToChar(file_data_genetics)),
  show_col_types = FALSE) |>
  clean_names() |>
  rename(run_name = final_run_designation,
       field_run_type_id = field_run_type) |>  # renaming for now to keep consistency with previous sample query
  mutate(fork_length_mm = ifelse(is.na(fork_length_mm),
                               sample(fork_length_mm[!is.na(fork_length_mm)], sum(is.na(fork_length_mm)), replace = TRUE),
                               fork_length_mm),
       field_run_type_id = ifelse(is.na(field_run_type_id),
                                  sample(field_run_type_id[!is.na(field_run_type_id)], sum(is.na(field_run_type_id)), replace = TRUE),
                                  field_run_type_id)) |>
mutate(run_name = tolower(run_name))

sample_location <- read_csv(here::here("data-raw","grunid_sample_location.csv"))

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

identifier <- "458"
version <- "13"
edi_file_base_url <- paste0(
  "https://pasta.lternet.edu/package/data/eml/edi/", identifier, "/", version
)
file_ids_bytes <- fetch_data_from_api(edi_file_base_url)
#file_ids contains the list of files from the package in the form of ids
file_ids <- read_csv(file_ids_bytes,
                     col_names = c("table_id"),
                     show_col_types = FALSE)
# file_ids |> View()

# ==================== data pull from edi ===============
edi_file_url <- paste0(edi_file_base_url, "/72c6b8cfbeca84df5086e721fcff1757")
file_data <- fetch_data_from_api(edi_file_url)

wq_data_raw <- read_csv(
  I(rawToChar(file_data)),
  show_col_types = FALSE) |>
  clean_names() |>
  rename(station_id = station,
         value = result_value,
         unit = result_unit)

# ==================== metadata pull frmo edi =====
edi_file_url_metadata <- paste0(edi_file_base_url, "/ac44e8bf5f7a8afce67ba0d6cbfbc228")
file_metadata <- fetch_data_from_api(edi_file_url_metadata)

wq_metadata_raw <- read_csv(
  I(rawToChar(file_metadata)),
  show_col_types = FALSE) |>
  clean_names()

# "variable" locations
# wq_metadata_raw |>
#   filter(latitude == "variable") |>
#   view()

wq_metadata <- wq_metadata_raw |>
  filter(latitude != "variable") |> # these data entries do not have lat/long, so I am leaving them out for now
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  group_by(station_description) |>
  mutate(station_description = case_when(
    status == "Inactive" ~ paste0(station_description, " - Historical"),
    TRUE ~ station_description)) |>
  ungroup() |>
  mutate(station_id_name = paste(station_id, "-", station_description))
# adding lat/long fields for zooming functionality
coords <- sf::st_coordinates(wq_metadata)
wq_metadata$longitude <- coords[, 1]
wq_metadata$latitude <- coords[, 2]

# =================== combine metadata with data ===============
wq_data_joined <- wq_data_raw |>
left_join(wq_metadata |>  st_drop_geometry() |> select(station_id, station_description),
          by = "station_id") |>
  st_drop_geometry()

wq_data <- wq_data_joined |>
  mutate(date = as.Date(date),
         value = as.numeric(value),
         year = year(date)) |>
  filter(!is.na(station_description),
         analyte != "Latitude",
         analyte != "Longitude",
         analyte != "Rain",
         analyte != "Sky Conditions") |>
  mutate(station_id_name = paste(station_id, "-", station_description)) |>
  glimpse()

# was planning on using analyte lat/long to assign location. however, it is inconsistent across same station_id
wq_data_missing_location <- wq_data |>
  filter(station_id %in% c("LSZ6", "LSZ2", "LSZ2-SJR", "LSZ6-SJR"),
         analyte %in% c("Latitude", "Longitude"))

# ==================== weather analytes ==========================
wq_quality_weather <- wq_data_joined |>
  filter(analyte %in% c("Rain", "Sky Conditions", "Weather Observations", "Wave Scale")) |>
  mutate(station_id_name = paste(station_id, "-", station_description)) |> glimpse()

