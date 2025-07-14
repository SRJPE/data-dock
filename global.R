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
library(sf)
library(DBI)
library(patchwork)
# remotes::install_github("SRJPE/SRJPEmodel@wip", force = TRUE)
# remotes::install_github("SRJPE/SRJPEdata")
# library(SRJPEmodel)


# data from jpe
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
                           site == "feather-rm17" ~ "Feather River - RM17")
        ) |>
  select(stream, site, geometry, label)

rst_sites <- bind_rows(additional_rst, rst)

# habitat extents
salmonid_habitat_extents <- readRDS("data-raw/salmonid_habitat_extents.Rds")



# Database data ----
library(patchwork)

colors_full <-  c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089", #Royal 2
                  "#899DA4", "#C93312", "#DC863B", # royal 1 (- 3)
                  "#F1BB7B", "#FD6467", "#5B1A18", # Grand Budapest 1 (-4)
                  "#D8B70A", "#02401B", "#A2A475", # Cavalcanti 1
                  "#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4", #Grand Budapest 2
                  "#9986A5", "#EAD3BF", "#AA9486", "#B6854D", "#798E87" # Isle of dogs 2 altered slightly
)


tol_muted <- c("#2E2585", "#337538", "#5DA899", "#94CBEC","#DCCD7D", "#C26A77", "#9F4A96","#7E2954")

four_colors <- c("#2E2585","#337538","#C26A77","#7E2954")

grand_budapest <- c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4")

moonrise_2 <- c("#798E87FF", "#C27D38FF", "#CCC591FF", "#29211FFF")

a_palette <- c("#2A363BFF", "#019875FF", "#99B898FF", "#FECEA8FF", "#FF847CFF", "#E84A5FFF", "#C0392BFF", "#96281BFF")
pony_o <- c("#4C413FFF", "#5A6F80FF", "#278B9AFF", "#E75B64FF", "#DE7862FF", "#D8AF39FF", "#E8C4A2FF")


library(wesanderson)
?wesanderson::wes_palette()
# displayAllColors(wes_palette("Moonrise2"))
# library(colorspace)
# library(scales)
# show_col(tritan(colors_full, severity = 0.6))

library(colorBlindness)
#displayAllColors(torres)
con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                      host = "run-id-database.postgres.database.azure.com",
                      dbname = "runiddb-prod",
                      user = Sys.getenv("runid_db_user"),
                      password = Sys.getenv("runid_db_password"),
                      port = 5432)

DBI::dbListTables(con)

# Emanuel provided this query (https://github.com/SRJPE/jpe-genetics-edi/blob/main/data-query.sql)
run_designation_raw <- dbGetQuery(
  con,
  "SELECT DISTINCT ON (gri.sample_id)
    gri.sample_id,
    rt.run_name,
    substring(gri.sample_id FROM '^[^_]+_((?:100|[1-9][0-9]?))_') AS sample_event,
    st.datetime_collected,
    st.fork_length_mm,
    st.field_run_type_id
FROM
    genetic_run_identification gri
JOIN run_type rt
ON rt.id = gri.run_type_id
JOIN sample st
ON st.id = gri.sample_id
WHERE gri.sample_id LIKE '___24%%'
ORDER BY
    gri.sample_id,
    gri.created_at DESC;
"
)

sample_location <- dbGetQuery(con, "select code, location_name, stream_name, description from sample_location")
sample <- dbGetQuery(con, "select * from sample")

run_designation <- run_designation_raw |>
  mutate(code = substr(sample_id, 1, 3),
         year= paste0(20,substr(sample_id, 4,5)),
         sample_event = as.numeric(sample_event)) |>
  left_join(select(sample_location, code, location_name)) |>
  mutate(map_label = case_when(location_name %in% c("Battle", "Clear", "Mill", "Deer", "Butte") ~ paste0(location_name, " Creek"),
                               location_name == "Sac-KNL" ~ "Sacramento River - Knights Landing",
                               location_name == "Sac-Tisdale" ~ "Sacramento River - Tisdale",
                               location_name == "Feather-RM61" ~ "Feather River - RM 61",
                               location_name == "Feather-RM17" ~ "Feather River - RM 17",
                               location_name == "Sac-Delta Entry" ~ "Sacramento River - Delta Entry",
                               location_name == "Yuba" ~ "Yuba River"))

run_designation_percent <- run_designation |>
  group_by(map_label, sample_event, year, run_name) |>
  summarize(count = n()) |>
  group_by(map_label, year, sample_event) |>
  mutate(total_sample = sum(count),
         run_percent = (count/total_sample) * 100)

# plots just for display now
mean_proportions <- run_designation_percent |>
  group_by(map_label, run_name) |>
  summarize(mean_run_percent = mean(run_percent, na.rm = T),
            run_sd = sd(run_percent, na.rm = T),
            count = sum(count, na.rm = T))

# plot1 <- mean_proportions |>
#   filter(location_name == "Butte") |>
#   ggplot(aes(x = run_name, y = mean_run_percent)) +
#   geom_bar(stat = "identity", fill = "#9986A5") +
#   geom_errorbar(aes(ymin = mean_run_percent - run_sd, ymax = mean_run_percent + run_sd), width = 0.2, color = "gray") +
#   geom_text(aes(label = paste0("n=", count), y = 3), size = 3) +
#   theme_minimal() +
#   labs(x = "",
#        y = "Percent")
