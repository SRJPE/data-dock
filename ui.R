library(shiny)
library(leaflet)
library(bslib)
library(shinyWidgets)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  navbarPage(
    "Downstream Dashboard",
    id = "navbar",

    ## Overview Tab --------------------------------------------------------
    tabPanel("Overview", includeHTML("www/overview.html")),
    ## Water Quality Tab ---------------------------------------------------
    tabPanel(
      "Water Quality",
      tabsetPanel(
        id = "wq_tabs",

        ### Visualize ---------------------------------------------------------------

        tabPanel(
          "Visualize Data",
          fluidRow(column(width = 12, )),
          div(
            style = "display: flex; gap: 20px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 10px;",
            div(
              style = "min-width: 200px;",

              {
                stations <- sort(unique(wq_metadata$station_id_name))

                # Pad single-digit station codes at the very start
                stations_label <- stringr::str_replace(
                  stations,
                  "^([A-Za-z]+)(\\d)([A-Za-z]?)(\\s*-\\s*)",
                  "\\10\\2\\3\\4"
                )

                # Sort by the padded display label
                ord <- order(stations_label)

              selectizeInput(
                inputId = "location_filter_wq",
                label = tags$strong("Station"),
                choices = stats::setNames(stations[ord], stations_label[ord]),
                multiple = TRUE,
                options = list(plugins = list('remove_button'), placeholder = 'Select a station')
              )
              }
            ),
            div(
              style = "min-width: 250px;",
              sliderInput(
                "year_range",
                tags$strong("Date Range"),
                min = as.Date(min(wq_data$date)),
                max = as.Date(max(wq_data$date)),
                value = c(as.Date(min(wq_data$date)), as.Date(max(wq_data$date))),
                timeFormat = "%b %Y"
              )
            ),
            div(
              style = "min-width: 200px;",
              selectizeInput(
                inputId = "analyte",
                label = tags$strong("Analyte"),
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(plugins = list('remove_button'), placeholder = "Select an analyte")
              )
            ),
            div(style = "min-width: 200px;", selectInput(
              "plot_type", tags$strong("Plot Type"), choices = c("Time Series", "Box Plot")
            )),
            actionButton("clear_all", "Clear All", icon = icon("eraser")),
          ),

          # === Map and Floating Plot Panel ===
          fluidRow(
            column(width = 4, leafletOutput("wq_map", height = "600px")),
            column(
              width = 8,
              plotlyOutput("wq_dynamic_plot", height = "600px"),
              conditionalPanel(condition = "input.plot_type == 'Time Series'",
              tags$p(
                "Note: Vertical dashed lines with a short horizontal cap indicate non-detect values.
                             Their height corresponds to the reporting limit (MDL/MRL).",
                style = "font-size: 0.9em; font-style: italic; color: #555; margin-top:5px;"
              )
            ),
            conditionalPanel(condition = "input.plot_type == 'Box Plot'",
                             tags$p(
                               "Note: Boxplots for some stations may not be displayed if more than 50% of the data at that location are non-detects.",
                               style = "font-size: 0.9em; font-style: italic; color: #555; margin-top:5px;"
                             )
            )
            )
          ),
          fluidRow(column(
            width = 12,
            div(
              style = "margin-top: 20px; text-align: right;",
              downloadBttn(
                "download_wq_csv",
                "Download Selected Data",
                style = "unite",
                color = "primary",
                size = "sm"
              ),
              tags$p(
                tags$span("Download the data currently selected in the map and filters."),
                tags$br(),
                tags$span("For custom queries, visit the Download tab."),
                style = "margin-bottom: 5px; font-style: italic; color: #555;"
              )
            )
          ))
        ),

        ### Download ----------------------------------------------------------------

        tabPanel("Download Data", sidebarLayout(
          sidebarPanel(
            width = 4,
            h4("Select Data to Download"),
            tags$hr(),
            selectizeInput(
              "location_filter_dl",
              "Station",
              choices = sort(unique(wq_metadata$station_id_name)),
              multiple = TRUE,
              options = list(plugins = list('remove_button'), placeholder = "Select a station")
            ),
            sliderInput(
              "year_range_dl",
              "Date Range",
              min = as.Date(min(wq_data$date)),
              max = as.Date(max(wq_data$date)),
              value = c(as.Date(min(wq_data$date)), as.Date(max(wq_data$date))),
              timeFormat = "%b %Y"
            ),
            selectizeInput(
              "analyte_download",
              "Analyte",
              choices = sort(unique(wq_data$analyte)),
              multiple = TRUE,
              options = list(plugins = list('remove_button'), placeholder = "Select an analyte")
            ),
            checkboxInput(
              inputId = "include_weather",
              label = "Include associated weather observations (Rain, Sky Conditions)",
              value = FALSE
            ),
            actionButton(
              inputId = "clear_all_dl",
              label = "Clear all",
              icon = icon("eraser")
            ),
            tags$hr(),
            div(
              style = "margin-top: 10px; text-align: center;",
              downloadBttn(
                "download_wq_csv_dl",
                "Download Selected Data",
                style = "unite",
                color = "primary",
                size = "lg"
              ),
              tags$p(
                HTML(
                  "Download the data you’ve selected using the filters on this tab.<br>
        The table provides a preview only — the exported <code>.csv</code> file will include the complete raw dataset.<br>
        For more information about the data and metadata, please visit the
        <a href='link-to-metadata-file' target='_blank'>EDI package here</a>."
                ),
                #TODO add link to EDI
                style = "font-style: italic; color: #555; text-align: center; margin-top: 10px;"
              )
            )
          ),

          # Data preview table
          mainPanel(
            width = 8,
            h3("Preview of Selected Data"),
            tags$p(
              "This table updates automatically when you change filters in the sidebar.",
              style = "font-style: italic; color: #555; margin-bottom: 10px;"
            ),
            div(style = "margin-top: 10px;", DT::dataTableOutput("dl_preview_table"))
          )
        ))
      )
    ),

    ## Genetics Tab ---------------------------------------------------------
    tabPanel(
      "Genetics",
      tabsetPanel(
        id = "genetics_tabs",

        ### Visualize ---------------------------------------------------------------

        tabPanel("Visualize Data", fluidRow(
          column(
            width = 12,
            div(
              style = "display: flex; gap: 20px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 10px;",
              div(
                style = "min-width: 200px;",
                selectizeInput(
                  inputId = "location_filter_g",
                  label = tags$strong("Monitoring Site"),
                  choices = c(sort(unique(
                    run_designation$map_label
                  ))),
                  selected = "Clear Creek",
                  multiple = TRUE,
                  options = list(plugins = list('remove_button'), placeholder = "Select a site")
                )
              ),
              div(
                style = "min-width: 250px;",
                sliderInput(
                  "year_range_g",
                  tags$strong("Year Range"),
                  min = as.numeric(min(run_designation$year)),
                  max = as.numeric(max(run_designation$year)),
                  value = c(as.numeric(min(
                    run_designation$year
                  )), max(run_designation$year)),
                  step = 1,
                  sep = "",
                  ticks = TRUE
                )
              ),
              div(
                style = "min-width: 200px;",
                selectInput(
                  "plot_type_g",
                  tags$strong("Data Summary Type"),
                  choices = c("Monitoring Year", "Month")
                )
              ),
              div(style = "min-width: 200px;", selectInput(
                "data_plot_g",
                tags$strong("Data Type"),
                choices = c("Run Type", "Greb 1L RoSA Genotype")
              )),
              actionButton("clear_all_g", "Clear All", icon = icon("eraser")),
            ),

            # === Map and Floating Plot Panel ===
            fluidRow(
              column(width = 4, leafletOutput("g_map", height = "600px")),
              column(
                width = 8,
                plotlyOutput("g_dynamic_plot", height = "600px"),
                tags$p(
                  "Note: The year is representative of a monitoring year (Nov-May), see [data collection methods](insert link) for more detail.",
                  style = "font-size: 0.9em; font-style: italic; color: #555; margin-top:5px;"
                )
              )
            ),
            fluidRow(column(
              width = 12,
              div(
                style = "margin-top: 20px; text-align: right;",
                downloadBttn(
                  "download_g_csv",
                  "Download Selected Data",
                  style = "unite",
                  color = "primary",
                  size = "sm"
                ),
                tags$p(
                  tags$span("Download the data currently selected in the map and filters."),
                  tags$br(),
                  tags$span("For custom queries, visit the Download tab."),
                  style = "margin-bottom: 5px; font-style: italic; color: #555;"
                )
              )
            ))
          )
        )),

        ### Download ----------------------------------------------------------------

        # TODO currently not very functional
        tabPanel("Download Data", sidebarLayout(
          sidebarPanel(
            width = 4,
            h4("Select Data to Download"),
            tags$hr(),
            selectizeInput(
              "location_filter_dl_g",
              "Monitoring Site",
              choices = sort(unique(run_designation$map_label)),
              selected = "Clear Creek",
              multiple = TRUE,
              options = list(plugins = list('remove_button'), placeholder = "Select a site")
            ),
            sliderInput(
              "year_range_dl_g",
              "Year Range",
              min = as.numeric(min(run_designation$year)),
              max = as.numeric(max(run_designation$year)),
              value = c(as.numeric(min(
                run_designation$year
              )), max(run_designation$year)),
              step = 1,
              sep = ""
            ),
            selectizeInput(
              "run_download",
              "Run Name",
              choices = sort(unique(run_designation$run_name)),
              multiple = TRUE,
              options = list(plugins = list('remove_button'), placeholder = "Select run name")
            ),
            actionButton(
              inputId = "clear_all_dl_g",
              label = "Clear all",
              icon = icon("eraser")
            ),
            tags$hr(),
            div(
              style = "margin-top: 10px; text-align: center;",
              downloadBttn(
                "download_g_csv_dl",
                "Download Selected Data",
                style = "unite",
                color = "primary",
                size = "lg"
              ),
              tags$p(
                HTML(
                  "Download the data you’ve selected using the filters on this tab.<br>
        The table provides a preview only — the exported <code>.csv</code> file will include the complete raw dataset.<br>
        For more information about the data and metadata, please visit the
        <a href='link-to-metadata-file' target='_blank'>EDI package here</a>."
                ),
                #TODO add link to EDI
                style = "font-style: italic; color: #555; text-align: center; margin-top: 10px;"
              )
            )
          ),

          # Data preview table
          mainPanel(
            width = 8,
            h3("Preview of Selected Data"),
            tags$p(
              "This table updates automatically when you change filters in the sidebar.",
              style = "font-style: italic; color: #555; margin-bottom: 10px;"
            ),
            div(style = "margin-top: 10px;", DT::dataTableOutput("dl_preview_table_g"))
          )
        ))
      )
    )
  )
)
