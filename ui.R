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
    tabPanel(
      "Overview",
      includeHTML("www/overview.html")
    ),
    ## Water Quality Tab ---------------------------------------------------
    tabPanel(
      "Water Quality",
      tabsetPanel(
        id = "wq_tabs",
        tabPanel(
          "Visualize Data",
          fluidRow(column(width = 12, )),
          div(
            style = "display: flex; gap: 20px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 10px;",
            div(
              style = "min-width: 200px;",
              selectizeInput(
                inputId = "location_filter_wq",
                label = tags$strong("Station"),
                choices = sort(unique(wq_metadata$station_id_name)),
                multiple = TRUE,
                options = list(
                  plugins = list('remove_button'),
                  placeholder = 'Select a station'
                )
              )
            ),
            div(
              style = "min-width: 250px;",
              sliderInput(
                "year_range",
                "Year Range",
                min = 2020,
                max = 2025,
                value = c(2020, 2025),
                step = 1,
                sep = ""
              )
            ),
            div(
              style = "min-width: 280px;",
              dateRangeInput(
                inputId = "date_range_wq",
                label   = tags$strong("Exact Date Range"),
                start   = as.Date("2020-01-01"),
                end     = as.Date("2025-12-31"),
                format  = "yyyy-mm-dd",
                separator = " to "
              )
            ),

            div(
              style = "min-width: 200px;",
              selectizeInput(
                inputId = "analyte",
                label = "Analyte",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(plugins = list('remove_button'),
                               placeholder = "Select an analyte")
              )
            ),
            div(style = "min-width: 200px;", selectInput(
              "plot_type", "Plot Type", choices = c("Time Series", "Box Plot")
            )),
            actionButton("clear_all", "Clear All", icon = icon("eraser")),
          ),
          # --- Map Filter Panel ---
          conditionalPanel(
            condition = "input.which_view_wq == 'Map Filter'",
            div(
              style = "display: flex; gap: 20px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 10px;",
              div(
                style = "min-width: 250px;",
                sliderInput(
                  inputId = "year_range2",
                  label = tags$strong("Year Range:"),
                  min = 2020,
                  max = 2025,
                  value = c(2020, 2025),
                  step = 1,
                  sep = ""
                )
              ),
              div(
                style = "min-width: 280px;",
                dateRangeInput(
                  inputId = "date_range_wq2",
                  label   = tags$strong("Exact Date Range:"),
                  start   = as.Date("2020-01-01"),
                  end     = as.Date("2025-12-31"),
                  format  = "yyyy-mm-dd",
                  separator = " to "
                )
              ),
            )
          ),

          # === Map and Floating Plot Panel ===
          fluidRow(
            column(width = 4, leafletOutput("wq_map", height = "600px")),
            column(
              width = 8,
              plotlyOutput("wq_dynamic_plot", height = "600px"),
              tags$p(
                "Note: Vertical dashed lines with a short horizontal cap indicate non-detect values.
                             Their height corresponds to the reporting limit (MDL/MRL).",
                style = "font-size: 0.9em; font-style: italic; color: #555; margin-top:5px;"
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
        ## Download WQ Data -----
        tabPanel("Download Data", sidebarLayout(
          sidebarPanel(
            width = 4,
            h4("Select Data to Download"),
            tags$hr(),
            selectizeInput(
              "location_filter_dl",
              "Filter by Location:",
              choices = sort(unique(wq_metadata$station_id_name)),
              multiple = TRUE,
              options = list(plugins = list('remove_button'),
                             placeholder = "Select a station")
            ),
            sliderInput(
              "year_range_dl",
              "Year Range:",
              min = 2020,
              max = 2025,
              value = c(2020, 2025),
              step = 1,
              sep = ""
            ),
            selectizeInput(
              "analyte_download",
              "Analyte:",
              choices = sort(unique(wq_data$analyte)),
              multiple = TRUE,
              options = list(plugins = list('remove_button'),
                             placeholder = "Select an analyte")
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
        tabPanel(
          "Visualize Data",
          fluidRow(column(width = 12,
          div(
            style = "display: flex; gap: 20px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 10px;",
            div(
              style = "min-width: 200px;",
              selectizeInput(
                inputId = "location_filter_g",
                label = tags$strong("Monitoring Site"),
                choices = c(sort(unique(run_designation$map_label))),
                selected = "Clear Creek",
                multiple = TRUE,
                options = list(plugins = list('remove_button'),
                               placeholder = "Select a site")
              )
            ),
            div(
              style = "min-width: 250px;",
              sliderInput(
                "year_range_g",
                "Year Range",
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
                "Data Summary Type",
                choices = c("Monitoring Year", "Month")
              )
            ),
            div(style = "min-width: 200px;", selectInput(
              "data_plot_g",
              "Data Type",
              choices = c("Run Type", "Greb 1L RoSA Genotype")
            )),
          ),

          # --- Map Filter Panel ---
          conditionalPanel(
            condition = "input.which_view_g == 'Map Filter'",
            div(
              style = "display: flex; gap: 20px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 10px;",
              div(
                style = "min-width: 250px;",
                sliderInput(
                  inputId = "year_range2_g",
                  label = tags$strong("Year Range:"),
                  min = as.numeric(min(run_designation$year)),
                  max = as.numeric(max(run_designation$year)),
                  value = c(as.numeric(min(
                    run_designation$year
                  )), as.numeric(max(
                    run_designation$year
                  ))),
                  step = 1,
                  sep = ""
                )
              ),
              div(
                style = "min-width: 200px;",
                selectInput(
                  "plot_type_g",
                  "Data Summary Type",
                  choices = c("Monitoring Year", "Month")
                )
              )
            )
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
        ## Download Gen Data -----
        # TODO currently not very functional
        tabPanel("Download Data", sidebarLayout(
          sidebarPanel(
            width = 4,
            h4("Select Data to Download"),
            tags$hr(),
            selectizeInput(
              "location_filter_g",
              "Filter by Monitoring Site:",
              choices = sort(unique(run_designation$map_label)),
              selected = "Clear Creek",
              multiple = TRUE,
              options = list(plugins = list('remove_button'),
                             placeholder = "Select a site")
            ),
            sliderInput(
              "year_range_g",
              "Year Range:",
              min = 2020,
              max = 2025,
              value = c(2020, 2025),
              step = 1,
              sep = ""
            ),
            selectizeInput(
              "run_download",
              "Run Name:",
              choices = sort(unique(run_designation$run_name)),
              multiple = TRUE,
              options = list(plugins = list('remove_button'),
                             placeholder = "Select run name")
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
            div(style = "margin-top: 10px;", DT::dataTableOutput("g_preview_table"))
          )
        ))
      )
    )
  )
)
