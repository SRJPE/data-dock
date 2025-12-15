library(shiny)
library(leaflet)

ui <- fluidPage(
  navbarPage(
    "Genetics and Discrete Water Quality Pilot Dashboard",
    id = "navbar",

    ## Overview Tab --------------------------------------------------------
    tabPanel("Overview",
               tags$iframe(src = "overview.html", style = "height:1200px; width:100%", frameborder = "0")
               ),
    ## Water Quality Tab ---------------------------------------------------
    tabPanel("Water Quality",
             fluidRow(
               column(
                 width = 12,)
               ),
             div(
               style = "display: flex; gap: 20px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 10px;",
               div(
                 style = "min-width: 200px;",
                 selectInput(
                   inputId = "location_filter_wq",
                   label = tags$strong("Station"),
                   choices = c(
                     setNames(
                       # wq_metadata$station_id[order(wq_metadata$station_description)],
                       # wq_metadata$station_description[order(wq_metadata$station_description)]
                       wq_metadata$station_id_name[order(wq_metadata$station_id_name)],
                       wq_metadata$station_id_name[order(wq_metadata$station_id_name)]
                       )
                     ),
                   selected = NULL,
                   multiple = TRUE,
                   selectize = TRUE)
                 ),
               div(
                 style = "min-width: 250px;",
                 sliderInput("year_range", "Year Range",
                             min = 2020,
                             max = 2025,
                             value = c(2020, 2025),
                             step = 1,
                             sep = "")
                 ),
               div(
                 style = "min-width: 200px;",
                 selectizeInput(
                   inputId = "analyte",
                   label = "Analyte",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "Select an analyte")
                   )
                 ),
               div(
                 style = "min-width: 200px;",
                 selectInput("plot_type", "Plot Type",
                             choices = c("Time Series", "Box Plot"))
                 )
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
                     sep = "")
                   ),
                 div(
                   style = "min-width: 200px;",
                   selectInput("plot_type", "Plot Type:",
                               choices = c("Time Series", "Box Plot"))
                   )
                 )
               ),

             # === Map and Floating Plot Panel ===
             fluidRow(
               column(width = 4,
                      leafletOutput("wq_map", height = "600px")
                      ),
               column(width = 8,
                      plotlyOutput("wq_dynamic_plot", height = "600px"),
                      tags$p("Note: Vertical dashed lines with a short horizontal cap indicate non-detect values.
                             Their height corresponds to the reporting limit (MDL/MRL).",
                             style = "font-size: 0.9em; font-style: italic; color: #555; margin-top:5px;")
                      )
               ),
             fluidRow(
               column(width = 12,
                      div(style = "margin-top: 20px; text-align: right;",
                          downloadBttn("download_wq_csv", "Download Selected Data",
                                       style = "unite", color = "primary", size = "sm"),
                          tags$p(
                            tags$span("Download the data currently selected in the map and filters."),
                            tags$br(),
                            tags$span("For custom queries, visit the Download tab."),
                            style = "margin-bottom: 5px; font-style: italic; color: #555;")
                          )
                      )
               )
             ),

    ## Genetics Tab ---------------------------------------------------------
    tabPanel("Genetics",
             fluidRow(
               column(
                 width = 12,)
             ),
             div(
               style = "display: flex; gap: 20px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 10px;",
               div(
                 style = "min-width: 200px;",
                 selectInput(
                   inputId = "location_filter_g",
                   label = tags$strong("Monitoring Site"),
                   choices = c(
                     setNames(
                       run_designation$map_label[order(run_designation$map_label)],
                       run_designation$map_label[order(run_designation$map_label)]
                     )
                   ),
                   selected = "Clear Creek",
                   multiple = TRUE,
                   selectize = TRUE)
               ),
               div(
                 style = "min-width: 250px;",
                 sliderInput("year_range_g", "Year Range",
                             min = as.numeric(min(run_designation$year)),
                             max = as.numeric(max(run_designation$year)),
                             value = c(as.numeric(min(run_designation$year)), max(run_designation$year)),
                             step = 1,
                             sep = "",
                             ticks = TRUE)
               ),
               div(
                 style = "min-width: 200px;",
                 selectInput("plot_type_g", "Data Summary Type",
                             choices = c("Monitoring Year", "Month"))
               ),
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
                     value = c(as.numeric(min(run_designation$year)), as.numeric(max(run_designation$year))),
                     step = 1,
                     sep = "")
                 ),
                 div(
                   style = "min-width: 200px;",
                   selectInput("plot_type_g", "Data Summary Type",
                               choices = c("Monitoring Year", "Month"))
                 )
               )
             ),

             # === Map and Floating Plot Panel ===
             fluidRow(
               column(width = 4,
                      leafletOutput("g_map", height = "600px")
               ),
               column(width = 8,
                      plotlyOutput("g_dynamic_plot", height = "600px"),
                      tags$p("Note: The year is representative of a monitoring year (Nov-May), see [data collection methods](insert link) for more detail.",
                             style = "font-size: 0.9em; font-style: italic; color: #555; margin-top:5px;")
               )
             ),
             fluidRow(
               column(width = 12,
                      div(style = "margin-top: 20px; text-align: right;",
                          downloadBttn("download_g_csv", "Download Selected Data",
                                       style = "unite", color = "primary", size = "sm"),
                          tags$p(
                            tags$span("Download the data currently selected in the map and filters."),
                            tags$br(),
                            tags$span("For custom queries, visit the Download tab."),
                            style = "margin-bottom: 5px; font-style: italic; color: #555;")
                      )
               )
             )
    ),

    ## Download Tab -------------------------------------------------------
    tabPanel(
      "Download WQ Data",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h4("Select Data to Download"),
          tags$hr(),
          selectInput(
            "location_filter_dl", "Filter by Location:",
            choices = wq_metadata$station_id_name,
            multiple = TRUE),
          sliderInput(
            "year_range_dl", "Year Range:",
            min = 2020, max = 2025,
            value = c(2020, 2025),
            step = 1,
            sep = ""),
          selectizeInput(
            "analyte_download", "Analyte:",
            choices = sort(unique(wq_data$analyte)),
            multiple = TRUE,
            options = list(placeholder = "Select analyte")
            ),
          checkboxInput(
            inputId = "include_weather",
            label = "Include associated weather observations (Rain, Sky Conditions)",
            value = FALSE),
          tags$hr(),
          div(
            style = "margin-top: 10px; text-align: center;",
            downloadBttn(
              "download_wq_csv_dl",
              "Download Selected Data",
              style = "unite",
              color = "primary",
              size = "lg"),
            tags$p(
              HTML("Download the data you’ve selected using the filters on this tab.<br>
        The table provides a preview only — the exported <code>.csv</code> file will include the complete raw dataset.<br>
        For more information about the data and metadata, please visit the
        <a href='link-to-metadata-file' target='_blank'>EDI package here</a>."), #TODO add link to EDI
              style = "font-style: italic; color: #555; text-align: center; margin-top: 10px;")
            )
          ),

        # Data preview table
        mainPanel(
          width = 8,
          h3("Preview of Selected Data"),
          tags$p(
            "This table updates automatically when you change filters in the sidebar.",
            style = "font-style: italic; color: #555; margin-bottom: 10px;"),
          div(
            style = "margin-top: 10px;",
            DT::dataTableOutput("dl_preview_table")
            )
          )
        )
      ),
    )
  )
