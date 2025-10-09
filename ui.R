library(shiny)
library(leaflet)

ui <- fluidPage(
  navbarPage(
    "Genetics and Discrete Water Quality Pilot Dashboard",
    id = "navbar",

    ## Overview Tab --------------------------------------------------------
    tabPanel("Overview",
             fluidRow(
               column(6,
                      h3("What is this project?"),
                      p("Text explaining project..."),
                      h3("Dashboard explanation"),
                      p("Water Quality"),
                      p("Genetics")
                      ),
               column(6,
                      imageOutput("map", height = "400px")
                      )
               )
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
                   label = tags$strong("Filter by Location:"),
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

               #TODO confirm that we will no longer use the sites
               # sidebarLayout(
               #   sidebarPanel(
               #     h4("Controls"),
               #     selectInput("region", "Region/site:",
               #                 choices = c("Carquinez", "Central Delta", "Confluence",
               #                             "North Delta", "San Pablo Bay", "South Delta",
               #                             "Suisun and Grizzly Bays", "Suisun Marsh")),
               div(
                 style = "min-width: 250px;",
                 sliderInput("year_range", "Year Range (update min and max years):",
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
                   label = "Analyte:",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "Select an analyte")
                   )
                 ),
               div(
                 style = "min-width: 200px;",
                 selectInput("plot_type", "Plot Type:",
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
                 width = 12,
                 tags$div(
                   style = "margin-bottom: 10px;",
                   tags$strong("Choose how to filter the data:"),
                   tags$span(
                     class = "help-icon",
                     shiny::icon("question-circle"),
                     style = "cursor: pointer; color: #007BFF; margin-left: 5px;",
                     onclick = "document.getElementById('instructions_modal').style.display='block';
                     document.getElementById('modal_overlay').style.display='block';"),
                   radioButtons(
                     inputId = "which_view",
                     label = NULL,
                     choices = c("Dropdown Filter", "Map Filter"),
                     inline = TRUE,
                     selected = "Dropdown Filter")
                   )
                 )
               ),
             # === Conditional Panels for Filtering ===
             conditionalPanel(
               condition = "input.which_view == 'Dropdown Filter'",
               div(
                 style = "display: flex; gap: 20px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 10px;",
                 div(
                   style = "min-width: 200px;",
                   selectInput(
                     inputId = "location_filter",
                     label = tags$strong("Filter by Location:"),
                     choices = c("All Locations", sort(unique(run_designation$map_label))),
                     selected = "All Locations",
                     multiple = TRUE)
                   ),
                 div(
                   style = "min-width: 250px;",
                   sliderInput(
                     inputId = "year_range1",
                     label = tags$strong("Year Range:"),
                     min = 2020,
                     max = 2025,
                     value = c(2020, 2024),
                     step = 1,
                     sep = "")
                   ),
                 div(
                   style = "min-width: 200px;",
                   selectInput(
                     inputId = "plot_type1",
                     label = tags$strong("Summarize by:"),
                     choices = c("Run Proportions", "Run Proportions by Month")
                     )
                   )
                 )
               ),

             # --- Map Filter Panel ---
             conditionalPanel(
               condition = "input.which_view == 'Map Filter'",
               div(
                 style = "display: flex; gap: 20px; align-items: flex-end; flex-wrap: wrap; margin-bottom: 10px;",
                 div(
                   style = "min-width: 250px;",
                   sliderInput(
                     inputId = "year_range2",
                     label = tags$strong("Year Range:"),
                     min = 2020,
                     max = 2025,
                     value = c(2020, 2024),
                     step = 1,
                     sep = "")
                   ),
                 div(
                   style = "min-width: 200px;",
                   selectInput(
                     inputId = "plot_type2",
                     label = tags$strong("Summarize by:"),
                     choices = c("Run Proportions", "Run Proportions by Month")
                     )
                   )
                 )
               ),

             # === Hidden Modal Overlay ===
             tags$div(
               id = "modal_overlay",
               style = "display:none; position:fixed; top:0; left:0; width:100%; height:100%; background-color:rgba(0, 0, 0, 0.5); z-index:999;",
               onclick = "document.getElementById('instructions_modal').style.display='none';
               document.getElementById('modal_overlay').style.display='none';"
               ),
             tags$div(
               id = "instructions_modal",
               style = "display:none; position:fixed; z-index:1000; left:50%; top:50%; transform:translate(-50%, -50%); background-color:white; padding:20px; border:1px solid #ccc; box-shadow:0px 4px 8px rgba(0,0,0,0.2); width: 500px;",
               tags$div(
                 style = "text-align:right;",
                 tags$button("Close",
                             onclick = "document.getElementById('instructions_modal').style.display='none';
                             document.getElementById('modal_overlay').style.display='none';",
                             style = "background:none; border:none; color:#007BFF; font-weight:bold; cursor:pointer;")
                 ),
               tags$h4("Instructions"),
               tags$p("Filter the data by time and space or view all data. You can filter by monitoring location either by selecting the name in the dropdown menu or by selecting the location of interest in the map. If you would like to use the map as a filter please select Map Filter")
               ),

             # === Map and Floating Plot Panel ===
             fluidRow(
               column(
                 width = 4,
                 leafletOutput("genetics_map", height = "600px")),
               column(width = 8,
                      uiOutput("genetics_dynamic_plot")
                      )
               )
             ),

    ## Download Tab -------------------------------------------------------
    tabPanel(
      "Download Data",
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
        The table below provides a preview only — the exported <code>.csv</code> file will include the complete raw dataset.<br>
        For more information about the data and metadata,
        <a href='link-to-metadata-file' target='_blank'>click here</a>."),
              style = "font-style: italic; color: #555; text-align: center; margin-top: 10px;",
              tags$br(),
              "Files will be exported as .csv.",
              style = "font-style: italic; color: #555;")
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

    ## Resources Tab -------------------------------------------------------
    tabPanel("Resources",
             tabsetPanel(
               tabPanel("Data Repositories",
                        h4("Data Access"),
                        p("Information about data sources...")
               ),
               tabPanel("R Packages",
                        h4("GitHub Resources"),
                        p("List of relevant packages...")
               ),
               tabPanel("Shiny Applications",
                        h4("Shiny Resources"),
                        p("Links to other apps...")
                        )
               )
             )
    )
  )
