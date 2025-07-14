library(shiny)
library(leaflet)

ui <- fluidPage(
  navbarPage(
    "Genetics and Discrete Water Quality Pilot Dashboard",

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
                      imageOutput("map", height = "400px") # Placeholder map
                      )
               )
             ),

## Water Quality Tab ---------------------------------------------------
tabPanel("Water Quality",
         sidebarLayout(
           sidebarPanel(
             h4("Controls"),
             selectInput("region", "Region/site:",
                         choices = c("Carquinez", "Central Delta", "Confluence",
                                     "North Delta", "San Pablo Bay", "South Delta",
                                     "Suisun and Grizzly Bays", "Suisun Marsh")),
             sliderInput("year_range", "Year Range (update min and max years):",
                         min = 2020,
                         max = 2025,
                         value = c(2020, 2022),
                         step = 1,
                         sep = ""),
             selectInput("analyte", "Analyte:",
                         choices = c("Specific Conductance", "Turbidity",
                                     "Dissolved Ammonia", "Chlorophyll a",
                                     "Dissolved Nitrate + Nitrite", "Total Phosphorus")),
             selectInput("data_classification", "Data Classification:",
                         choices = c("Field", "Lab")),
             selectInput("plot_type", "Plot Type:",
                         choices = c("Time Series", "Bar Plot"))

             ),
           mainPanel(
             tabsetPanel(
               tabPanel("Map Placeholder"),
               # tabPanel("Plot Placeholder"), keeping this in case we want more tabs
               # tabPanel("Table Placeholder")
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
                     document.getElementById('modal_overlay').style.display='block';"
               ),
               radioButtons(
                 inputId = "which_view",
                 label = NULL,
                 choices = c("Dropdown Filter", "Map Filter"),
                 inline = TRUE,
                 selected = "Dropdown Filter"
               )
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
                 choices = c("All Locations", sort(unique(run_designation_percent$location_name))),
                 selected = "All Locations"
                 )
               ),
             # Uncomment this section if needed
             # div(
             #   style = "min-width: 200px;",
             #   uiOutput("location_id_ui")
             # ),
             div(
               style = "min-width: 250px;",
               sliderInput(
                 inputId = "year_range",
                 label = tags$strong("Year Range:"),
                 min = 2020,
                 max = 2025,
                 value = c(2020, 2024),
                 step = 1,
                 sep = ""
                 )
               ),
             div(
               style = "min-width: 200px;",
               selectInput(
                 inputId = "plot_type",
                 label = tags$strong("Summarize by:"),
                 choices = c("Run Proportion", "Run Proportions by Month")
                 )
               )
             )
           ),

         # --- Map Filter Panel ---
         conditionalPanel(
           condition = "input.which_view == 'Map Filter'",
           fluidRow(
             column(
               width = 6,
               uiOutput("location_id_ui_map")
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
             width = 12,
             div(
               leafletOutput("genetics_map", height = "600px"),

               # Floating plot panel
               absolutePanel(id = "plot_panel",
                             class = "panel panel-default",
                             top = 80, right = 30, width = 500, draggable = TRUE,
                             style = "z-index:500; background-color:white; padding:10px; border-radius:10px;",
                             h4("Run Composition Plot"),
                             plotOutput("genetics_plot", height = "300px")
                             )
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
