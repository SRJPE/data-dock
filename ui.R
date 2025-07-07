library(shiny)


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
                         min = 2000,
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

## Another Tab ---------------------------------------------------------
tabPanel("Genetics",
         sidebarLayout(
           sidebarPanel(
             h4("Filter Options"),
             p("More filters to come"),

             selectInput("location", "Location:",
                         choices = c("Battle Creek", "Butte Creek", "Clear Creek",
                                     "Deer Creek", "Feather River RM-17", "Feather River RM-61",
                                     "Mill Creek", "Yuba River", "Sacramento River - Knights Landing",
                                     "Sacramento River - Tisdale")),
             sliderInput("year_range", "Year Range (decide if Water Year filter is necessary):",
                         min = 2000,
                         max = 2025,
                         value = c(2020, 2022),
                         step = 1,
                         sep = ""),
             selectInput("plot_type", "Plot Type:",
                         choices = c("Time Series", "Bar Plot"))
             ),
           mainPanel(
             tabsetPanel(
               tabPanel("Map Placeholder (to be updated with genetics data)",
                        leafletOutput("genetics_map", height = "600px"))
               # sub tabs ?
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
