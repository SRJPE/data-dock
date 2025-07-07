library(shiny)


ui <- fluidPage(
  navbarPage(
    "Name of Dashboard",

## Overview Tab --------------------------------------------------------
    tabPanel("Overview",
             fluidRow(
               column(6,
                      h3("What is this project?"),
                      p("Text explaining project..."),
                      h3("Model Alternatives"),
                      p("(1) Stock-recruit"),
                      p("Additional explanation.")
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
             h4("Filter Options"),   # Placeholder for future filters
             p("More filters to come")
             ),
           mainPanel(
             tabsetPanel(
               tabPanel("Conceptual Diagram"),
               tabPanel("Additional Panel 1"),
               tabPanel("Additional Panel 2")
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
