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
             h4("Controls (coming soon)")
             ),
           mainPanel(
             tabsetPanel(
               tabPanel("Map Placeholder"),
               tabPanel("Plot Placeholder"),
               tabPanel("Table Placeholder")
               )
             )
           )
         ),

## Another Tab ---------------------------------------------------------
tabPanel("Data Explorer",
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
