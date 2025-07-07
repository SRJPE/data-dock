server <- function(input, output, session) {
  # welcome modal
  # first visit modal
  showModal(modalDialog(title = "Welcome to the SRJPE Genetics and Water Quality Dashboard!",
                        tagList(
                          tags$h5("NOTE: This tool is in development!"),
                          tags$p("This web-tool is designed to explore model inputs and results for the Spring Run Juvenile Production Estimate."),
                          tags$p("Use the tool to:"),
                          tags$ul(
                            tags$li("Review water quality outputs"),
                            tags$li("Review genetics data outputs"),
                            tags$li("Learn about other SRJPE resources")
                          )
                        ),
                        easyClose = TRUE))
  # Overview visuals
  # output$model_alternatives <- renderImage({
  #   list(src = "data_raw/images/model_alternatives_simplified.png",
  #        width = "50%",
  #        align = "left")
  #
  # }, deleteFile = F)

  # output$map <- renderImage({
  #
  #   list(src = "data_raw/images/map.png",
  #        width = "90%",
  #        align = "left")
  #
  # }, deleteFile = F)

  # Conceptual diagrams

output$genetics_map <- renderLeaflet({
  leaflet() |>
    addMapPane("Lines-Habitat", zIndex = 430) |>
    addTiles(
      urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
      attribution = 'Basemap © Esri, GEBCO, NOAA, CHS, etc.') |>
    addPolylines(
      data = salmonid_habitat_extents,
      label = ~lapply(river, htmltools::HTML),
      popup = ~river,
      color = "#5299D9",
      opacity = 1,
      weight = 1.5) |>
    addCircleMarkers(
      data = rst_sites,
      radius = 6,
      fillColor = "black",
      color = "black",
      weight = 1,
      fillOpacity = 0.7,
      popup = ~popup)
})
}
