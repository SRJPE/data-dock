server <- function(input, output, session) {
  # welcome modal
  # first visit modal
  showModal(modalDialog(title = "Welcome to the SRJPE Genetics and Water Quality Dashboard!",
                        tagList(
                          tags$h5("NOTE: This tool is in development!"),
                          tags$p("This web-tool is designed to explore..."),
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
        layerId = ~site,  # this enables marker click tracking
        radius = 6,
        fillColor = "black",
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        popup = ~popup)
    })

  # ---- Plot Output Based on Marker Click ----
  observeEvent(input$genetics_map_marker_click, {
    clicked_site <- input$genetics_map_marker_click$id
    output$genetics_plot <- renderPlot({
      req(clicked_site)
      req(input$plot_type == "Bar Plot")

     # map site names
      site_name_lookup <- c(
        "butte" = "Butte",
        "feather-rm17" = "Feather River RM-17",
        "mill creek" = "Mill Creek")
      display_location <- site_name_lookup[[clicked_site]] %||% clicked_site

      # Filter your dataset
      filtered_data <- run_designation_percent |>
        filter(location_name == display_location,
               year >= input$year_range[1],
               year <= input$year_range[2])

      # If there's no data, return an empty message
      if (nrow(filtered_data) == 0) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, hjust = 0.5) +
          theme_void()
        } else {
          ggplot(filtered_data, aes(x = sample_event, y = run_percent, fill = run_name)) +
            geom_bar(stat = "identity", position = "stack") +
            scale_fill_viridis_d(option = "D") +
            scale_y_continuous(breaks = seq(0, 100, by = 20)) +
            theme_minimal() +
            labs(fill = "",
                 x = "Sample Event",
                 y = "Percent")
          }
      })
  })
  }

