server <- function(input, output, session) {
  # welcome modal
  # first visit modal
  showModal(modalDialog(title = "Welcome to the Genetics and Water Quality Dashboard!",
                        tagList(
                          tags$h5("NOTE: This tool is in development!"),
                          tags$p("This web-tool is designed to explore..."),
                          tags$p("Use the tool to:"),
                          tags$ul(
                            tags$li("Review water quality outputs"),
                            tags$li("Review genetics data outputs"),
                            tags$li("Learn about other resources")
                            )
                          ),
                        easyClose = TRUE))


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
        color = "black",
        fillOpacity = 0.2,
        popup = ~label)
    })

  # ---- Floating Plot (Triggers on Map Click) ----
  observeEvent(input$genetics_map_marker_click, {
    clicked_site <- input$genetics_map_marker_click$id

    output$genetics_plot <- renderPlot({
      req(clicked_site)
      req(input$plot_type == "Bar Plot")

      # Optional: map site ID to display name
      location_lookup <- c("butte" = "Butte", "mill creek" = "Mill Creek")
      location_name <- location_lookup[[clicked_site]] %||% clicked_site

      filtered <- run_designation_percent |>
        filter(location_name == location_name,
               year >= input$year_range[1],
               year <= input$year_range[2])
      if (nrow(filtered) == 0) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, hjust = 0.5) +
          theme_void()
      } else {
        ggplot(filtered, aes(x = sample_event, y = run_percent, fill = run_name)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_viridis_d(option = "D") +
          scale_y_continuous(breaks = seq(0, 100, by = 20)) +
          theme_minimal() +
          labs(fill = "", x = "Sample Event", y = "Percent")
        }
      })
    })
  # ---- (Optional) Plot for Attribute Filter ----
  observeEvent(input$location_id, {
    req(input$which_view == "Dropdown Filter")
    req(input$location_id)

    output$genetics_plot <- renderPlot({
      selected_label <- input$location_id
      selected_data <- run_designation_percent  |>
        mutate(label = as.charachter(sample_event))  |>
        filter(label == selected_label)

      if (nrow(selected_data) == 0) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6, hjust = 0.5) +
          theme_void()
        } else {
          ggplot(selected_data, aes(x = sample_event, y = run_percent, fill = run_name)) +
            geom_bar(stat = "identity", position = "stack") +
            scale_fill_viridis_d(option = "D") +
            scale_y_continuous(breaks = seq(0, 100, by = 20)) +
            theme_minimal() +
            labs(fill = "", x = "Sample Event", y = "Percent")
          }
      })
    })
  # adding just to display example
  # output$genetics_plot <- renderPlot({
  #   req(input$which_view == "Attribute Filter")
  #   if (input$location_filter == "Butte" && input$plot_type == "Run Proportion") {
  #     return(plot1)
  #   }
  #
  #   ggplot() +
  #     annotate("text", x = 0.5, y = 0.5, label = "Select a valid filter", size = 5) +
  #     theme_void()
  # })
  }

