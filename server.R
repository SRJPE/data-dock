server <- function(input, output, session) {
  # welcome modal
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


# Genetics Map

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
        layerId = ~label,  # this enables marker click tracking
        radius = 6,
        color = "black",
        fillOpacity = 0.2,
        popup = ~label)
    })

  # ---- Floating Plot (Triggers on Map Click) ----
  # observeEvent(input$genetics_map_marker_click, {
  #   clicked_site <- input$genetics_map_marker_click$id

  click_marker <- eventReactive(input$genetics_map_marker_click, {
    req(input$which_view == "Map Filter")
    click <- input$genetics_map_marker_click
    print(click)
    return(click$id)
  })

  genetics_filtered_data <- reactive({
    if(input$which_view == "Map Filter") {
    run_designation_percent |>
    filter(map_label %in% click_marker(),
           year >= input$year_range[1],
           year <= input$year_range[2])
    } else if (input$which_view == "Dropdown Filter" & input$location_filter != "All Locations") {
      run_designation_percent |>
        filter(map_label %in% input$location_filter,
               year >= input$year_range[1],
               year <= input$year_range[2])
    } else {
      run_designation_percent |>
        filter(year >= input$year_range[1],
               year <= input$year_range[2])
    }
  })

  genetics_dropdown_filtered_data <- reactive({

  })

  output$genetics_map_plot <- renderPlot({
    req(input$which_view == "Map Filter")
    req(input$plot_type == "Run Proportions by Month")

    if (is.null(input$genetics_map_marker_click)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Click on a Sampling Location\nin Map View to Populate Plot",
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        )

   } else if (nrow(genetics_filtered_data()) == 0) {
      ggplot() +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "No data available",
          size = 6,
          hjust = 0.5
        ) +
        theme_void()
    } else {
      ggplot(genetics_filtered_data(),
             aes(x = sample_event, y = run_percent, fill = run_name)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_viridis_d(option = "D") +
        scale_y_continuous(breaks = seq(0, 100, by = 20)) +
        theme_minimal() +
        labs(fill = "", x = "Sample Event", y = "Percent")
    }
  })
  # ---- Plot for Dropdown Filter ----
  output$genetics_dropdown_plot <- renderPlot({
    req(input$which_view == "Dropdown Filter")
    req(input$plot_type == "Run Proportions by Month")

      if (nrow(genetics_filtered_data()) == 0) {
      ggplot() +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "No data available",
          size = 6,
          hjust = 0.5
        ) +
        theme_void()
    } else {
      ggplot(genetics_filtered_data(),
             aes(x = sample_event, y = run_percent, fill = run_name)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_viridis_d(option = "D") +
        scale_y_continuous(breaks = seq(0, 100, by = 20)) +
        theme_minimal() +
        labs(fill = "", x = "Sample Event", y = "Percent")
    }
  })

  output$dynamic_plot <- renderUI({
    if (input$which_view == "Map Filter") {
      plotOutput("genetics_map_plot")
    } else {
      plotOutput("genetics_dropdown_plot")
    }
  })

  }

