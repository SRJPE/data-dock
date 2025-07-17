server <- function(input, output, session) {
  # Welcome -----------------------------------------------------------------
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


  # Genetics Map --------------------------------------------------------------

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

# Genetics Plots ----------------------------------------------------------

  click_marker <- eventReactive(input$genetics_map_marker_click, {
    req(input$which_view == "Map Filter")
    click <- input$genetics_map_marker_click
    print(click)
    return(click$id)
  })

  # Types of data:
  # 1. Summarize run percent by sample event/date - option a. summarize across locations, option b. facet plot and treat each location separate
  # currently using option a.
  # 2. Summarize run percent by monitoring year - option a. summarize across locations, option b. facet plot and treat each location separate
  # currently using option a.

  genetics_filtered_data_month <- reactive({
    if(input$which_view == "Map Filter") {
      run_designation |>
        filter(map_label %in% click_marker(),
               year >= input$year_range2[1],
               year <= input$year_range2[2]) |>
        group_by(map_label, sample_event, year, run_name) |>
        summarize(count = n()) |>
        group_by(map_label, year, sample_event) |>
        mutate(total_sample = sum(count),
               run_percent = (count / total_sample) * 100)
    } else if (input$which_view == "Dropdown Filter" & input$location_filter != "All Locations") {
      run_designation |>
        filter(map_label %in% input$location_filter,
               year >= input$year_range1[1],
               year <= input$year_range1[2]) |>
        group_by(map_label, sample_event, year, run_name) |>
        summarize(count = n()) |>
        group_by(map_label, year, sample_event) |>
        mutate(total_sample = sum(count),
               run_percent = (count / total_sample) * 100)
    } else {
      run_designation |>
        filter(year >= input$year_range1[1],
               year <= input$year_range1[2]) |>
        group_by(map_label, sample_event, year, run_name) |>
        summarize(count = n()) |>
        group_by(map_label, year, sample_event) |>
        mutate(total_sample = sum(count),
               run_percent = (count / total_sample) * 100)
    }
  })

  genetics_filtered_data_year <- reactive({
    if(input$which_view == "Map Filter") {
      run_designation |>
        filter(map_label %in% click_marker(),
               year >= input$year_range2[1],
               year <= input$year_range2[2]) |>
        group_by(map_label, year, run_name) |>
        summarize(count = n()) |>
        group_by(map_label, year) |>
        mutate(total_sample = sum(count),
               run_percent = (count / total_sample) * 100)
    } else if (input$which_view == "Dropdown Filter" & input$location_filter != "All Locations") {
      run_designation |>
        filter(map_label %in% input$location_filter,
               year >= input$year_range1[1],
               year <= input$year_range1[2]) |>
        group_by(map_label, year, run_name) |>
        summarize(count = n()) |>
        group_by(map_label, year) |>
        mutate(total_sample = sum(count),
               run_percent = (count / total_sample) * 100)
    } else {
      run_designation |>
        filter(year >= input$year_range1[1],
               year <= input$year_range1[2]) |>
        group_by(map_label, year, run_name) |>
        summarize(count = n()) |>
        group_by(map_label, year) |>
        mutate(total_sample = sum(count),
               run_percent = (count / total_sample) * 100)
    }
  })

  output$genetics_plot_month <- renderPlot({
    req(input$plot_type1 == "Run Proportions by Month" | input$plot_type2 == "Run Proportions by Month")

    if (input$which_view == "Map Filter" & is.null(input$genetics_map_marker_click)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Click on a Sampling Location\nin Map View to Populate Plot",
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        )

   } else if (nrow(genetics_filtered_data_month()) == 0) {
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
      plot1 <- ggplot(genetics_filtered_data_month(),
                      aes(x = sample_event, y = run_percent, fill = run_name)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_viridis_d(option = "D") +
        scale_y_continuous(breaks = seq(0, 100, by = 20)) +
        theme_minimal() +
        labs(fill = "", x = "Sample Event", y = "Percent")
      plot2 <- ggplot(genetics_filtered_data_month(),
                      aes(x = sample_event, y = count, color = run_name)) +
        geom_point(size = 4) +
        scale_color_viridis_d(option = "D") +
        scale_x_continuous(breaks = 1:20) +
        scale_y_continuous(breaks = 1:11) +
        theme_minimal() +
        guides(color = "none") +
        labs(fill = "", x = "Sample Event", y = "Sample Count")

      plot1 / plot2
    }
  })

  output$genetics_plot_year <- renderPlot({
    req(input$plot_type1 == "Run Proportions" | input$plot_type2 == "Run Proportions" )

    if (input$which_view == "Map Filter" & is.null(input$genetics_map_marker_click)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Click on a Sampling Location\nin Map View to Populate Plot",
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        )

    } else if (nrow(genetics_filtered_data_year()) == 0) {
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
      genetics_filtered_data_year() |>
        ggplot(aes(x = run_name, y = run_percent)) +
        geom_bar(stat = "identity", fill = "#9986A5") +
        geom_text(aes(label = paste0("n=", count), y = 3), size = 3) +
        theme_minimal() +
        labs(x = "",
             y = "Percent")
    }
  })

  output$genetics_dynamic_plot <- renderUI({
    if (input$plot_type1 == "Run Proportions" | input$plot_type2 == "Run Proportions") {
      plotOutput("genetics_plot_year", height = "600px")
    } else {
      plotOutput("genetics_plot_month", height = "600px")
    }
  })

# Water Quality  Map --------------------------------------------------------------

output$wq_map <- renderLeaflet({
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
      data = wq_metadata,
      layerId = ~station_id,
      label = ~paste(station_description),
      radius = 6,
      stroke = TRUE,
      weight = 1,
      color = "black",
      fillOpacity = 0.7,
      fillColor = ~ifelse(status == "Active", "#1b9e77", "#d95f02"),  # green vs orange
      popup = ~paste0(
        "<b>", station_description, "</b><br/>",
        "<b>Status:</b> ", status, "<br/>",
        "<b>Station Type:</b> ", station_type, "<br/>",
        "<b>Start Date:</b> ", start_date, "<br/>",
        "<b>End Date:</b> ", end_date)
      )
  })

  click_marker_wq <- eventReactive(input$wq_map_marker_click, {
    req(!is.null(input$wq_map_marker_click))
    input$wq_map_marker_click$id
    })

# adding reactive to zoom into selected site
observeEvent(input$location_filter_wq, {
  req(input$location_filter_wq)

    if (input$location_filter_wq == "All Locations") {
      leafletProxy("wq_map") |>
        clearGroup("highlight") |>
        fitBounds(
          lng1 = min(wq_metadata$longitude, na.rm = TRUE),
          lat1 = min(wq_metadata$latitude, na.rm = TRUE),
          lng2 = max(wq_metadata$longitude, na.rm = TRUE),
          lat2 = max(wq_metadata$latitude, na.rm = TRUE)
          )
      return()
    }

    selected_station <- wq_metadata[wq_metadata$station_id == input$location_filter_wq, ]
    req(nrow(selected_station) == 1)

    leafletProxy("wq_map") |>
      clearGroup("highlight") |>
      addCircleMarkers(
        data = selected_station,
        lat = ~latitude,
        lng = ~longitude,
        radius = 10,
        fillColor = "red",
        color = "white",
        weight = 2,
        fillOpacity = 0.9,
        group = "highlight",
        label = ~paste("Selected:", status, station_type, "Station")) |>
      addPopups(
        lng = selected_station$longitude,
        lat = selected_station$latitude,
        popup = paste0(
          "<b>", selected_station$station_description, "</b><br/>",
          "<b>Status:</b> ", selected_station$status, "<br/>",
          "<b>Type:</b> ", selected_station$station_type, "<br/>",
          "<b>Start Date:</b> ", selected_station$start_date, "<br/>",
          "<b>End Date:</b> ", selected_station$end_date)) |>
      setView(lng = selected_station$longitude,
              lat = selected_station$latitude,
              zoom = 11)
    })

# zoom & highlight when map marker is clicked
observeEvent(input$wq_map_marker_click, {
  clicked_id <- input$wq_map_marker_click$id
  req(clicked_id)

  # Optional: keep dropdown in sync
  updateSelectInput(session, "location_filter_wq", selected = clicked_id)

  selected_station <- wq_metadata[wq_metadata$station_id == clicked_id, ]
  req(nrow(selected_station) == 1)

  leafletProxy("wq_map") |>
    clearGroup("highlight") |>
    addCircleMarkers(
      data = selected_station,
      lat = ~latitude,
      lng = ~longitude,
      radius = 10,
      fillColor = "red",
      color = "white",
      weight = 2,
      fillOpacity = 0.9,
      group = "highlight",
      label = ~paste("<b>Selected:</b>", station_description)) |>
    setView(lng = selected_station$longitude,
            lat = selected_station$latitude,
            zoom = 11)
})
}

