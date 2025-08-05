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

  observeEvent(input$location_filter, {
    if ("All Locations" %in% input$location_filter && length(input$location_filter) > 1) {
      updateSelectInput(
        session,
        "location_filter",
        selected = setdiff(input$location_filter, "All Locations")
      )
    }
  })


  # Types of data:
  # 1. Summarize run percent by sample event/date - option a. summarize across locations, option b. facet plot and treat each location separate
  # currently using option a.
  # 2. Summarize run percent by monitoring year - option a. summarize across locations, option b. facet plot and treat each location separate
  # currently using option a.

  genetics_filtered_data_month <- reactive({
    if (!is.null(input$genetics_map_marker_click)) {

      run_designation |>
        filter(map_label %in% click_marker(),
               year >= input$year_range2[1],
               year <= input$year_range2[2]) |>
        group_by(map_label, sample_event, year, run_name) |>
        summarize(count = n()) |>
        group_by(map_label, year, sample_event) |>
        mutate(total_sample = sum(count),
               run_percent = (count / total_sample) * 100)
    } else if (!is.null(input$location_filter) && !"All Locations" %in% input$location_filter) {

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

  observe({
    updateSelectizeInput(
      session,
      inputId = "analyte",
      choices = sort(unique(wq_data$analyte)),
      selected = character(0),
      server = TRUE
    )
  })


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
      fillColor = ~ifelse(status == "Active", "black", "gray"),
      popup = ~paste0(
        "<b>", station_description, "</b><br/>"
        # ,
        # "<b>Status:</b> ", status, "<br/>",
        # "<b>Station Type:</b> ", station_type, "<br/>",
        # "<b>Start Date:</b> ", start_date, "<br/>",
        # "<b>End Date:</b> ", end_date
        )
      )|>
    addLegend(
      position = "bottomright",
      colors = c("black", "gray"),
      labels = c("Active Station", "Inactive Station"),
      title = "Station Status",
      opacity = 0.7
    )
  })

  click_marker_wq <- eventReactive(input$wq_map_marker_click, {
    req(!is.null(input$wq_map_marker_click))
    input$wq_map_marker_click$id
    })

# adding reactive to zoom into selected site
  observeEvent(input$location_filter_wq, {
    req(input$location_filter_wq)

    # If "All Locations" is selected (and only it), reset the map
    if (identical(input$location_filter_wq, "All Locations")) {
      leafletProxy("wq_map") |>
        clearGroup("highlight") |>
        clearPopups() |>
        fitBounds(
          lng1 = min(wq_metadata$longitude, na.rm = TRUE),
          lat1 = min(wq_metadata$latitude, na.rm = TRUE),
          lng2 = max(wq_metadata$longitude, na.rm = TRUE),
          lat2 = max(wq_metadata$latitude, na.rm = TRUE))
      return()
    }

    # If "All Locations" is selected alongside others, remove it from the selection
    if ("All Locations" %in% input$location_filter_wq && length(input$location_filter_wq) > 1) {
      updateSelectInput(
        session,
        "location_filter_wq",
        selected = setdiff(input$location_filter_wq, "All Locations"))
      return()
    }

    # Filter selected stations
    selected_station <- wq_metadata[wq_metadata$station_id %in% input$location_filter_wq, ]
    req(nrow(selected_station) > 0)
    bbox <- sf::st_bbox(selected_station)

    map <- leafletProxy("wq_map") |>
      clearGroup("highlight") |>
      clearPopups() |>
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
        label = ~paste("Selected:", status, station_type, "Station"))

    # Apply different zoom logic depending on number of selected sites
    if (nrow(selected_station) == 1) {
      map |>
        setView(
          lng = selected_station$longitude,
          lat = selected_station$latitude,
          zoom = 11
        ) |>
        addPopups(
          lng = selected_station$longitude,
          lat = selected_station$latitude,
          popup = paste0(
            "<b>", selected_station$station_description, "</b><br/>"
            # ,
            # "<b>Status:</b> ", selected_station$status, "<br/>",
            # "<b>Type:</b> ", selected_station$station_type, "<br/>",
            # "<b>Start Date:</b> ", selected_station$start_date, "<br/>",
            # "<b>End Date:</b> ", selected_station$end_date
            ))
      } else {

        map |>
          fitBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]) |>
          addPopups(
            lng = selected_station$longitude,
            lat = selected_station$latitude,
            popup = paste0(
              "<b>", selected_station$station_description, "</b><br/>"
              # ,
              # "<b>Status:</b> ", selected_station$status, "<br/>",
              # "<b>Type:</b> ", selected_station$station_type, "<br/>",
              # "<b>Start Date:</b> ", selected_station$start_date, "<br/>",
              # "<b>End Date:</b> ", selected_station$end_date
              ))
      }
    })


# zoom & highlight when map marker is clicked
observeEvent(input$wq_map_marker_click, {
  clicked_id <- input$wq_map_marker_click$id
  req(clicked_id)

  # Optional: keep dropdown in sync
  current <- setdiff(input$location_filter_wq, "All Locations")
  new_selection <- union(current, clicked_id)
  updateSelectInput(session, "location_filter_wq", selected = new_selection)

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

filtered_wq_data <- reactive({
  req(input$analyte, input$year_range)

  data <- wq_data |>
    filter(
      analyte == input$analyte,
      # data_classification == input$data_classification,
      lubridate::year(date) >= input$year_range[1],
      lubridate::year(date) <= input$year_range[2]
    )

  if (!is.null(input$location_filter_wq) && !"All Locations" %in% input$location_filter_wq) {
    data <- data |> filter(station_id %in% input$location_filter_wq)
  }

  data
})

output$wq_dynamic_plot <- renderPlotly({
  req(input$analyte)
  req(!is.null(input$location_filter_wq) && length(input$location_filter_wq) > 0)

  df <- filtered_wq_data()
  if (nrow(df) == 0) {
    return(plotly_empty(type = "scatter", mode = "lines") |>
             layout(title = "No data available for current selection."))
  }

  df <- df |> arrange(station_description, date)

  # plot options so far
  p <- if (input$plot_type == "Time Series") {
    ggplot(df |> filter(!is.na(value)), aes(x = date, y = value, color = station_description)) +
      geom_line() +
      geom_point(size = 1, alpha = 0.6) +
      labs(title = paste(input$analyte, "Over Time"),
           x = "Date",
           y = "Value",
           color = "Location")
    } else if (input$plot_type == "Box Plot") {
      ggplot(df |> filter(!is.na(value)), aes(x = station_description, y = value, fill = station_description)) +
        geom_boxplot(outlier.shape = NA) +
        coord_flip() +
        scale_y_log10() +
        labs(title = paste("Distribution of", input$analyte),
             x = "Location",
             y = "Value (log scale)")
      } else {
        return(NULL)
        }

  ggplotly(p)

})

}

