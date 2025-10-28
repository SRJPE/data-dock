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


# Genetics Map v2 ---------------------------------------------------------
  # zoom to selection
  draw_and_zoom_selection_g <- function(sel_names) {
    map <- leafletProxy("g_map") |>
      clearGroup("highlight") |>
      clearPopups()

    # if nothing selected, zoom to full extent
    if (is.null(sel_names) || length(sel_names) == 0) {
      return(
        map |> fitBounds(
          lng1 = min(rst_sites$longitude, na.rm = TRUE),
          lat1 = min(rst_sites$latitude,  na.rm = TRUE),
          lng2 = max(rst_sites$longitude, na.rm = TRUE),
          lat2 = max(rst_sites$latitude,  na.rm = TRUE)
        )
      )
    }

    selected_station_g <- subset(rst_sites, label %in% sel_names)
    if (nrow(selected_station_g) == 0) return(invisible(map))

    # highlight markers
    map <- map |>
      addCircleMarkers(
        data = selected_station_g,
        lat = ~latitude, lng = ~longitude,
        radius = 10,
        fillColor = "#7E2954", color = "white",
        weight = 2, fillOpacity = 0.9,
        group = "highlight",
        label = ~paste("Selected:", label),
        layerId = ~paste0(label, "__hi")
      )

    # Zoom logic
    if (nrow(selected_station_g) == 1) {
      map |>  setView(
        lng = selected_station_g$longitude[1],
        lat = selected_station_g$latitude[1],
        zoom = 11)
    } else {
      map |>  fitBounds(
        lng1 = min(selected_station_g$longitude, na.rm = TRUE),
        lat1 = min(selected_station_g$latitude,  na.rm = TRUE),
        lng2 = max(selected_station_g$longitude, na.rm = TRUE),
        lat2 = max(selected_station_g$latitude,  na.rm = TRUE)
      )
    }
  }

  output$g_map <- renderLeaflet({
    leaflet() |>
      addMapPane("Lines-Habitat", zIndex = 430) |>
      addTiles(
        urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
        attribution = 'Basemap © Esri, GEBCO, NOAA, CHS, etc.') |>
      addPolylines(
        data = salmonid_habitat_extents,
        label = ~lapply(river, htmltools::HTML),
        popup = ~river, color = "#5299D9",
        opacity = 1, weight = 1.5) |>
      addCircleMarkers(
        data = rst_sites,
        layerId = ~label,
        label = ~label,
        radius = 6, stroke = TRUE, weight = 1, color = "black",
        fillOpacity = 0.7,
        #fillColor = ~site_color,
        popup = ~label
      ) |>
      # Initial full-extent view
      fitBounds(
        lng1 = min(rst_sites$longitude, na.rm = TRUE),
        lat1 = min(rst_sites$latitude,  na.rm = TRUE),
        lng2 = max(rst_sites$longitude, na.rm = TRUE),
        lat2 = max(rst_sites$latitude,  na.rm = TRUE)
      )
  })

  observeEvent(input$g_map_marker_click, ignoreInit = TRUE, {
    click <- input$g_map_marker_click
    req(!is.null(click), !is.null(click$id))

    # If highlight markers use the "__hi" suffix, normalize it here
    id_raw <- click$id
    id     <- sub("__hi$", "", id_raw)

    current <- input$location_filter_g
    if (is.null(current)) current <- character(0)

    new_sel <- if (id %in% current) setdiff(current, id) else union(current, id)

    updateSelectInput(session, "location_filter_g", selected = new_sel)
  })

  # redraw highlight + zoom on dropdown change
  observeEvent(input$location_filter_g, {
    draw_and_zoom_selection_g(input$location_filter_g)
  }, ignoreInit = TRUE)


# Genetics Plots v2 -------------------------------------------------------
  filtered_g_data <- reactive({
    req(input$year_range_g)

    data <- run_designation |>
      filter(
        year >= input$year_range_g[1],
        year <= input$year_range_g[2]
      )

    if (!is.null(input$location_filter_g) && !"All Locations" %in% input$location_filter_g) {
      data <- data |> filter(map_label %in% input$location_filter_g)
    }
    if (!is.null(input$genetic_filter_g) && length(input$genetic_filter_g) > 0) {
      data <- data |> filter(run_name %in% input$genetic_filter_g)
    }

    data
  })

  output$download_g_csv <- downloadHandler(
    filename = function() {
      paste0("genetics_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- filtered_g_data()
      readr::write_csv(df, file)
    }
  )

  #wq_missing_sites <- reactiveVal(character(0)) # filtering so message works

  data_for_plot_g <- reactive({
    if (input$plot_type_g == "Monitoring Year") {
      df <- filtered_g_data() |>
        group_by(year, map_label, run_name) |>
        summarize(count = n()) |>
        group_by(year, map_label) |>
        mutate(total_sample = sum(count),
               run_percent = (count/total_sample) * 100)
    }
    if (input$plot_type_g == "Month") {
      sample_event_temp <- run_designation |>
        distinct(map_label, year, month) |>
        arrange(map_label,year, month) |>
        group_by(map_label) |>
        mutate(sample_event2 = row_number())
      df <- filtered_g_data() |>
        group_by(year, map_label, month, run_name) |>
        summarize(count = n()) |>
        group_by(year, map_label, month) |>
        mutate(total_sample = sum(count),
               run_percent = (count/total_sample) * 100) |>
        left_join(sample_event_temp)
    }
    df
  })

  output$g_dynamic_plot <- renderPlotly({
    req(!is.null(input$location_filter_g) && length(input$location_filter_g) > 0)
    df <- data_for_plot_g()

#
#     selected_locs <- input$location_filter_g %||% character(0)
#
#     present_locs <- sort(unique(df$location_name))
#     missing_locs <- setdiff(selected_locs, present_locs)
#
#     prev_missing <- g_missing_sites()
#     new_missing  <- setdiff(missing_locs, prev_missing)
#
#     if (length(new_missing) > 0) {
#       showNotification(
#         paste0("No data for selected analyte(s) at: ",
#                paste(new_missing, collapse = ", ")),
#         type = "warning", duration = 6
#       )
#     }
#
#     # update the tracker (so sites with data don't trigger, and new missing will)
#     wq_missing_sites(missing_locs)
#
#     if (n_distinct(df$analyte) > 8) {
#       validate(
#         need(FALSE, "Too many analytes selected. Please select 8 or fewer.")
#       )
#     }
    if (nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines") |>
               layout(title = "No data available for current selection."))
    }
    run_col <- c("Spring" = "#2E2585", "Winter" = "#94CBEC", "Spring/Winter" = "#5DA899", "Fall" = "#7E2954",
                 "LateFall" = "#C26A77", "Fall/LateFall" = "#9F4A96", "Unknown" = "gray", "Early/Late Heterozygous" = "#DCCD7D")
    if (input$plot_type_g == "Monitoring Year") {
      # plot <- ggplot(df, aes(x = run_name, y = run_percent)) +
      #   geom_boxplot(fill = "#9986A5") +
      #   theme_minimal() +
      #   facet_wrap( ~ map_label, ncol = 1) +
      #   labs(x = "", y = "Percent")
      # sample_event_temp <- run_designation |>
      #   distinct(map_label, year, sample_event) |>
      #   arrange(map_label,year, sample_event) |>
      #   group_by(map_label) |>
      #   mutate(sample_event2 = row_number())
      # df <- filtered_g_data() |>
      #   group_by(year, map_label, sample_event, run_name) |>
      #   summarize(count = n()) |>
      #   group_by(year, map_label, sample_event) |>
      #   mutate(total_sample = sum(count),
      #          run_percent = (count/total_sample) * 100) |>
      #   left_join(sample_event_temp)
      plot <- ggplot(df, aes(x = run_name, y = run_percent, color = year)) +
        #geom_jitter()+
        geom_point() +
        #geom_line() +
        theme_minimal() +
        scale_color_manual(values = c( "#2E2585", "#94CBEC", "#7E2954","#337538")) +
        facet_wrap( ~ map_label, ncol = 1) +
        labs(x = "", y = "Percent", color = "")

    }

    if (input$plot_type_g == "Month") {

      plot <- ggplot(df, aes(x = month, y = run_percent, fill = run_name)) +
        geom_bar(stat = "identity", position = "stack") +
        #scale_fill_viridis_d(option = "D") +
        scale_fill_manual(values = run_col) +
        scale_y_continuous(breaks = seq(0, 100, by = 20)) +
        theme_minimal() +
        facet_wrap( ~ map_label, ncol = 1) +
        labs(fill = "", x = "Sample Event", y = "Percent")
    }
    ggplotly(plot)

  })


  # Genetics Map --------------------------------------------------------------

  # output$genetics_map <- renderLeaflet({
  #   leaflet() |>
  #     addMapPane("Lines-Habitat", zIndex = 430) |>
  #     addTiles(
  #       urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
  #       attribution = 'Basemap © Esri, GEBCO, NOAA, CHS, etc.') |>
  #     addPolylines(
  #       data = salmonid_habitat_extents,
  #       label = ~lapply(river, htmltools::HTML),
  #       popup = ~river,
  #       color = "#5299D9",
  #       opacity = 1,
  #       weight = 1.5) |>
  #     addCircleMarkers(
  #       data = rst_sites,
  #       layerId = ~label,  # this enables marker click tracking
  #       radius = 6,
  #       color = "black",
  #       fillOpacity = 0.2,
  #       popup = ~label)
  #   })

# Genetics Plots ----------------------------------------------------------

  # click_marker <- eventReactive(input$genetics_map_marker_click, {
  #   req(input$which_view_g == "Map Filter")
  #   click <- input$genetics_map_marker_click
  #   print(click)
  #   return(click$id)
  # })
  #
  # observeEvent(input$location_filter_g, {
  #   if ("All Locations" %in% input$location_filter_g && length(input$location_filter_g) > 1) {
  #     updateSelectInput(
  #       session,
  #       "location_filter_g",
  #       selected = setdiff(input$location_filter_g, "All Locations")
  #     )
  #   }
  # })
  #
  #
  # # Types of data:
  # # 1. Summarize run percent by sample event/date - option a. summarize across locations, option b. facet plot and treat each location separate
  # # currently using option a.
  # # 2. Summarize run percent by monitoring year - option a. summarize across locations, option b. facet plot and treat each location separate
  # # currently using option a.
  #
  # genetics_filtered_data_month <- reactive({
  #   if (!is.null(input$genetics_map_marker_click)) {
  #
  #     run_designation |>
  #       filter(map_label %in% click_marker(),
  #              year >= input$year_range2[1],
  #              year <= input$year_range2[2]) |>
  #       group_by(map_label, sample_event, year, run_name) |>
  #       summarize(count = n()) |>
  #       group_by(map_label, year, sample_event) |>
  #       mutate(total_sample = sum(count),
  #              run_percent = (count / total_sample) * 100)
  #   } else if (!is.null(input$location_filter) && !"All Locations" %in% input$location_filter) {
  #
  #     run_designation |>
  #       filter(map_label %in% input$location_filter,
  #              year >= input$year_range1[1],
  #              year <= input$year_range1[2]) |>
  #       group_by(map_label, sample_event, year, run_name) |>
  #       summarize(count = n()) |>
  #       group_by(map_label, year, sample_event) |>
  #       mutate(total_sample = sum(count),
  #              run_percent = (count / total_sample) * 100)
  #   } else {
  #     run_designation |>
  #       filter(year >= input$year_range1[1],
  #              year <= input$year_range1[2]) |>
  #       group_by(map_label, sample_event, year, run_name) |>
  #       summarize(count = n()) |>
  #       group_by(map_label, year, sample_event) |>
  #       mutate(total_sample = sum(count),
  #              run_percent = (count / total_sample) * 100)
  #   }
  # })
  #
  # genetics_filtered_data_year <- reactive({
  #   if(input$which_view == "Map Filter") {
  #     run_designation |>
  #       filter(map_label %in% click_marker(),
  #              year >= input$year_range2[1],
  #              year <= input$year_range2[2]) |>
  #       group_by(map_label, year, run_name) |>
  #       summarize(count = n()) |>
  #       group_by(map_label, year) |>
  #       mutate(total_sample = sum(count),
  #              run_percent = (count / total_sample) * 100)
  #   } else if (input$which_view == "Dropdown Filter" & input$location_filter != "All Locations") {
  #     run_designation |>
  #       filter(map_label %in% input$location_filter,
  #              year >= input$year_range1[1],
  #              year <= input$year_range1[2]) |>
  #       group_by(map_label, year, run_name) |>
  #       summarize(count = n()) |>
  #       group_by(map_label, year) |>
  #       mutate(total_sample = sum(count),
  #              run_percent = (count / total_sample) * 100)
  #   } else {
  #     run_designation |>
  #       filter(year >= input$year_range1[1],
  #              year <= input$year_range1[2]) |>
  #       group_by(map_label, year, run_name) |>
  #       summarize(count = n()) |>
  #       group_by(map_label, year) |>
  #       mutate(total_sample = sum(count),
  #              run_percent = (count / total_sample) * 100)
  #   }
  # })
  #
  # output$genetics_plot_month <- renderPlot({
  #   req(input$plot_type1 == "Run Proportions by Month" | input$plot_type2 == "Run Proportions by Month")
  #
  #   if (input$which_view == "Map Filter" & is.null(input$genetics_map_marker_click)) {
  #     ggplot() +
  #       annotate("text", x = 0.5, y = 0.5, label = "Click on a Sampling Location\nin Map View to Populate Plot",
  #                size = 6, hjust = 0.5, vjust = 0.5) +
  #       theme_void() +
  #       theme(
  #         plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  #       )
  #
  #  } else if (nrow(genetics_filtered_data_month()) == 0) {
  #     ggplot() +
  #       annotate(
  #         "text",
  #         x = 0.5,
  #         y = 0.5,
  #         label = "No data available",
  #         size = 6,
  #         hjust = 0.5
  #       ) +
  #       theme_void()
  #   } else {
  #     plot1 <- ggplot(genetics_filtered_data_month(),
  #                     aes(x = sample_event, y = run_percent, fill = run_name)) +
  #       geom_bar(stat = "identity", position = "stack") +
  #       scale_fill_viridis_d(option = "D") +
  #       scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  #       theme_minimal() +
  #       labs(fill = "", x = "Sample Event", y = "Percent")
  #     plot2 <- ggplot(genetics_filtered_data_month(),
  #                     aes(x = sample_event, y = count, color = run_name)) +
  #       geom_point(size = 4) +
  #       scale_color_viridis_d(option = "D") +
  #       scale_x_continuous(breaks = 1:20) +
  #       scale_y_continuous(breaks = 1:11) +
  #       theme_minimal() +
  #       guides(color = "none") +
  #       labs(fill = "", x = "Sample Event", y = "Sample Count")
  #
  #     plot1 / plot2
  #   }
  # })
  #
  # output$genetics_plot_year <- renderPlot({
  #   req(input$plot_type1 == "Run Proportions" | input$plot_type2 == "Run Proportions" )
  #
  #   if (input$which_view == "Map Filter" & is.null(input$genetics_map_marker_click)) {
  #     ggplot() +
  #       annotate("text", x = 0.5, y = 0.5, label = "Click on a Sampling Location\nin Map View to Populate Plot",
  #                size = 6, hjust = 0.5, vjust = 0.5) +
  #       theme_void() +
  #       theme(
  #         plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  #       )
  #
  #   } else if (nrow(genetics_filtered_data_year()) == 0) {
  #     ggplot() +
  #       annotate(
  #         "text",
  #         x = 0.5,
  #         y = 0.5,
  #         label = "No data available",
  #         size = 6,
  #         hjust = 0.5
  #       ) +
  #       theme_void()
  #   } else {
  #     genetics_filtered_data_year() |>
  #       ggplot(aes(x = run_name, y = run_percent)) +
  #       geom_bar(stat = "identity", fill = "#9986A5") +
  #       geom_text(aes(label = paste0("n=", count), y = 3), size = 3) +
  #       theme_minimal() +
  #       labs(x = "",
  #            y = "Percent")
  #   }
  # })
  #
  # output$genetics_dynamic_plot <- renderUI({
  #   if (input$plot_type1 == "Run Proportions" | input$plot_type2 == "Run Proportions") {
  #     plotOutput("genetics_plot_year", height = "600px")
  #   } else {
  #     plotOutput("genetics_plot_month", height = "600px")
  #   }
  # })

# Water Quality  Map --------------------------------------------------------------

  observe({
    updateSelectizeInput(
      session,
      inputId = "analyte",
      choices = sort(unique(wq_data$analyte)),
      selected = character(0),
      server = TRUE)
    })
# zoom to selection
  draw_and_zoom_selection <- function(sel_names) {
    map <- leafletProxy("wq_map") |>
      clearGroup("highlight") |>
      clearPopups()

    # if nothing selected, zoom to full extent
    if (is.null(sel_names) || length(sel_names) == 0) {
      return(
        map |> fitBounds(
          lng1 = min(wq_metadata$longitude, na.rm = TRUE),
          lat1 = min(wq_metadata$latitude,  na.rm = TRUE),
          lng2 = max(wq_metadata$longitude, na.rm = TRUE),
          lat2 = max(wq_metadata$latitude,  na.rm = TRUE)
          )
      )
    }

    selected_station <- subset(wq_metadata, station_id_name %in% sel_names)
    if (nrow(selected_station) == 0) return(invisible(map))

    # highlight markers
    map <- map |>
      addCircleMarkers(
        data = selected_station,
        lat = ~latitude, lng = ~longitude,
        radius = 10,
        fillColor = "#7E2954", color = "white",
        weight = 2, fillOpacity = 0.9,
        group = "highlight",
        label = ~paste("Selected:", station_id_name),
        layerId = ~paste0(station_id_name, "__hi")
        )

    # Zoom logic
    if (nrow(selected_station) == 1) {
      map |>  setView(
        lng = selected_station$longitude[1],
        lat = selected_station$latitude[1],
        zoom = 11)
      } else {
        map |>  fitBounds(
          lng1 = min(selected_station$longitude, na.rm = TRUE),
          lat1 = min(selected_station$latitude,  na.rm = TRUE),
          lng2 = max(selected_station$longitude, na.rm = TRUE),
          lat2 = max(selected_station$latitude,  na.rm = TRUE)
        )
      }
    }

  output$wq_map <- renderLeaflet({
    leaflet() |>
      addMapPane("Lines-Habitat", zIndex = 430) |>
      addTiles(
        urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
        attribution = 'Basemap © Esri, GEBCO, NOAA, CHS, etc.') |>
      addPolylines(
        data = salmonid_habitat_extents,
        label = ~lapply(river, htmltools::HTML),
        popup = ~river, color = "#5299D9",
        opacity = 1, weight = 1.5) |>
      # Active sites (circles)
      addCircleMarkers(
        data = subset(wq_metadata, status == "Active"),
        layerId = ~station_id_name,
        label = ~paste(station_id_name, "-", station_description),
        radius = 6, stroke = TRUE, weight = 1, color = "black",
        fillOpacity = 0.7,
        fillColor = ~site_color,
        popup = ~paste0("<b>", station_id, "</b><br/>", station_description)
        ) |>
      # Inactive sites (triangles using a custom icon)
      # addMarkers(
      #   data = subset(wq_metadata, status == "Inactive"),
      #   layerId = ~station_id_name,
      #   label = ~paste(station_id_name, "-", station_description),
      #   icon = icons(
      #     iconUrl = "https://upload.wikimedia.org/wikipedia/commons/3/3c/Black_triangle.svg",
      #     iconWidth = 12, iconHeight = 12),
      #   popup = ~paste0("<b>", station_id, "</b><br/>", station_description)) |>
      addCircleMarkers(
        data    = wq_metadata,
        layerId = ~station_id_name,
        label   = ~paste(station_id_name, "-", station_description),
        radius = 6, stroke = TRUE, weight = 1, color = "black",
        fillOpacity = 0.7,
        fillColor = ~ifelse(status == "Active", "black", "gray"),
        popup = ~paste0("<b>", station_id, "</b><br/>", station_description)
      ) |>
      addLegend(
        position = "bottomright",
        colors = c("black", "gray"),
        labels = c("Active Station", "Inactive Station"),
        title = "Station Status",
        opacity = 0.7) |>
      # Initial full-extent view
      fitBounds(
        lng1 = min(wq_metadata$longitude, na.rm = TRUE),
        lat1 = min(wq_metadata$latitude,  na.rm = TRUE),
        lng2 = max(wq_metadata$longitude, na.rm = TRUE),
        lat2 = max(wq_metadata$latitude,  na.rm = TRUE)
        )
    })

  observeEvent(input$wq_map_marker_click, ignoreInit = TRUE, {
    click <- input$wq_map_marker_click
    req(!is.null(click), !is.null(click$id))

    # If highlight markers use the "__hi" suffix, normalize it here
    id_raw <- click$id
    id     <- sub("__hi$", "", id_raw)

    current <- input$location_filter_wq
    if (is.null(current)) current <- character(0)

    new_sel <- if (id %in% current) setdiff(current, id) else union(current, id)

    updateSelectInput(session, "location_filter_wq", selected = new_sel)
  })

  # redraw highlight + zoom on dropdown change
  observeEvent(input$location_filter_wq, {
    draw_and_zoom_selection(input$location_filter_wq)
  }, ignoreInit = TRUE)

filtered_wq_data <- reactive({
  req(input$analyte, length(input$analyte) > 0, input$year_range)

  data <- wq_data |>
    filter(
      analyte %in% input$analyte,
      # data_classification == input$data_classification,
      lubridate::year(date) >= input$year_range[1],
      lubridate::year(date) <= input$year_range[2]
    )

  if (!is.null(input$location_filter_wq) && !"All Locations" %in% input$location_filter_wq) {
    data <- data |> filter(station_id_name %in% input$location_filter_wq)
  }

  data
})

output$download_wq_csv <- downloadHandler(
  filename = function() {
    paste0("water_quality_", Sys.Date(), ".csv")
  },
  content = function(file) {
    df <- filtered_wq_data()
    readr::write_csv(df, file)
  }
)

wq_missing_sites <- reactiveVal(character(0)) # filtering so message works

output$wq_dynamic_plot <- renderPlotly({
  req(!is.null(input$location_filter_wq) && length(input$location_filter_wq) > 0)
  req(input$analyte, length(input$analyte) > 0)

  df <- filtered_wq_data()

  selected_locs <- input$location_filter_wq %||% character(0)

  present_locs <- sort(unique(df$station_id_name))
  missing_locs <- setdiff(selected_locs, present_locs)

  prev_missing <- wq_missing_sites()
  new_missing  <- setdiff(missing_locs, prev_missing)

  if (length(new_missing) > 0) {
    showNotification(
      paste0("No data for selected analyte(s) at: ",
             paste(new_missing, collapse = ", ")),
      type = "warning", duration = 6
    )
  }

  # update the tracker (so sites with data don't trigger, and new missing will)
  wq_missing_sites(missing_locs)

  if (n_distinct(df$analyte) > 8) {
    validate(
      need(FALSE, "Too many analytes selected. Please select 8 or fewer.")
    )
  }
  if (nrow(df) == 0) {
    return(plotly_empty(type = "scatter", mode = "lines") |>
             layout(title = "No data available for current selection."))
  }

  plot_type <- as.character(input$plot_type)[1]
  y_lab <- if (length(input$analyte) == 1) input$analyte[[1]] else "Value"


  # flags + segment ids
  df <- df |>
    dplyr::arrange(analyte, station_id_name, date) |>
    dplyr::group_by(analyte, station_id_name) |>
    dplyr::mutate(nd_flag = tolower(trimws(detection_status)) %in% c("not detected","not detected."),
                  seg_id  = cumsum(dplyr::lag(nd_flag, default = FALSE))
                  ) |>
    dplyr::ungroup()

  # if (plot_type == "Time Series") {
  #   detected <- df |>
  #     dplyr::filter(!is.na(value) & !nd_flag) # splitting detected vs non-detected using the flag
  #
  #   nd <- df |>
  #     dplyr::filter(nd_flag) |>
  #     dplyr::mutate(nd_height = dplyr::case_when(
  #       reports_to == "MDL" ~ as.numeric(mdl),
  #       reports_to == "MRL" ~ as.numeric(mrl),
  #       TRUE ~ NA_real_),
  #       x_minus = date - lubridate::days(10),
  #       x_plus  = date + lubridate::days(10)
  #       ) |>
  #     dplyr::filter(!is.na(nd_height))
  #
  #   # base plot so facets exist
  #   p <- ggplot(df, aes(x = date)) +
  #     facet_wrap(~ analyte, scales = "free_y", ncol = 1) +
  #     labs(x = "", y = y_lab, color = "Location") +
  #     theme_minimal()
  #
  #
  #   # lines/points for detected ONLY, with group resetting after non-detects
  #   if (nrow(detected) > 0) {
  #     p <- p +
  #       geom_line(data = detected,
  #                 aes(y = value,
  #                     color = station_id_name,
  #                     group = interaction(station_id_name, seg_id)), linewidth = 0.6) +
  #       geom_point(data = detected,
  #                  aes(y = value, color = station_id_name),
  #                  size = 1, alpha = 0.6)
  #     }
  #
  #   # Non-detect markers (vertical + short horizontal at MDL/MRL)
  #   if (nrow(nd) > 0) {
  #     p <- p +
  #       geom_segment(data = nd,
  #                    aes(x = date, xend = date, y = 0, yend = nd_height, color = station_id_name),
  #                    linewidth = 0.6, linetype = 5, inherit.aes = FALSE) +
  #       geom_segment(data = nd,
  #                    aes(x = x_minus, xend = x_plus, y = nd_height, yend = nd_height, color = station_id_name),
  #                    linewidth = 0.6, lineend = "square", inherit.aes = FALSE)
  #     }
  #   }

  # normalize unit per analyte
  unit_lu <- df |>
    dplyr::filter(!is.na(unit)) |>
    dplyr::count(analyte, unit, name = "n") |>
    dplyr::arrange(analyte, dplyr::desc(n)) |>
    dplyr::group_by(analyte) |>
    dplyr::slice(1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(analyte, unit_label = unit)

  df_plot <- df |>
    dplyr::left_join(unit_lu, by = "analyte") |>
    dplyr::mutate(
      unit_label   = dplyr::coalesce(unit_label, "unit unknown"),
      analyte_label = paste0(analyte, " (", unit_label, ")")
    )


  df_plot <- df_plot |>
    dplyr::arrange(analyte, station_id_name, date) |>
    dplyr::group_by(analyte, station_id_name) |>
    dplyr::mutate(
      nd_flag = tolower(trimws(detection_status)) %in% c("not detected","not detected."),
      seg_id  = cumsum(dplyr::lag(nd_flag, default = FALSE))
    ) |>
    dplyr::ungroup()

  if (plot_type == "Time Series") {
    detected <- df_plot |>
      dplyr::filter(!is.na(value) & !nd_flag)

    nd <- df_plot |>
      dplyr::filter(nd_flag) |>
      dplyr::mutate(
        nd_height = dplyr::case_when(
          reports_to == "MDL" ~ as.numeric(mdl),
          reports_to == "MRL" ~ as.numeric(mrl),
          TRUE ~ NA_real_),
        x_minus = date - lubridate::days(10),
        x_plus  = date + lubridate::days(10)
        ) |>
      dplyr::filter(!is.na(nd_height))

    # y-axis label: unit if single analyte; otherwise blank (units are in each panel title)
    y_axis_lab <- if (dplyr::n_distinct(df_plot$analyte) == 1) {
      unique(df_plot$unit_label)
    } else {
      NULL
    }

    p <- ggplot(df_plot, aes(x = date)) +
      facet_wrap(~ analyte_label, scales = "free_y", ncol = 1) +
      labs(x = "", y = y_axis_lab, color = "Location") +
      theme_minimal()

    # Tol Muted color palette
    tol_muted <- c(
      "#332288", "#117733", "#44AA99", "#88CCEE",
      "#DDCC77", "#CC6677", "#AA4499", "#882255")

    # lines/points for detected ONLY, with group resetting after non-detects
    if (nrow(detected) > 0) {
      p <- p +
        geom_line(data = detected,
                  aes(y = value,
                      color = station_id_name,
                      group = interaction(station_id_name, seg_id)), linewidth = 0.6, show.legend = FALSE) +
        geom_point(data = detected,
                   aes(y = value,
                       color = station_id_name,
                       shape = station_id_name),
          size = 1.8, alpha = 0.8, show.legend = TRUE)
      }


    # Non-detect markers (vertical + short horizontal at MDL/MRL)
    if (nrow(nd) > 0) {
      p <- p +
        geom_segment(data = nd,
                     aes(x = date, xend = date, y = 0, yend = nd_height, color = station_id_name),
                     linewidth = 0.6, linetype = 5, inherit.aes = FALSE) +
        geom_segment(data = nd,
                     aes(x = x_minus, xend = x_plus, y = nd_height, yend = nd_height, color = station_id_name),
                     linewidth = 0.6, lineend = "square", inherit.aes = FALSE)
    }

    # apply Tol Muted palette, shapes, and unified legend
    p <- p +
      scale_color_manual(values = tol_muted) +
      scale_shape_manual(values = c(16, 17, 15, 3, 7, 8, 9, 10)) +
      guides(color = guide_legend(
        override.aes = list(shape = 16, linetype = 0, size = 2.5, alpha = 1)),
        shape = "none") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
      )


# TODO add units to the box plots (just like line plot)
  } else if (plot_type == "Box Plot") {
    p <- ggplot(
      df |> dplyr::filter(!is.na(value)),
      aes(x = station_id, y = value, fill = station_id)) +
      geom_boxplot(outlier.shape = NA) +
      facet_wrap(~ analyte, scales = "free_y", ncol = 2) +
      labs(x = "", y = "value", fill = "Station") +
      scale_fill_manual(values = tol_muted) +
      theme_minimal() +
      theme(legend.position = "none")

  } else {
    return(NULL)
  }

  gp <- ggplotly(p)

  for (i in seq_along(gp$x$data)) {
    tr <- gp$x$data[[i]]

    # Hide lines from legend
    if (!is.null(tr$mode) && tr$mode == "lines") {
      gp$x$data[[i]]$showlegend <- FALSE
    }
#TODO figure out how to remove duplicate shape symbology
    # enable legend for points
    if (!is.null(tr$mode) && grepl("markers", tr$mode)) {
      gp$x$data[[i]]$showlegend <- TRUE
    }
  }

  # draw legend
  gp <- gp |> layout(showlegend = TRUE)

  gp

})

}

