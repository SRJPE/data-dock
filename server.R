server <- function(input, output, session) {
  # Welcome -----------------------------------------------------------------
  # DELETE WHEN FINALIZED
  showModal(
    modalDialog(
      title = "Welcome to the Downstream Dashboard!",
      tagList(
        tags$h5("NOTE: This tool is in development!")
      ),
      easyClose = TRUE
    )
  )

  # Genetics Map v2 ---------------------------------------------------------
  # zoom to selection
  draw_and_zoom_selection_g <- function(sel_names) {
    map <- leafletProxy("g_map") |>
      clearGroup("highlight") |>
      clearPopups()

    # if nothing selected, zoom to full extent
    if (is.null(sel_names) || length(sel_names) == 0) {
      return(map |> fitBounds(
        lng1 = min(rst_sites$longitude, na.rm = TRUE),
        lat1 = min(rst_sites$latitude, na.rm = TRUE),
        lng2 = max(rst_sites$longitude, na.rm = TRUE),
        lat2 = max(rst_sites$latitude, na.rm = TRUE)
      ))
    }

    selected_station_g <- subset(rst_sites, label %in% sel_names)
    if (nrow(selected_station_g) == 0)
      return(invisible(map))

    # highlight markers
    map <- map |>
      addCircleMarkers(
        data = selected_station_g,
        lat = ~ latitude,
        lng = ~ longitude,
        radius = 10,
        fillColor = "#7E2954",
        color = "white",
        weight = 2,
        fillOpacity = 0.9,
        group = "highlight",
        label = ~ paste("Selected:", label),
        layerId = ~ paste0(label, "__hi")
      )

    # Zoom logic
    if (nrow(selected_station_g) == 1) {
      map |>  setView(
        lng = selected_station_g$longitude[1],
        lat = selected_station_g$latitude[1],
        zoom = 11
      )
    } else {
      map |>  fitBounds(
        lng1 = min(selected_station_g$longitude, na.rm = TRUE),
        lat1 = min(selected_station_g$latitude, na.rm = TRUE),
        lng2 = max(selected_station_g$longitude, na.rm = TRUE),
        lat2 = max(selected_station_g$latitude, na.rm = TRUE)
      )
    }
  }

  output$g_map <- renderLeaflet({
    leaflet() |>
      addMapPane("Lines-Habitat", zIndex = 430) |>
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}", attribution = 'Basemap © Esri, GEBCO, NOAA, CHS, etc.') |>
      addPolylines(
        data = salmonid_habitat_extents,
        label = ~ lapply(river, htmltools::HTML),
        popup = ~ river,
        color = "#5299D9",
        opacity = 1,
        weight = 1.5
      ) |>
      addCircleMarkers(
        data = rst_sites,
        layerId = ~ label,
        label = ~ label,
        radius = 6,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        fillColor = ifelse(rst_sites$stream == "clear creek", "#7E2954", "black"),
        #default highlight to Clear
        popup = ~ label
      ) |>
      # Initial full-extent view
      fitBounds(
        lng1 = min(rst_sites$longitude, na.rm = TRUE),
        lat1 = min(rst_sites$latitude, na.rm = TRUE),
        lng2 = max(rst_sites$longitude, na.rm = TRUE),
        lat2 = max(rst_sites$latitude, na.rm = TRUE)
      ) |>
      htmlwidgets::onRender("
      function(el, x){
        this.zoomControl.setPosition('bottomright');
      }
    ")
  })

  observeEvent(input$g_map_marker_click, ignoreInit = TRUE, {
    click <- input$g_map_marker_click
    req(!is.null(click), !is.null(click$id))

    # If highlight markers use the "__hi" suffix, normalize it here
    id_raw <- click$id
    id     <- sub("__hi$", "", id_raw)

    current <- input$location_filter_g
    if (is.null(current))
      current <- character(0)

    new_sel <- if (id %in% current)
      setdiff(current, id)
    else
      union(current, id)

    updateSelectizeInput(session, "location_filter_g", selected = new_sel)

    # updateSelectInput(session, "location_filter_g", selected = new_sel)
  })

  # redraw highlight + zoom on dropdown change
  observeEvent(input$location_filter_g, {
    draw_and_zoom_selection_g(input$location_filter_g)
  }, ignoreInit = TRUE)


  # Genetics Plots v2 -------------------------------------------------------
  filtered_g_data <- reactive({
    req(input$year_range_g)
    req(input$location_filter_g)

    data <- run_designation |>
      filter(year >= input$year_range_g[1], year <= input$year_range_g[2])

    if (!is.null(input$location_filter_g) &&
        !"All Locations" %in% input$location_filter_g) {
      data <- data |> filter(map_label %in% input$location_filter_g)
    }
    if (input$data_plot_g == "Run Type") {
      data <- data |>  filter(run_name != "greb1l heterozygote") # per instructions from sean/melinda remove heterozygotes from the run type plot
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

  data_for_plot_g <- reactive({
    if (input$data_plot_g == "Greb 1L RoSA Genotype") {
      grouping_variable <- "genotype"
    }
    if (input$data_plot_g == "Run Type") {
      grouping_variable <- "run_name"
    }
    if (input$plot_type_g == "Monitoring Year") {
      df <- filtered_g_data() |>
        group_by(year, map_label, .data[[grouping_variable]]) |>
        summarize(count = n()) |>
        group_by(year, map_label) |>
        mutate(
          total_sample = sum(count),
          run_percent = (count / total_sample) * 100
        )

    }
    if (input$plot_type_g == "Month") {
      df <- filtered_g_data() |>
        filter(!is.na(month)) |>
        group_by(year, month, location_name, .data[[grouping_variable]]) |>
        summarise(total_samples = n(), .groups = "drop") |>
        group_by(year, month, location_name) |>
        mutate(
          site_total = sum(total_samples),
          run_percent = (total_samples / site_total) * 100
        ) |>
        ungroup() |>
        # code to fix the plot so that it starts from monitoring year
        mutate(
          fake_year = ifelse(month %in% 10:12, "1991", "1992"),
          fake_date = as.Date(paste0(fake_year, "-", month, "-01"))
        ) |>
        select(-fake_year) |>
        mutate(month = factor(month, levels = 1:12, labels = month.abb)) |>
        # filter(run_name != "Unknown") |>  # removing unknowns for now - plot will no longer be at a 100%
        complete(
          location_name,
          year,
          month,
          fake_date,
          .data[[grouping_variable]],
          fill = list(run_percent = 0, site_total = 0)
        )
    } # adding this so when years are not present at a given location, there is still a facet (empty) for that year
    df
  })

  output$g_dynamic_plot <- renderPlotly({

    df <- data_for_plot_g()


    if (nrow(df) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "lines") |>
          layout(title = "No data available for current selection.")
      )
    }

    run_col <- c(
      "spring" = "#337538",
      "fall or late fall" = "#DCCD7D",
      "winter" = "#94CBEC",
      "early" = "#5DA899",
      "late" =  "#DCCD7D",
      "heterozygote" = "gray"
    )

    if (input$data_plot_g == "Greb 1L RoSA Genotype") {
      grouping_variable <- "genotype"
      title_text <- "Genotype Proportion: "
      title_text2 <- "Genotype: "
      y_axis_text <- "Genotype Proportions"
    }
    if (input$data_plot_g == "Run Type") {
      grouping_variable <- "run_name"
      title_text <- "Run Assignment Proportion: "
      title_text2 <- "Run Type: "
      y_axis_text <- "Run Assignment Proportions"
    }

    if (input$plot_type_g == "Monitoring Year") {
      plot <- ggplot(df,
                     aes(
                       x = year,
                       y = run_percent,
                       fill = .data[[grouping_variable]],
                       text = paste0(
                         "Monitoring Year: ",
                         year,
                         "<br>",
                         title_text,
                         signif(run_percent, 2),
                         "<br>",
                         title_text2,
                         .data[[grouping_variable]],
                         "<br>",
                         "Sample Size: ",
                         total_sample
                       )
                     )) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap( ~ map_label, ncol = 1) +
        scale_fill_manual(values = run_col) +
        theme_minimal() +
        labs(x = "", y = y_axis_text, fill = "")
    }

    if (input$plot_type_g == "Month") {
      selected_years <- seq(input$year_range_g[1], input$year_range_g[2])
      if (length(selected_years) > 3) {
        showNotification("Please select a range of 3 years or fewer.", type = "error")
        return(
          plotly_empty(type = "scatter", mode = "lines") |>
            layout(
              title = "Too many years selected. Please select 3 years max.<br>
                   Recommend summarizing by monitoring year when looking at more than 3 years."
            )
        )
      }
      n_years <- length(unique(df$year))
      plot <- ggplot(df,
                     aes(
                       x = fake_date,
                       y = run_percent,
                       fill = .data[[grouping_variable]],
                       text = paste0(
                         "Monitoring Year: ",
                         year,
                         "<br>",
                         title_text,
                         signif(run_percent, 2),
                         "<br>",
                         title_text2,
                         .data[[grouping_variable]],
                         "<br>",
                         "Sample Size: ",
                         site_total
                       )
                     )) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap( ~ location_name + year, ncol = n_years) +
        scale_fill_manual(name = "Run type", values = run_col) +
        labs(x = "", y = y_axis_text) +
        theme_minimal() +
        theme(axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )) +
        # add labels
        scale_x_date(breaks = "1 month", date_labels = "%b")
    }
    ggplotly(plot, tooltip = "text")

  })


  # Water Quality  Map --------------------------------------------------------------

  observeEvent(input$location_filter_wq, {
    sel_stations <- input$location_filter_wq

    analyte_choices <-
      if (is.null(sel_stations) || length(sel_stations) == 0) {
        sort(unique(wq_data$analyte))
      } else {
        wq_data |>
          dplyr::filter(station_id_name %in% sel_stations) |>
          dplyr::pull(analyte) |>
          unique() |>
          sort()
      }

    updateSelectizeInput(
      session,
      inputId = "analyte",
      choices  = analyte_choices,
      selected = NULL,
      server   = TRUE
    )
  }
  )

  # keep date range in sync with year_range (main controls)
  # This makes the year slider behave like a shortcut preset for the exact date range

  sync_lock_main <- reactiveVal(FALSE)
  sync_lock_map  <- reactiveVal(FALSE)

  observeEvent(input$year_range, {
    if (isTRUE(sync_lock_main()))
      return()

    sync_lock_main(TRUE)
    on.exit(sync_lock_main(FALSE), add = TRUE)

    yr <- input$year_range
    updateDateRangeInput(
      session,
      "date_range_wq",
      start = as.Date(sprintf("%d-01-01", yr[1])),
      end   = as.Date(sprintf("%d-12-31", yr[2]))
    )
  }, ignoreInit = TRUE)

  observeEvent(input$date_range_wq, {
    if (isTRUE(sync_lock_main()))
      return()

    dr <- input$date_range_wq
    req(!is.null(dr), length(dr) == 2, all(!is.na(dr)))

    # Convert chosen dates into years
    y1 <- as.integer(format(as.Date(dr[1]), "%Y"))
    y2 <- as.integer(format(as.Date(dr[2]), "%Y"))

    # If same as current slider, do nothing (avoids unnecessary updates)
    cur <- input$year_range
    if (!is.null(cur) &&
        length(cur) == 2 && identical(c(y1, y2), as.integer(cur)))
      return()

    sync_lock_main(TRUE)
    on.exit(sync_lock_main(FALSE), add = TRUE)

    updateSliderInput(session, "year_range", value = c(y1, y2))
  }, ignoreInit = TRUE)

  observeEvent(input$year_range2, {
    if (isTRUE(sync_lock_map()))
      return()

    sync_lock_map(TRUE)
    on.exit(sync_lock_map(FALSE), add = TRUE)

    yr <- input$year_range2
    updateDateRangeInput(
      session,
      "date_range_wq2",
      start = as.Date(sprintf("%d-01-01", yr[1])),
      end   = as.Date(sprintf("%d-12-31", yr[2]))
    )
  }, ignoreInit = TRUE)

  # slider
  observeEvent(input$date_range_wq2, {
    if (isTRUE(sync_lock_map()))
      return()

    dr <- input$date_range_wq2
    req(!is.null(dr), length(dr) == 2, all(!is.na(dr)))

    y1 <- as.integer(format(as.Date(dr[1]), "%Y"))
    y2 <- as.integer(format(as.Date(dr[2]), "%Y"))

    cur <- input$year_range2
    if (!is.null(cur) &&
        length(cur) == 2 && identical(c(y1, y2), as.integer(cur)))
      return()

    sync_lock_map(TRUE)
    on.exit(sync_lock_map(FALSE), add = TRUE)

    updateSliderInput(session, "year_range2", value = c(y1, y2))
  }, ignoreInit = TRUE)

  # zoom to selection
  draw_and_zoom_selection <- function(sel_names) {
    map <- leafletProxy("wq_map") |>
      clearGroup("highlight") |>
      clearPopups()

    # if nothing selected, zoom to full extent
    if (is.null(sel_names) || length(sel_names) == 0) {
      return(map |> fitBounds(
        lng1 = min(wq_metadata$longitude, na.rm = TRUE),
        lat1 = min(wq_metadata$latitude, na.rm = TRUE),
        lng2 = max(wq_metadata$longitude, na.rm = TRUE),
        lat2 = max(wq_metadata$latitude, na.rm = TRUE)
      ))
    }

    selected_station <- subset(wq_metadata, station_id_name %in% sel_names)
    if (nrow(selected_station) == 0)
      return(invisible(map))

    # highlight markers
    map <- map |>
      addCircleMarkers(
        data = selected_station,
        lat = ~ latitude,
        lng = ~ longitude,
        radius = 10,
        fillColor = "#7E2954",
        color = "white",
        weight = 2,
        fillOpacity = 0.9,
        group = "highlight",
        label = ~ paste("Selected:", station_id_name),
        layerId = ~ paste0(station_id_name, "__hi")
      )

    # Zoom logic
    if (nrow(selected_station) == 1) {
      map |>  setView(
        lng = selected_station$longitude[1],
        lat = selected_station$latitude[1],
        zoom = 11
      )
    } else {
      map |>  fitBounds(
        lng1 = min(selected_station$longitude, na.rm = TRUE),
        lat1 = min(selected_station$latitude, na.rm = TRUE),
        lng2 = max(selected_station$longitude, na.rm = TRUE),
        lat2 = max(selected_station$latitude, na.rm = TRUE)
      )
    }
  }

  output$wq_map <- renderLeaflet({
    leaflet() |>
      addMapPane("Lines-Habitat", zIndex = 430) |>
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}", attribution = 'Basemap © Esri, GEBCO, NOAA, CHS, etc.') |>
      addPolylines(
        data = salmonid_habitat_extents,
        label = ~ lapply(river, htmltools::HTML),
        popup = ~ river,
        color = "#5299D9",
        opacity = 1,
        weight = 1.5
      ) |>
      # Active sites (circles)
      addCircleMarkers(
        data = subset(wq_metadata, status == "Active"),
        layerId = ~ station_id_name,
        label = ~ paste(station_id_name, "-", station_description),
        radius = 6,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        fillColor = ~ site_color,
        popup = ~ paste0("<b>", station_id, "</b><br/>", station_description)
      ) |>
      addCircleMarkers(
        data    = wq_metadata,
        layerId = ~ station_id_name,
        label   = ~ paste(station_id_name, "-", station_description),
        radius = 6,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        fillColor = ~ ifelse(status == "Active", "black", "gray"),
        popup = ~ paste0("<b>", station_id, "</b><br/>", station_description)
      ) |>
      addLegend(
        position = "bottomright",
        colors = c("black", "gray"),
        labels = c("Active Station", "Inactive Station"),
        title = "Station Status",
        opacity = 0.7
      ) |>
      # Initial full-extent view
      fitBounds(
        lng1 = min(wq_metadata$longitude, na.rm = TRUE),
        lat1 = min(wq_metadata$latitude, na.rm = TRUE),
        lng2 = max(wq_metadata$longitude, na.rm = TRUE),
        lat2 = max(wq_metadata$latitude, na.rm = TRUE)
      )
  })
  observeEvent(input$wq_map_marker_click, ignoreInit = TRUE, {
    click <- input$wq_map_marker_click
    req(!is.null(click), !is.null(click$id))

    # If highlight markers use the "__hi" suffix, normalize it here
    id_raw <- click$id
    id     <- sub("__hi$", "", id_raw)

    current <- input$location_filter_wq
    if (is.null(current))
      current <- character(0)

    new_sel <- if (id %in% current)
      setdiff(current, id)
    else
      union(current, id)

    updateSelectInput(session, "location_filter_wq", selected = new_sel)
  })

  #
  #   observeEvent(input$clear_sites_wq, {
  #     updatePickerInput(session, "location_filter_wq", selected = character(0))
  #     draw_and_zoom_selection(character(0))
  #   }, ignoreInit = TRUE)


  # redraw highlight + zoom on dropdown change
  observeEvent(input$location_filter_wq, {
    draw_and_zoom_selection(input$location_filter_wq)
  }, ignoreInit = TRUE)

  filtered_wq_data <- reactive({
    req(input$analyte, length(input$analyte) > 0)

    # Use the active date range input (so changing it triggers a reactive update)
    dr <- if (!is.null(input$which_view_wq) &&
              input$which_view_wq == "Map Filter") {
      input$date_range_wq2
    } else {
      input$date_range_wq
    }
    req(!is.null(dr), length(dr) == 2, all(!is.na(dr)))

    data <- wq_data |>
      dplyr::filter(analyte %in% input$analyte,
                    as.Date(date) >= dr[1],
                    as.Date(date) <= dr[2])

    if (!is.null(input$location_filter_wq) &&
        length(input$location_filter_wq) > 0 &&
        !"All Locations" %in% input$location_filter_wq) {
      data <- data |>
        dplyr::filter(station_id_name %in% input$location_filter_wq)
    }

    data
  })

  observeEvent(input$location_filter_wq, {
    draw_and_zoom_selection(input$location_filter_wq)
  })

  # reset all ----
  observeEvent(input$clear_all, {
    # Reset station picker
    updateSelectizeInput(session, inputId = "location_filter_wq", selected = character(0))

    # Reset analyte picker
    updateSelectizeInput(session, inputId = "analyte", selected = character(0))

    # Reset year sliders
    updateSliderInput(session, "year_range", value = c(min(wq_data$year), 2025))
    updateSliderInput(session, "year_range2", value = c(min(wq_data$year), 2025))

    # Reset date ranges
    updateDateRangeInput(
      session,
      "date_range_wq",
      start = as.Date(sprintf("%d-01-01", min(wq_data$year))),
      end   = as.Date(sprintf("%d-12-31", 2025))
    )
    updateDateRangeInput(
      session,
      "date_range_wq2",
      start = as.Date(sprintf("%d-01-01", min(wq_data$year))),
      end   = as.Date(sprintf("%d-12-31", 2025)) #setting to 2025 for now, ideally range will be max(wq_data$year), but latest date is 2023 right now
    )

    # Reset map (zoom + highlight)
    draw_and_zoom_selection(character(0))
  })

  # ----

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
    req(!is.null(input$location_filter_wq) &&
          length(input$location_filter_wq) > 0)
    req(input$analyte, length(input$analyte) > 0)

    df <- filtered_wq_data()

    selected_locs <- input$location_filter_wq %||% character(0)

    present_locs <- sort(unique(df$station_id_name))
    missing_locs <- setdiff(selected_locs, present_locs)

    prev_missing <- wq_missing_sites()
    new_missing  <- setdiff(missing_locs, prev_missing)

    if (length(new_missing) > 0) {
      showNotification(
        paste0(
          "No data for selected analyte(s) at: ",
          paste(new_missing, collapse = ", ")
        ),
        type = "warning",
        duration = 6
      )
    }

    # update the tracker (so sites with data don't trigger, and new missing will)
    wq_missing_sites(missing_locs)

    if (n_distinct(df$analyte) > 8) {
      validate(need(
        FALSE,
        "Too many analytes selected. Please select 8 or fewer."
      ))
    }
    if (nrow(df) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "lines") |>
          layout(title = "No data available for current selection.")
      )
    }

    plot_type <- as.character(input$plot_type)[1]
    y_lab <- if (length(input$analyte) == 1)
      input$analyte[[1]]
    else
      "Value"


    # flags + segment ids
    df <- df |>
      dplyr::arrange(analyte, station_id_name, date) |>
      dplyr::group_by(analyte, station_id_name) |>
      dplyr::mutate(
        nd_flag = tolower(trimws(detection_status)) %in% c("not detected", "not detected."),
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
        nd_flag = tolower(trimws(detection_status)) %in% c("not detected", "not detected."),
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
            TRUE ~ NA_real_
          ),
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
        facet_wrap( ~ analyte_label, scales = "free_y", ncol = 1) +
        labs(x = "", y = y_axis_lab, color = "Location") +
        theme_minimal()

      # Tol Muted color palette
      tol_muted <- c(
        "#332288",
        "#117733",
        "#44AA99",
        "#88CCEE",
        "#DDCC77",
        "#CC6677",
        "#AA4499",
        "#882255"
      )

      # lines/points for detected ONLY, with group resetting after non-detects
      if (nrow(detected) > 0) {
        p <- p +
          geom_line(
            data = detected,
            aes(
              y = value,
              color = station_id_name,
              group = interaction(station_id_name, seg_id),
              text = paste0(
                "Date: ",
                date,
                "<br>",
                "Value: ",
                value,
                "<br>",
                "Station: ",
                station_id_name
              )
            ),
            linewidth = 0.6,
            show.legend = FALSE
          ) +
          geom_point(
            data = detected,
            aes(
              y = value,
              color = station_id_name,
              shape = station_id_name,
              text = paste0(
                "Date: ",
                date,
                "<br>",
                "Value: ",
                value,
                "<br>",
                "Station: ",
                station_id_name
              )
            ),
            size = 1.8,
            alpha = 0.8,
            show.legend = TRUE
          )
      }


      # Non-detect markers (vertical + short horizontal at MDL/MRL)
      if (nrow(nd) > 0) {
        p <- p +
          geom_segment(
            data = nd,
            aes(
              x = date,
              xend = date,
              y = 0,
              yend = nd_height,
              color = station_id_name,
              text = paste0(
                "Date: ",
                date,
                "<br>",
                "MDL value: ",
                mdl,
                "<br>",
                "MRL value: ",
                mrl,
                "<br>",
                "Reports to: ",
                reports_to,
                "<br>",
                "Station: ",
                station_id_name
              )
            ),
            linewidth = 0.6,
            linetype = 5,
            inherit.aes = FALSE
          ) +
          geom_segment(
            data = nd,
            aes(
              x = x_minus,
              xend = x_plus,
              y = nd_height,
              yend = nd_height,
              color = station_id_name,
              text = paste0(
                "Date: ",
                date,
                "<br>",
                "MDL value: ",
                mdl,
                "<br>",
                "MRL value: ",
                mrl,
                "<br>",
                "Reports to: ",
                reports_to,
                "<br>",
                "Station: ",
                station_id_name
              )
            ),
            linewidth = 0.6,
            lineend = "square",
            inherit.aes = FALSE
          )
      }

      # apply Tol Muted palette, shapes, and unified legend
      p <- p +
        scale_color_manual(values = tol_muted) +
        scale_shape_manual(values = c(16, 17, 15, 3, 7, 8, 9, 10)) +
        guides(color = guide_legend(override.aes = list(
          shape = 16,
          linetype = 0,
          size = 2.5,
          alpha = 1
        )),
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
        aes(
          x = station_id,
          y = value,
          fill = station_id,
          text = paste0(
            "Date: ",
            date,
            "<br>",
            "Value: ",
            value,
            "<br>",
            "Station: ",
            station_id_name
          )
        )
      ) +
        geom_boxplot(outlier.shape = NA) +
        facet_wrap( ~ analyte, scales = "free_y", ncol = 2) +
        labs(x = "", y = "value", fill = "Station") +
        scale_fill_manual(values = tol_muted) +
        theme_bw() +
        theme(legend.position = "none")

    } else {
      return(NULL)
    }

    gp <- ggplotly(p, tooltip = "text")

    seen <- character()   # track which site names already used

    for (i in seq_along(gp$x$data)) {
      tr <- gp$x$data[[i]]

      # Hide lines from legend
      if (!is.null(tr$mode) && tr$mode == "lines") {
        gp$x$data[[i]]$showlegend <- FALSE
      }

      # Enable legend only once per site for markers
      if (!is.null(tr$mode) && grepl("markers", tr$mode)) {
        site_name <- tr$name
        if (site_name %in% seen) {
          gp$x$data[[i]]$showlegend <- FALSE
        } else {
          gp$x$data[[i]]$showlegend <- TRUE
          seen <- c(seen, site_name)
        }
      }
    }

    # draw legend
    gp <- gp |> layout(showlegend = TRUE)

    gp

  })
  # Download WQ tab  --------------------------------------------------------------
  # sync Water Quality selections to Download tab
  # observeEvent(input$navbar, {
  #   if (input$navbar == "Download WQ Data") {
  #     updateSelectInput(session, "location_filter_dl",
  #                       selected = input$location_filter_wq)
  #     updateSliderInput(session, "year_range_dl",
  #                       value = input$year_range)
  #     updateSelectizeInput(session, "analyte_download",
  #                          selected = input$analyte)
  #   }
  # })

  observeEvent(input$wq_tabs, {
    if (input$wq_tabs == "Download Data") {
      updateSelectInput(session,
                        "location_filter_dl",
                        selected = input$location_filter_wq)
      updateSliderInput(session, "year_range_dl", value = input$year_range)
      updateSelectizeInput(session, "analyte_download", selected = input$analyte)
    }
  })

  # reset all ----
  observeEvent(input$clear_all_dl, {
    # Reset location (selectInput)
    updateSelectInput(session, inputId = "location_filter_dl", selected = character(0))

    # Reset analyte (selectizeInput)
    updateSelectizeInput(session, inputId = "analyte_download", selected = character(0))

    # Reset year range slider (download tab)
    updateSliderInput(session, "year_range_dl", value = c(min(wq_data$year), 2025))

    # Reset include_weather checkbox (download tab)
    updateCheckboxInput(session, inputId = "include_weather", value = FALSE)
  })



  # shared filtering logic
  filter_wq_data <- function(locations,
                             years,
                             analytes,
                             include_weather = FALSE) {
    out <- wq_data |>
      dplyr::filter(lubridate::year(date) >= years[1],
                    lubridate::year(date) <= years[2])

    if (!is.null(locations) && length(locations) > 0) {
      out <- out |> dplyr::filter(station_id_name %in% locations)
    }

    # handle analytes
    if (!is.null(analytes) && length(analytes) > 0) {
      out <- out |> dplyr::filter(analyte %in% analytes)
    }

    # add weather analytes if requested
    if (include_weather) {
      weather <- wq_quality_weather |>
        dplyr::filter(
          analyte %in% c("Rain", "Sky Conditions"),
          lubridate::year(date) >= years[1],
          lubridate::year(date) <= years[2]
        )

      # same location filtering to weather data
      if (!is.null(locations) && length(locations) > 0) {
        weather <- weather |> dplyr::filter(station_id_name %in% locations)
      }

      if (nrow(weather) == 0) {
        showNotification(
          "No weather data (Rain or Sky Conditions) available for the selected site(s) and year(s).",
          type = "message",
          duration = 6
        )
      } else {
        weather <- weather |>
          dplyr::mutate(value_text = value, value = NA_real_) |>
          select(-value) |>
          mutate(value = as.character(value_text)) |>
          select(-value_text)

        out <- out |>
          dplyr::mutate(value = as.character(value))

        out <- dplyr::bind_rows(out, weather)
      }
    }

    out

  }
  # reactives for both tabs
  wq_download_data <- reactive({
    filter_wq_data(
      input$location_filter_wq,
      input$year_range,
      input$analyte,
      include_weather = input$include_weather
    )
  })

  dl_download_data <- reactive({
    filter_wq_data(
      input$location_filter_dl,
      input$year_range_dl,
      input$analyte_download,
      include_weather = input$include_weather
    )
  })

  # shared download handler function
  make_download_handler <- function(data_fun) {
    downloadHandler(
      filename = function()
        paste0("water_quality_", Sys.Date(), ".csv"),
      content = function(file) {
        readr::write_csv(data_fun(), file)
      }
    )
  }

  # assign to outputs
  output$download_wq_csv_dl <- make_download_handler(dl_download_data)

  output$dl_preview_table <- DT::renderDataTable({
    req(
      input$location_filter_dl,
      input$year_range_dl,
      input$analyte_download,
      length(input$location_filter_dl) > 0,
      length(input$analyte_download) > 0
    )

    dl_download_data() |>
      dplyr::select(date, station_id_name, analyte, value, unit) |>
      DT::datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
}
