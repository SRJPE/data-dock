server <- function(input, output, session) {

############################# ===
# DOWNLOAD HANDLERS ------
# Defined first so they are available to both Genetics and Water Quality sections
############################# ===

  # Genetics downloads — filename: genetics_YYYY-MM-DD.csv
  make_genetics_download_handler <- function(data_fun) {
    downloadHandler(
      filename = function() paste0("genetics_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(data_fun(), file)
    )
  }

  # Water quality downloads — filename: water_quality_YYYY-MM-DD.csv
  make_wq_download_handler <- function(data_fun) {
    downloadHandler(
      filename = function() paste0("water_quality_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(data_fun(), file)
    )
  }

############################# ===
# GENETICS ----
############################# ===

## Map ----

  # Draws highlight markers on the genetics map for selected sites.
  # Called when: dropdown selection changes, map marker is clicked, clear button
  # Note: function name retained for clarity — no longer zooms to selection
  draw_and_zoom_selection_g <- function(sel_names) {
    map <- leafletProxy("g_map") |>
      clearGroup("highlight") |>
      clearPopups()

    # If nothing selected, clear highlights and return
    if (is.null(sel_names) || length(sel_names) == 0)
      return(invisible(map))

    selected_station_g <- subset(rst_sites, label %in% sel_names)
    if (nrow(selected_station_g) == 0)
      return(invisible(map))

    # Draw larger filled markers on top of base markers to indicate selection
    map |>
      addCircleMarkers(
        data = selected_station_g,
        lat = ~latitude,
        lng = ~longitude,
        radius = 10,
        fillColor   = "#7E2954",
        color = "white",
        weight = 2,
        fillOpacity = 0.9,
        group = "highlight",
        label = ~paste("Selected:", label),
        layerId = ~paste0(label, "__hi")
      )
  }

  output$g_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) |>
      addMapPane("Lines-Habitat", zIndex = 430) |>
      addTiles(
        urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
        attribution = "Basemap © Esri, HERE, Garmin, FAO, NOAA, USGS"
      ) |>
      # River habitat extent polylines
      addPolylines(
        data = salmonid_habitat_extents,
        label = ~lapply(river, htmltools::HTML),
        popup = ~river,
        color = "#5299D9",
        opacity = 1,
        weight = 1.5
      ) |>
      # RST monitoring site markers
      # NOTE: coordinates are jittered ~500m for privacy — see global.R
      addCircleMarkers(
        data = rst_sites,
        layerId = ~label,
        label = ~label,
        radius = 6,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        fillColor = "black",
        popup = ~label
      ) |>
      fitBounds(
        lng1 = min(rst_sites$longitude, na.rm = TRUE),
        lat1 = min(rst_sites$latitude,  na.rm = TRUE),
        lng2 = max(rst_sites$longitude, na.rm = TRUE),
        lat2 = max(rst_sites$latitude,  na.rm = TRUE)
      ) |>
      htmlwidgets::onRender("
        function(el, x) {
          L.control.zoom({ position: 'bottomleft' }).addTo(this);
        }
      ")
  })

  # Draw initial highlight for default "Clear Creek" selection on app load.
  # session$onFlushed ensures the map is fully rendered before highlight is drawn.
  session$onFlushed(function() {
    observe({
      draw_and_zoom_selection_g(input$location_filter_g)
    })
  }, once = TRUE)

  # Map click -> toggle site in/out of dropdown selection
  # "__hi" suffix on highlight marker IDs is stripped before processing
  observeEvent(input$g_map_marker_click, ignoreInit = TRUE, {
    click <- input$g_map_marker_click
    req(!is.null(click), !is.null(click$id))

    id <- sub("__hi$", "", click$id)

    current <- input$location_filter_g %||% character(0)
    new_sel <- if (id %in% current) setdiff(current, id) else union(current, id)

    updateSelectizeInput(session, "location_filter_g", selected = new_sel)
  })

  # Dropdown change -> redraw highlights
  observeEvent(input$location_filter_g, {
    draw_and_zoom_selection_g(input$location_filter_g)
  }, ignoreInit = TRUE)

  # "All Years" button -> reset year dropdown to all available years
  observeEvent(input$select_all_years_g, {
    updateSelectizeInput(
      session,
      inputId  = "year_range_g",
      selected = sort(unique(run_designation$year))
    )
  })

# ── Reactive data ─────────────────────────────────────────────────────────────

  # Base filter: year and location
  # greb1l heterozygote excluded from Run Type plots per program guidance
  filtered_g_data <- reactive({
    req(input$year_range_g, input$location_filter_g)

    data <- run_designation |>
      filter(year %in% input$year_range_g)

    if (!is.null(input$location_filter_g) &&
        !"All Locations" %in% input$location_filter_g) {
      data <- data |> filter(map_label %in% input$location_filter_g)
    }

    if (input$data_plot_g == "Run Type") {
      data <- data |> filter(run_name != "greb1l heterozygote")
    }

    data
  })

  # Aggregates filtered data for plotting.
  # Water Year: summarizes by year + location -> proportions or counts
  # Month: summarizes by year + month + location -> proportions or counts
  #        uses fake_date (1991/1992) to align Nov-May monitoring year on x-axis
  data_for_plot_g <- reactive({
    grouping_variable <- if (input$data_plot_g == "Greb 1L RoSA Genotype") {
      "genotype"
    } else {
      "run_name"
    }

    if (input$plot_type_g == "Water Year") {
      df <- filtered_g_data() |>
        group_by(year, map_label, .data[[grouping_variable]]) |>
        summarize(count = n(), .groups = "drop") |>
        group_by(year, map_label) |>
        mutate(
          total_sample = sum(count),
          run_percent  = (count / total_sample) * 100
        )
    }

    if (input$plot_type_g == "Month") {
      df <- filtered_g_data() |>
        filter(!is.na(month)) |>
        group_by(year, month, location_name, .data[[grouping_variable]]) |>
        summarise(total_samples = n(), .groups = "drop") |>
        group_by(year, month, location_name) |>
        mutate(
          site_total  = sum(total_samples),
          run_percent = (total_samples / site_total) * 100
        ) |>
        ungroup() |>
        # Assign fake year so Nov-Dec plot before Jan-May (monitoring year order)
        mutate(
          fake_year = ifelse(month %in% 10:12, "1991", "1992"),
          fake_date = as.Date(paste0(fake_year, "-", month, "-01"))
        ) |>
        select(-fake_year) |>
        mutate(month = factor(month, levels = 1:12, labels = month.abb)) |>
        # complete() ensures facets appear for all year/location combos
        # even if no data exists — shows empty panel rather than missing facet
        complete(
          location_name, year, month, fake_date,
          .data[[grouping_variable]],
          fill = list(run_percent = 0, site_total = 0, total_samples = 0)
        )
    }

    # Enforce consistent bar order for both plot types
    if (input$data_plot_g == "Run Type") {
      df <- df |> dplyr::mutate(
        run_name = factor(run_name,
                          levels = c("spring", "winter",
                                     "fall or late fall", "greb1l heterozygote"))
      )
    }

    if (input$data_plot_g == "Greb 1L RoSA Genotype") {
      df <- df |> dplyr::mutate(
        genotype = factor(genotype, levels = c("early", "heterozygote", "late"))
      )
    }

    df
  })

# ── Downloads ─────────────────────────────────────────────────────────────────

  # Visualize tab download — uses filtered_g_data (location + year filter only)
  output$download_g_csv <- make_genetics_download_handler(filtered_g_data)

  # Download tab download — additionally filtered by run_name
  output$download_g_csv_dl <- make_genetics_download_handler(dl_download_data_g)

# ── Plot ──────────────────────────────────────────────────────────────────────

  output$g_dynamic_plot <- renderPlotly({
    df <- data_for_plot_g()

    if (nrow(df) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "lines") |>
          layout(title = "No data available for current selection.")
      )
    }

    # Set plot text and axis labels based on data type selection
    if (input$data_plot_g == "Greb 1L RoSA Genotype") {
      grouping_variable <- "genotype"
      title_text <- "Genotype Proportion: "
      title_text2 <- "Genotype: "
      y_axis_text <- "Genotype Proportions"
      legend_title <- "Genotype"
      count_y_label <- "Genotype Count (n)"
    } else {
      grouping_variable <- "run_name"
      title_text <- "Run Assignment Proportion: "
      title_text2 <- "Run Type: "
      y_axis_text <- "Run Assignment Proportions"
      legend_title <- "Run type"
      count_y_label <- "Run Type Count (n)"
    }

    # Toggle between proportions (default) and absolute counts
    show_counts <- input$count_type_g == "TRUE"

    if (input$plot_type_g == "Water Year") {
      y_var       <- if (show_counts) "count"       else "run_percent"
      y_label     <- if (show_counts) count_y_label else y_axis_text
      hover_label <- if (show_counts) "Count: "     else title_text
      hover_val   <- if (show_counts) df$count      else signif(df$run_percent, 2)

      plot <- ggplot(df, aes(
        x = year,
        y = .data[[y_var]],
        fill = .data[[grouping_variable]],
        text = paste0(
          "Water Year: ", year, "<br>",
          hover_label, hover_val, "<br>",
          title_text2, .data[[grouping_variable]], "<br>",
          "Sample Size: ", total_sample
        )
      )) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap(~map_label, ncol = 1) +
        scale_fill_manual(values = run_col) +
        theme_minimal() +
        labs(x = "", y = y_label, fill = legend_title)
    }

    if (input$plot_type_g == "Month") {
      selected_years <- input$year_range_g

      # Limit Month view to 3 years — more becomes unreadable
      if (length(selected_years) > 3) {
        showNotification("Please select 3 years or fewer for Month view.",
                         type = "error")
        return(
          plotly_empty(type = "scatter", mode = "lines") |>
            layout(title = "Too many years selected. Please select 3 years or fewer.")
        )
      }

      y_var <- if (show_counts) "total_samples" else "run_percent"
      y_label <- if (show_counts) count_y_label   else y_axis_text
      hover_label <- if (show_counts) "Count: "       else title_text
      hover_val <- if (show_counts) df$total_samples else signif(df$run_percent, 2)
      n_years <- length(unique(df$year))

      plot <- ggplot(df, aes(
        x = fake_date,
        y = .data[[y_var]],
        fill = .data[[grouping_variable]],
        text = paste0(
          "Water Year: ", year, "<br>",
          hover_label, hover_val, "<br>",
          title_text2, .data[[grouping_variable]], "<br>",
          "Sample Size: ", site_total
        )
      )) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap(~location_name + year, ncol = n_years) +
        scale_fill_manual(name = legend_title, values = run_col) +
        labs(x = "", y = y_label) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        scale_x_date(breaks = "1 month", date_labels = "%b")
    }

    ggplotly(plot, tooltip = "text")
  })

# ── Clear button (Visualize tab) ──────────────────────────────────────────────

  observeEvent(input$clear_all_g, {
    updateSelectizeInput(session, inputId = "location_filter_g",
                         selected = character(0))
    updateSelectizeInput(session, inputId = "year_range_g",
                         selected = sort(unique(run_designation$year)))
    updateSelectInput(session, "plot_type_g", "Water Year")
    updateSelectInput(session, "data_plot_g", "Run Type")
    shinyWidgets::updateRadioGroupButtons(session, "count_type_g",
                                          selected = "FALSE")
    draw_and_zoom_selection_g(character(0))
    # Reset map extent to full view
    leafletProxy("g_map") |>
      fitBounds(
        lng1 = min(rst_sites$longitude, na.rm = TRUE),
        lat1 = min(rst_sites$latitude,  na.rm = TRUE),
        lng2 = max(rst_sites$longitude, na.rm = TRUE),
        lat2 = max(rst_sites$latitude,  na.rm = TRUE)
      )
  })

## Download tab ------------------------------------------------------

  # Sync Visualize tab selections to Download tab when user switches tabs
  observeEvent(input$genetics_tabs, {
    if (input$genetics_tabs == "Download Data") {
      updateSelectizeInput(session, "location_filter_dl_g",
                           selected = input$location_filter_g)
      updateSelectizeInput(session, "year_range_dl_g",
                           selected = input$year_range_g)
      updateSelectizeInput(session, "run_download",
                           selected = sort(unique(run_designation$run_name)))
    }
  })

  # Clear button (Download tab)
  observeEvent(input$clear_all_dl_g, {
    updateSelectizeInput(session, inputId = "location_filter_dl_g",
                         selected = character(0))
    updateSelectizeInput(session, inputId = "run_download",
                         selected = character(0))
    updateSelectizeInput(session, "year_range_dl_g",
                         selected = sort(unique(run_designation$year)))
  })

  # Download tab data — additionally filtered by run_name
  dl_download_data_g <- reactive({
    req(input$year_range_dl_g, input$location_filter_dl_g)

    data <- run_designation |>
      filter(year %in% input$year_range_dl_g)

    if (!is.null(input$location_filter_dl_g) &&
        !"All Locations" %in% input$location_filter_dl_g) {
      data <- data |> filter(map_label %in% input$location_filter_dl_g)
    }

    if (length(input$run_download) > 0) {
      data <- data |> filter(run_name %in% input$run_download)
    }

    data
  })

  # Preview table — shows key columns first, then remaining columns
  output$dl_preview_table_g <- DT::renderDataTable({
    req(
      input$location_filter_dl_g,
      input$year_range_dl_g,
      input$run_download,
      length(input$location_filter_dl_g) > 0,
      length(input$run_download) > 0
    )
    dl_download_data_g() |>
      dplyr::select(sample_id, code, sample_event, year, month,
                    datetime_collected, location_name, map_label,
                    everything()) |>
      DT::datatable(options = list(pageLength = 10, scrollX = TRUE))
  })

############################# ===
# WATER QUALITY ---------
############################# ===

# ── Shared filter function ────────────────────────────────────────────────────

  # Core filtering logic used by both Visualize and Download tabs.
  # start_date and end_date are snapped to month boundaries:
  # start_date -> first day of selected month
  # end_date -> last day of selected month
  # This ensures month-picker selections always return complete months.
  filter_wq_data <- function(locations,
                             start_date,
                             end_date,
                             analytes,
                             include_weather = FALSE) {

    start_date <- floor_date(as.Date(start_date), "month")
    end_date   <- ceiling_date(as.Date(end_date), "month") - days(1)

    out <- wq_data |>
      dplyr::filter(date >= start_date, date <= end_date)

    if (!is.null(locations) && length(locations) > 0) {
      out <- out |> dplyr::filter(station_id_name %in% locations)
    }

    if (!is.null(analytes) && length(analytes) > 0) {
      out <- out |> dplyr::filter(analyte %in% analytes)
    }

    # Weather analytes are stored separately in wq_quality_weather
    # and only appended when user checks "Include weather observations"
    if (include_weather) {
      weather <- wq_quality_weather |>
        dplyr::filter(
          analyte %in% c("Rain", "Sky Conditions"),
          date >= start_date,
          date <= end_date
        )

      if (!is.null(locations) && length(locations) > 0) {
        weather <- weather |> dplyr::filter(station_id_name %in% locations)
      }

      if (nrow(weather) == 0) {
        showNotification(
          "No weather data available for the selected site(s) and date range.",
          type = "message", duration = 12
        )
      } else {
        # Convert both to character before binding — weather values are text
        weather <- weather |>
          dplyr::mutate(value_text = value, value = NA_real_) |>
          select(-value) |>
          mutate(value = as.character(value_text)) |>
          select(-value_text)

        out <- out |> dplyr::mutate(value = as.character(value))
        out <- dplyr::bind_rows(out, weather)
      }
    }

    out
  }

# ── Reactives ─────────────────────────────────────────────────────────────────

  # Visualize tab — filtered by map/dropdown selections
  wq_download_data <- reactive({
    filter_wq_data(
      input$location_filter_wq,
      input$start_date_wq,
      input$end_date_wq,
      input$analyte,
      include_weather = input$include_weather
    )
  })

  # Download tab — filtered by Download tab sidebar selections
  dl_download_data <- reactive({
    filter_wq_data(
      input$location_filter_dl,
      input$start_date_dl,
      input$end_date_dl,
      input$analyte_download,
      include_weather = input$include_weather
    )
  })

# ── Analyte dropdown ──────────────────────────────────────────────────────────

  # Update analyte choices when station selection changes.
  # Only shows analytes available at the selected stations.
  # Preserves previously selected analytes that are still valid —
  # prevents analyte selection from clearing when a new station is added.
  observeEvent(input$location_filter_wq, {
    sel_stations <- input$location_filter_wq

    analyte_choices <- if (is.null(sel_stations) || length(sel_stations) == 0) {
      sort(unique(wq_data$analyte))
    } else {
      wq_data |>
        dplyr::filter(station_id_name %in% sel_stations) |>
        dplyr::pull(analyte) |>
        unique() |>
        sort()
    }

    still_valid <- intersect(input$analyte, analyte_choices)

    updateSelectizeInput(
      session,
      inputId  = "analyte",
      choices  = analyte_choices,
      selected = still_valid,
      server   = TRUE
    )
  })

## Map ----------------------------

  # Draws highlight markers on the WQ map for selected stations.
  # Active stations highlighted black, inactive highlighted gray.
  # Maroon outline distinguishes selected from unselected markers.
  # Note: function name retained — no longer zooms to selection
  draw_and_zoom_selection <- function(sel_names) {
    map <- leafletProxy("wq_map") |>
      clearGroup("highlight") |>
      clearPopups()

    if (is.null(sel_names) || length(sel_names) == 0)
      return(invisible(map))

    # Only highlight stations that have map coordinates
    selected_station <- subset(wq_metadata, station_id_name %in% sel_names)
    if (nrow(selected_station) == 0)
      return(invisible(map))

    map |>
      addCircleMarkers(
        data = selected_station,
        lat = ~latitude,
        lng = ~longitude,
        radius = 10,
        fillColor = ~ifelse(status == "Active", "black", "gray"),
        color = "#7E2954",
        weight = 2,
        fillOpacity = 0.9,
        group = "highlight",
        label = ~paste("Selected:", station_id_name),
        layerId = ~paste0(station_id_name, "__hi")
      )
  }

  output$wq_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) |>
      addMapPane("Lines-Habitat", zIndex = 430) |>
      addTiles(
        urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
        attribution = "Basemap © Esri, HERE, Garmin, FAO, NOAA, USGS"
      ) |>
      addPolylines(
        data = salmonid_habitat_extents,
        label = ~lapply(river, htmltools::HTML),
        popup = ~river,
        color = "#5299D9",
        opacity = 1,
        weight = 1.5
      ) |>
      # Black = Active, Gray = Historical (Inactive)
      addCircleMarkers(
        data = wq_metadata,
        layerId = ~station_id_name,
        label = ~paste(station_id_name),
        radius = 6,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        fillColor = ~ifelse(status == "Active", "black", "gray"),
        popup = ~paste0("<b>", station_id, "</b><br/>", station_description)
      ) |>
      addLegend(
        position = "bottomright",
        colors = c("black", "gray"),
        labels = c("Active Station", "Historical Station"),
        title = "Station Status",
        opacity = 0.7
      ) |>
      fitBounds(
        lng1 = min(wq_metadata$longitude, na.rm = TRUE),
        lat1 = min(wq_metadata$latitude,  na.rm = TRUE),
        lng2 = max(wq_metadata$longitude, na.rm = TRUE),
        lat2 = max(wq_metadata$latitude,  na.rm = TRUE)
      ) |>
      htmlwidgets::onRender("
        function(el, x) {
          L.control.zoom({ position: 'bottomleft' }).addTo(this);
        }
      ")
  })

  # Map click -> toggle station in/out of dropdown selection
  observeEvent(input$wq_map_marker_click, ignoreInit = TRUE, {
    click <- input$wq_map_marker_click
    req(!is.null(click), !is.null(click$id))

    id <- sub("__hi$", "", click$id)
    current <- input$location_filter_wq %||% character(0)
    new_sel <- if (id %in% current) setdiff(current, id) else union(current, id)

    updateSelectizeInput(session, "location_filter_wq", selected = new_sel)
  })

  # Dropdown change -> redraw highlights + warn if selected site has no coordinates
  observeEvent(input$location_filter_wq, {
    draw_and_zoom_selection(input$location_filter_wq)

    # Warn user if selected station(s) won't appear on the map
    unmappable <- input$location_filter_wq[
      !input$location_filter_wq %in% wq_metadata$station_id_name
    ]
    if (length(unmappable) > 0) {
      showNotification(
        paste0(
          "The following site(s) have no location data and won't appear on the map: ",
          paste(unmappable, collapse = ", ")
        ),
        type = "message", duration = 12
      )
    }
  }, ignoreInit = TRUE)

# ── Clear button (Visualize tab) ──────────────────────────────────────────────

  observeEvent(input$clear_all, {
    updateSelectizeInput(session, inputId = "location_filter_wq",
                         selected = character(0))
    updateSelectizeInput(session, inputId = "analyte",
                         selected = character(0))
    shinyWidgets::updateAirDateInput(session, "start_date_wq",
                                     value = min(wq_data$date))
    shinyWidgets::updateAirDateInput(session, "end_date_wq",
                                     value = max(wq_data$date))
    draw_and_zoom_selection(character(0))
    # Reset map to full extent
    leafletProxy("wq_map") |>
      fitBounds(
        lng1 = min(wq_metadata$longitude, na.rm = TRUE),
        lat1 = min(wq_metadata$latitude,  na.rm = TRUE),
        lng2 = max(wq_metadata$longitude, na.rm = TRUE),
        lat2 = max(wq_metadata$latitude,  na.rm = TRUE)
      )
  })

# ── Downloads ─────────────────────────────────────────────────────────────────

  # Missing sites tracker — prevents repeat notifications for same missing sites
  wq_missing_sites <- reactiveVal(character(0))

  # Visualize tab download — uses current map/filter selections
  output$download_wq_csv    <- make_wq_download_handler(wq_download_data)

  # Download tab download — uses Download tab sidebar selections
  output$download_wq_csv_dl <- make_wq_download_handler(dl_download_data)

# ── Plot ──────────────────────────────────────────────────────────────────────

  output$wq_dynamic_plot <- renderPlotly({
    req(
      !is.null(input$location_filter_wq) && length(input$location_filter_wq) > 0,
      input$analyte, length(input$analyte) > 0
    )

    df <- filter_wq_data(
      input$location_filter_wq,
      input$start_date_wq,
      input$end_date_wq,
      input$analyte
    )

    # Notify user if selected stations have no data for chosen analyte(s)
    selected_locs <- input$location_filter_wq %||% character(0)
    missing_locs <- setdiff(selected_locs, sort(unique(df$station_id_name)))
    new_missing <- setdiff(missing_locs, wq_missing_sites())

    if (length(new_missing) > 0) {
      showNotification(
        paste0("No data for selected analyte(s) at: ",
               paste(new_missing, collapse = ", ")),
        type = "warning", duration = 12
      )
    }
    wq_missing_sites(missing_locs)

    if (n_distinct(df$analyte) > 8) {
      validate(need(FALSE, "Too many analytes selected. Please select 8 or fewer."))
    }

    if (nrow(df) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "lines") |>
          layout(title = "No data available for current selection.")
      )
    }

    plot_type <- as.character(input$plot_type)[1]

    # Flag non-detects and create segment IDs for line-break logic
    # seg_id resets after each non-detect to break the line at that point
    df <- df |>
      dplyr::arrange(analyte, station_id_name, date) |>
      dplyr::group_by(analyte, station_id_name) |>
      dplyr::mutate(
        nd_flag = tolower(trimws(detection_status)) %in%
          c("not detected", "not detected."),
        seg_id  = cumsum(dplyr::lag(nd_flag, default = FALSE))
      ) |>
      dplyr::ungroup()

    # For boxplots: exclude station/analyte combos where >50% are non-detects
    prop_nd <- df |>
      group_by(analyte, station_id_name) |>
      tally() |>
      left_join(
        df |>
          filter(detection_status != "Not detected") |>
          group_by(analyte, station_id_name) |>
          tally() |>
          rename(n_detected = n)
      ) |>
      mutate(prop_detected = n_detected / n)

    # Build analyte label with units for facet titles
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
        unit_label = dplyr::coalesce(unit_label, "unit unknown"),
        analyte_label = paste0(analyte, " (", unit_label, ")")
      )

    if (plot_type == "Time Series") {
      detected <- df_plot |> dplyr::filter(!is.na(value) & !nd_flag)

      # Non-detect markers: vertical dashed line from 0 to MRL
      # with a short horizontal cap at the MRL value
      nd <- df_plot |>
        dplyr::filter(nd_flag) |>
        dplyr::mutate(
          nd_height = mrl,
          x_minus = date - lubridate::days(10),
          x_plus = date + lubridate::days(10)
        ) |>
        dplyr::filter(!is.na(nd_height))

      # Show unit on y-axis only for single analyte — otherwise unit is in facet title
      y_axis_lab <- if (dplyr::n_distinct(df_plot$analyte) == 1) {
        unique(df_plot$unit_label)
      } else {
        NULL
      }

      p <- ggplot(df_plot, aes(x = date)) +
        facet_wrap(~analyte_label, scales = "free_y", ncol = 1) +
        labs(x = "", y = y_axis_lab, color = "Location") +
        theme_minimal()

      if (nrow(detected) > 0) {
        p <- p +
          geom_line(
            data = detected,
            aes(
              y = value,
              color = station_id_name,
              group = interaction(station_id_name, seg_id),
              text = paste0("Date: ", date, "<br>",
                             "Value: ", value, "<br>",
                             "Station: ", station_id_name)
            ),
            linewidth   = 0.6,
            show.legend = FALSE
          ) +
          geom_point(
            data = detected,
            aes(
              y = value,
              color = station_id_name,
              shape = station_id_name,
              text = paste0("Date: ", date, "<br>",
                             "Value: ", value, "<br>",
                             "Station: ", station_id_name)
            ),
            size = 1.8,
            alpha = 0.8,
            show.legend = TRUE
          )
      }

      if (nrow(nd) > 0) {
        # Jitter non-detect markers horizontally when multiple sites selected
        # prevents overlapping markers on the same date
        n_sites_selected <- length(unique(nd$station_id_name))

        if (n_sites_selected > 1) {
          station_levels <- sort(unique(nd$station_id_name))
          offsets_hours  <- seq(-36, 36, length.out = n_sites_selected)

          nd$date_j    <- nd$date + as.difftime(
            offsets_hours[match(nd$station_id_name, station_levels)],
            units = "hours"
          )
          nd$x_minus_j <- nd$x_minus + (nd$date_j - nd$date)
          nd$x_plus_j  <- nd$x_plus  + (nd$date_j - nd$date)
        } else {
          nd$date_j <- nd$date
          nd$x_minus_j <- nd$x_minus
          nd$x_plus_j <- nd$x_plus
        }

        p <- p +
          geom_segment(
            data = nd,
            aes(x = date_j, xend = date_j, y = 0, yend = nd_height,
                color = station_id_name,
                text = paste0("Date: ", date, "<br>",
                               "MRL value: ", mrl, "<br>",
                               "Station: ", station_id_name)),
            linewidth = 0.6,
            linetype = 5,
            inherit.aes = FALSE
          ) +
          geom_segment(
            data = nd,
            aes(x = x_minus_j, xend = x_plus_j,
                y = nd_height, yend = nd_height,
                color = station_id_name,
                text  = paste0("Date: ", date, "<br>",
                               "MRL value: ", mrl, "<br>",
                               "Station: ", station_id_name)),
            linewidth = 0.6,
            lineend = "square",
            inherit.aes = FALSE
          )
      }

      p <- p +
        scale_color_manual(values = tol_muted) +
        scale_shape_manual(values = c(16, 17, 15, 3, 7, 8, 9, 10)) +
        guides(color = guide_legend(override.aes = list(
          shape = 16, linetype = 0, size = 2.5, alpha = 1
        )), shape = "none") +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold")
        )

    } else if (plot_type == "Box Plot") {
      # Exclude station/analyte combos where >50% of values are non-detects
      boxplot_data <- df |>
        left_join(prop_nd) |>
        filter(prop_detected >= 0.5)

      if (nrow(boxplot_data) == 0) {
        return(
          plotly_empty(type = "scatter", mode = "lines") |>
            layout(title = "No data available for current selection.
                   When more than 50% of the data are non-detects, boxplots
                   are not generated. Please refer to the Time Series plots.")
        )
      } else {
        p <- ggplot(
          boxplot_data |> dplyr::filter(!is.na(value)),
          aes(
            x = station_id,
            y = value,
            fill = station_id,
            text = paste0("Date: ", date, "<br>",
                          "Value: ", value, "<br>",
                          "Station: ", station_id_name)
          )
        ) +
          geom_boxplot(outlier.shape = NA) +
          facet_wrap(~analyte, scales = "free_y", ncol = 2) +
          labs(x = "", y = "value", fill = "Station") +
          scale_fill_manual(values = tol_muted) +
          theme_bw() +
          theme(legend.position = "none")
      }
    } else {
      return(NULL)
    }

    gp <- ggplotly(p, tooltip = "text")

    # Clean up legend — show each station only once, hide line traces
    seen <- character()
    for (i in seq_along(gp$x$data)) {
      tr <- gp$x$data[[i]]
      if (!is.null(tr$mode) && tr$mode == "lines") {
        gp$x$data[[i]]$showlegend <- FALSE
      }
      if (!is.null(tr$mode) && grepl("markers", tr$mode)) {
        if (tr$name %in% seen) {
          gp$x$data[[i]]$showlegend <- FALSE
        } else {
          gp$x$data[[i]]$showlegend <- TRUE
          seen <- c(seen, tr$name)
        }
      }
    }

    gp |> layout(showlegend = TRUE)
  })

## Download tab ------------------------------------------------------

  # Sync Visualize tab selections to Download tab when user switches tabs
  observeEvent(input$wq_tabs, {
    if (input$wq_tabs == "Download Data") {
      updateSelectizeInput(session, "location_filter_dl",
                           selected = input$location_filter_wq)
      shinyWidgets::updateAirDateInput(session, "start_date_dl",
                                       value = input$start_date_wq)
      shinyWidgets::updateAirDateInput(session, "end_date_dl",
                                       value = input$end_date_wq)
      updateSelectizeInput(session, "analyte_download",
                           selected = input$analyte)
    }
  })

  # Clear button (Download tab)
  observeEvent(input$clear_all_dl, {
    updateSelectizeInput(session, inputId = "location_filter_dl",
                         selected = character(0))
    updateSelectizeInput(session, inputId = "analyte_download",
                         selected = character(0))
    shinyWidgets::updateAirDateInput(session, "start_date_dl",
                                     value = min(wq_data$date))
    shinyWidgets::updateAirDateInput(session, "end_date_dl",
                                     value = max(wq_data$date))
    updateCheckboxInput(session, inputId = "include_weather", value = FALSE)
  })

  # Preview table — shows key columns first then remaining columns
  output$dl_preview_table <- DT::renderDataTable({
    req(
      input$location_filter_dl,
      input$start_date_dl,
      input$end_date_dl,
      input$analyte_download,
      length(input$location_filter_dl) > 0,
      length(input$analyte_download) > 0
    )

    dl_download_data() |>
      dplyr::select(station_id, station_description, date, time,
                    year, analyte, value, unit,
                    everything(), -station_id_name) |>
      DT::datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
}
