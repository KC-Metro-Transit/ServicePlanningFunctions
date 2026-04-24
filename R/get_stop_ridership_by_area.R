#' Get Stop Ridership by Area
#'
#' @description This function returns either an interactive map of weekday stop-level ridership for the selected area or a dataframe showing ridership data for stops in the selected area.
#'
#' @param area Character. The name of the LOCUS district of interest. Can accept multiples.
#' @param gtfs_date 'YYYY-MM-DD' Date of GTFS file to use for stop locations. Defaults to nearest dataset released before or on specified date.
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param return_type Character. Specify what you want to see. Options are "table" and "interactive_map".
#' @param service_change_num Numeric. The three-digit identifier of the service change.
#'  Can accept multiple values as a vector if you are returning a table. For maps, select one service change at a time.
#' @param route Numeric.  The route identifiers of interest. Optional.
#' @param time_period Character. AM, PM, MID, XEV. XNT.
#' @param activity_type Character. ons - Average Daily Boarding, offs - Average Daily Alightings, avg_lod - Average Max Load.
#' @param data_source Character. Specify which areas to show. Options are 'LOCUS' or 'King County Council Districts'
#' @returns Interactive map object or dataframe of stop ridership summed by selected periods.
#'
#' @export

get_stop_ridership_by_area <- function(
  area,
  gtfs_date,
  service_change_num,
  route = "All",
  tbird_connection,
  return_type,
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  activity_type = 'ons',
  data_source
) {
  stops <- get_stops_by_area(
    area = area,
    gtfs_date = gtfs_date,
    tbird_connection = tbird_connection,
    return_type = "table",
    data_source = data_source
  ) |>
    sf::st_as_sf()

  stop_ids <- unique(stops$stop_id)

  rides <- get_stop_ridership(
    service_change_num = service_change_num,
    stop_id = stop_ids,
    route = route,
    tbird_connection = tbird_connection
  ) %>%
    dplyr::filter(
      day_part_cd %in% time_period,
      service_change_num %in% .env$service_change_num
    ) %>%
    dplyr::rename(period = time_period_at_stop)

  routes_at_stop <- rides |>
    dplyr::filter(service_change_num %in% .env$service_change_num) |>
    dplyr::distinct(stop_id, route_name, service) |>
    dplyr::group_by(stop_id, service) |>
    dplyr::mutate(routes_at_stop = toString(route_name)) |>
    dplyr::ungroup() |>
    dplyr::select(-route_name) |>
    dplyr::distinct()

  plot_data <- rides %>%
    dplyr::group_by_at(dplyr::vars(
      service_change_num,
      service,
      stop_id,
      bearing_cd,
      host_street_nm,
      cross_street_nm
    )) %>%
    dplyr::select(
      service_change_num,
      service,
      stop_id,
      bearing_cd,
      host_street_nm,
      cross_street_nm,
      ons,
      offs
    ) %>%
    dplyr::summarise(
      dplyr::across(ons:offs, ~ sum(.x, na.rm = TRUE)),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(rider = ons + offs) %>%
    tidyr::pivot_longer(
      cols = ons:rider,
      names_to = 'variable',
      values_to = 'value'
    ) %>%
    dplyr::filter(
      variable %in% activity_type,
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        variable == 'ons' ~ 'Average Daily Stop Boardings',
        variable == 'offs' ~ 'Average Daily Stop Alightings',
        variable == 'rider' ~ 'Average Daily Stop Ridership',
        TRUE ~ variable
      )
    )

  if (length(service_change_num) != 1) {
    cli::cli_abort(message = "Choose only 1 service change for each map.")
  } else {
    map_type_label <- stringr::str_flatten_comma(unique(plot_data$variable))

    time_period_data <- factor(
      unique(rides$period),
      levels = c("AM Peak", "Midday", "PM Peak", "Evening", "Night"),
      labels = c("AM Peak", "Midday", "PM Peak", "Evening", "Night"),
      ordered = TRUE
    )

    time_period_label <- stringr::str_flatten_comma(sort(time_period_data))

    geo_rides <- stops |>
      dplyr::distinct(.keep_all = TRUE) |>
      dplyr::left_join(plot_data, by = c("stop_id" = "stop_id")) |>
      dplyr::left_join(
        routes_at_stop,
        by = c("stop_id", "service")
      ) |>
      tidyr::drop_na(value) |>
      sf::st_transform(4326)
  }
  if (return_type == "table") {
    geo_rides
  } else if (return_type == "interactive_map") {
    if (data_source == "LOCUS") {
      geography <- sf::read_sf(fs::path_package(
        'extdata',
        'SASR_LocusZones.shp',
        package = "ServicePlanningFunctions"
      )) |>
        janitor::clean_names() |>
        dplyr::filter(name %in% area) |>
        sf::st_transform(crs = 4326)
    } else if (data_source == "King County Council Districts") {
      geography <- sf::read_sf(fs::path_package(
        'extdata',
        'king_county_council_districts.shp',
        package = "ServicePlanningFunctions"
      )) |>
        dplyr::rename(name = area) |>
        dplyr::filter(name %in% area) |>
        sf::st_transform(4326)
    }

    pal <- leaflet::colorBin(
      palette = c(
        "#FDB71A",
        "#FF7B21",
        "#31859F",
        "#006633",
        "#390854"
      ),
      domain = plot_data$value,
      reverse = FALSE,
      pretty = FALSE,
      bins = 5
    )

    ons_map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(provider = "CartoDB.Positron") %>%
      leaflet::addPolygons(
        data = geography,
        fill = FALSE,
        color = "black",
        stroke = TRUE,
        popup = geography$name,
        weight = 5,
        dashArray = c("6")
      ) |>
      leaflet::addCircleMarkers(
        data = geo_rides,
        fillColor = ~ pal(geo_rides$value),
        stroke = FALSE,
        fillOpacity = .75,
        weight = .5,
        opacity = 1,
        color = "white",
        label = ~ geo_rides$value,
        radius = 5, #~ geo_rides$size /
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        popup = paste(
          "<b>",
          geo_rides$stop_id,
          "<br>",
          geo_rides$service,
          "</b>",
          "<br>",
          "On Street:",
          geo_rides$host_street_nm,
          "<br>",
          "Cross Street:",
          geo_rides$cross_street_nm,
          "<br>",
          "Bearing Code:",
          geo_rides$bearing_cd,
          "<br>",
          "Routes at Stop:",
          geo_rides$routes_at_stop,
          "<br>",
          map_type_label,
          ":",
          geo_rides$value,
          "<br>"
        )
      ) %>%

      leaflet::addLegend(
        position = "bottomright",
        pal = pal,
        values = plot_data$value,
        opacity = .75,
        title = paste(
          map_type_label,
          "<br>",
          time_period_label,
          "<br>"
        )
      )

    ons_map
  } else {
    cli::cli_alert_info(
      text = "Incorrect return_type parameter. Options are 'interactive_map' or 'table'."
    )
  }
}
