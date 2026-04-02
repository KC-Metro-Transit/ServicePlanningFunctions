#' Get Route Ridership by Area
#'
#' This function returns either an interactive map of route  ridership for the selected area or a dataframe showing ridership data for routes that serve the selected area.

#'
#' @param area Character. The name of the LOCUS district of interest. Can accept multiples.
#' @param gtfs_date 'YYYY-MM-DD' Date of GTFS file to use for stop locations. Defaults to nearest dataset released before or on specified date.
#' @param service_change_num Numeric. The three-digit identifier of the service change.
#'  Can accept multiple values as a vector if you are returning a table. For maps, select one service change at a time.
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param return_type Character. Specify what you want to see. Options are "table" and "interactive_map".
#' @param sched_day_type_coded_num Numeric. 0 = Weekday, 1 = Saturday, 2 = Sunday. Can accept multiples.
#' @param time_period Character. AM, PM, MID, XEV. XNT.
#' @param activity_type Character. ons - Average Daily Boarding, offs - Average Daily Alightings, avg_lod - Average Max Load.
#' @param data_source Character. Specify which areas to show. Options are 'LOCUS' or 'King County Council Districts'
#'
#' @returns Interactive map object or dataframe of route ridership summed by selected periods.
#'
#'
#' @export

get_route_ridership_by_area <- function(
  area,
  gtfs_date,
  service_change_num,
  tbird_connection,
  return_type,
  sched_day_type_coded_num,
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  activity_type = 'ons',
  data_source
) {
  routes <- get_routes_by_area(
    area = area,
    gtfs_date = gtfs_date,
    tbird_connection = tbird_connection,
  )

  route_ids <- unique(routes$service_rte_num)

  rides <- get_trip_ridership(
    service_change_num = service_change_num,
    sched_day_type_coded_num = sched_day_type_coded_num,
    route = route_ids,
    tbird_connection = tbird_connection
  ) %>%
    dplyr::filter(
      day_part_cd %in% time_period,
      service_change_num %in% service_change_num
    ) |>
    dplyr::mutate(service_rte_num = as.character(route))

  plot_data <- rides %>%
    dplyr::group_by_at(dplyr::vars(
      service_change_num,
      service,
      route_name,
      service_rte_num
    )) %>%
    dplyr::select(
      service_change_num,
      service,
      route_name,
      service_rte_num,
      ons,
      avg_load,
      offs
    ) %>%
    dplyr::summarise(
      dplyr::across(ons:offs, sum, na.rm = TRUE),
      dplyr::across(avg_load, mean, na.rm = TRUE),
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
        variable == 'ons' ~ 'Average Daily Boardings',
        variable == 'offs' ~ 'Average Daily Alightings',
        variable == 'rider' ~ 'Average Daily Ridership',
        variable == 'avg_load' ~ 'Average Max Load ',
        TRUE ~ variable
      )
    )

  if (length(service_change_num) != 1) {
    cli::cli_abort(message = "Chose only 1 service change for each map.")
  } else {
    map_type_label <- stringr::str_flatten_comma(unique(plot_data$variable))

    time_period_data <- factor(
      unique(rides$period),
      levels = c("AM Peak", "Midday", "PM Peak", "Evening", "Night"),
      labels = c("AM Peak", "Midday", "PM Peak", "Evening", "Night"),
      ordered = TRUE
    )

    time_period_label <- stringr::str_flatten_comma(sort(time_period_data))

    day_data <- factor(
      unique(rides$day),
      levels = c("Weekday", "Saturday", "Sunday"),
      labels = c("Weekday", "Saturday", "Sunday"),
      ordered = TRUE
    )
    day_label <- stringr::str_flatten_comma(sort(day_data))
    geo_rides <- routes |>
      dplyr::left_join(
        plot_data,
        by = c("service_rte_num" = "service_rte_num")
      ) |>
      sf::st_transform(4326)
  }
  if (return_type == "table") {
    plot_data
  } else if (return_type == "interactive_map") {
    if (data_source == "LOCUS") {
      geography <- sf::read_sf(fs::path_package(
        'inst',
        'extdata',
        'SASR_LocusZones.shp',
        package = "ServicePlanningFunctions"
      )) |>
        janitor::clean_names() |>
        dplyr::filter(name %in% area) |>
        sf::st_transform(crs = 4326)
    } else if (data_source == "King County Council Districts") {
      geography <- sf::read_sf(fs::path_package(
        'inst',
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
      leaflet::addPolylines(
        data = geo_rides,
        color = ~ pal(geo_rides$value),
        stroke = TRUE,
        weight = 3,
        opacity = 1,
        label = ~ geo_rides$value,
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        popup = paste(
          "<b>",
          geo_rides$route_name,
          "<br>",
          geo_rides$service,
          "</b>",
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
          day_label,
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
