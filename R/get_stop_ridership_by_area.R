#' @param x_axis Character. Grouping variable based on columns found in output from get_stop_ridership(). day, period, hour

#' Title
#'
#' @param area
#' @param gtfs_date
#' @param service_change_num
#' @param tbird_connection
#' @param return_type
#' @param day
#' @param time_period
#' @param x_axis
#' @param activity_type
#'
#' @returns
#'
#' @export
#' @examples
get_stop_ridership_by_area <- function(
  area,
  gtfs_date,
  service_change_num,
  tbird_connection,
  return_type,
  day = c("Weekday", "Saturday", "Sunday"),
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  x_axis,
  activity_type = 'ons'
) {
  stops <- get_stops_by_area(
    area = area,
    gtfs_date = gtfs_date,
    tbird_connection = tbird_connection,
    return_type = "table"
  ) |>
    sf::st_as_sf()
  print(class(stops))
  stop_ids <- unique(stops$stop_id)

  rides <- get_stop_ridership(
    service_change_num = service_change_num,
    stop_id = stop_ids,
    route = "All",
    tbird_connection = tbird_connection
  )

  data <- rides %>%
    dplyr::filter(
      day %in% day,
      day_part_cd %in% time_period,
      service_change_num %in% .env$service_change_num #,
      # route %in% .env$route
    ) %>%
    dplyr::rename(period = time_period_at_stop)

  plot_data <- data %>%
    dplyr::group_by_at(dplyr::vars(
      service_change_num,
      service,
      x_axis,
      stop_id
    )) %>%
    dplyr::select(
      service_change_num,
      service,
      stop_id,
      'axis' = x_axis,
      ons,
      offs
    ) %>%
    dplyr::summarise(
      dplyr::across(ons:offs, sum, na.rm = TRUE),
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

  geo_rides <- stops |>
    dplyr::left_join(plot_data, by = c("stop_id" = "stop_id"))

  mapview::mapview(geo_rides, zcol = "value")
}
