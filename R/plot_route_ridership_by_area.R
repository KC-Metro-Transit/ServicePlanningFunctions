#' Plot Route Ridership within Area
#' This function filters routes to routes with stops in a geographic area, determines ridership on the route and generates a plot. Ridership is for the entire route, not the areas used as a filter.
#' @param area
#' @param gtfs_date
#' @param service_change_num
#' @param tbird_connection
#' @param sched_day_type_coded_num
#' @param time_period
#' @param activity_type
#' @param data_source
#' @param x_axis
#'
#' @returns A ggplot2 plot of route ridership.
#'
#' @export
#'
plot_route_ridership_by_area <- function(
  area,
  gtfs_date,
  service_change_num,
  tbird_connection,
  sched_day_type_coded_num,
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  activity_type = 'ons',
  data_source,
  x_axis
) {
  routes <- get_routes_by_area(
    area = area,
    gtfs_date = gtfs_date,
    tbird_connection = tbird_connection,
    data_source = data_source,
    return_type = "table"
  )

  route_ids <- unique(routes$service_rte_num)

  rides <- get_trip_ridership(
    service_change_num = service_change_num,
    route = route_ids,
    tbird_connection = tbird_connection,
    sched_day_type_coded_num = sched_day_type_coded_num
  )
  # %>%
  #   dplyr::filter(
  #     day_part_cd %in% time_period,
  #     service_change_num %in% .env$service_change_num,
  #     sched_day_type_coded_num %in% .env$sched_day_type_coded_num
  #   ) |>
  #   dplyr::mutate(service_rte_num = as.character(route))

  day_names <- sched_day_type_coded_num
  day_names <- stringr::str_replace(day_names, "0", "Weekday")
  day_names <- stringr::str_replace(day_names, "1", "Saturday")
  day_names <- stringr::str_replace(day_names, "2", "Sunday")

  plot_trip_crosstab(
    rides,
    service_change_num = service_change_num,
    route = route_ids,
    day = day_names,
    time_period = c("AM", "PM", "MID", "XEV", "XNT"),
    x_axis = x_axis,
    activity_type = activity_type
  )
}
