#' Plot Route Ridership within Area
#' This function filters routes to routes with stops in a geographic area, determines ridership on the route and generates a plot. Ridership is for the entire route, not the areas used as a filter.
#' @param area Character. The name of the LOCUS district of interest. Can accept multiples.
#' @param gtfs_date 'YYYY-MM-DD' Date of GTFS file to use for stop locations. Defaults to nearest dataset released before or on specified date.
#' @param service_change_num Numeric. The three-digit identifier of the service change.
#'  Can accept multiple values as a vector if you are returning a table. For maps, select one service change at a time.
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param sched_day_type_coded_num Numeric. 0 = Weekday, 1 = Saturday, 2 = Sunday.
#' @param time_period Character. AM, PM, MID, XEV. XNT.
#' @param activity_type Character. ons - Average Daily Boarding, offs - Average Daily Alightings, avg_lod - Average Max Load.
#' @param data_source Character. Specify which areas to show. Options are 'LOCUS' or 'King County Council Districts'
#' @param x_axis Character. Only required if returning a plot. Options are 'neighborhood', 'route', 'period', 'hour'.
#' @returns Interactive map object or dataframe of route ridership summed by selected periods.
#'
#'
#'
#
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
  ) %>%
    dplyr::filter(
      day_part_cd %in% time_period,
    ) |>
    dplyr::mutate(service_rte_num = as.character(route))

  day_names <- sched_day_type_coded_num
  day_names <- stringr::str_replace(day_names, "0", "Weekday")
  day_names <- stringr::str_replace(day_names, "1", "Saturday")
  day_names <- stringr::str_replace(day_names, "2", "Sunday")

  plot_trip_crosstab(
    rides,
    service_change_num = service_change_num,
    route = route_ids,
    day = day_names,
    time_period = time_period,
    x_axis = x_axis,
    activity_type = activity_type
  )
}
