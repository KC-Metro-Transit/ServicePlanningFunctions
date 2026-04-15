#' ggplot engine for stop-level ridership plots by area
#'
#' @description Generate plot of ons, offs, and load by Select Variable and Service Change for geograpic areas
#'
#' @param area Character. The name of the LOCUS or KC district of interest. Can accept multiples.
#' @param gtfs_date 'YYYY-MM-DD' Date of GTFS file to use for stop locations. Defaults to nearest dataset released before or on specified date. Can accept multiples.
#' @param service_change_num Numeric. The three-digit identifier of the service change. Can accept multiple values as a vector.
#' @param route Numeric. The route identifiers of interest. Values to be treated as characters to allow for non-numeric route identifiers. Can accept multiple values as a vector.
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param time_period Character. AM, PM, MID, XEV. XNT.
#' @param activity_type Character. ons - Average Daily Boarding, offs - Average Daily Alightings, rider - Total Stop Activity
#' @param data_source Character. Specify which areas to show. Options are 'LOCUS' or 'King County Council Districts'
#' @param x_axis Character. Grouping variable based on columns found in output from get_stop_ridership(). period, hour, route, route_name.
#'
#' @returns A ggplot2 plot showing ons, offs or total ridership at stops based on selected parameters.
#'
#' @export

#'
#'
plot_stop_ridership_by_area <- function(
  area,
  gtfs_date,
  service_change_num,
  route = "All",
  tbird_connection,
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  activity_type = 'ons',
  data_source,
  x_axis
) {
  stops <- get_stops_by_area(
    area = area,
    gtfs_date = gtfs_date,
    tbird_connection = tbird_connection,
    return_type = "table",
    data_source = data_source
  )

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
    )
  route_ids <- unique(rides$route)
  plot_stop_crosstab(
    rides,
    service_change_num = service_change_num,
    route = route_ids,
    time_period = time_period,
    x_axis = x_axis,
    activity_type = activity_type,
    stop_id = stop_ids
  )
}
