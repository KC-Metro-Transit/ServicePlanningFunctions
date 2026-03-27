#' Get Routes by Area
#'
#' Returns a map or table of stops within a specified area. Areas are based on the LOCUS districts shapefile. Use show_areas() to see a map of all available areas.
#'
#' @param area Character. The name of the LOCUS district of interest. Can accept multiples.
#' @param gtfs_date 'YYYY-MM-DD' Date of GTFS file to use for stop locations. Defaults to nearest dataset released before or on specified date.
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param return_type Character. Specify what you want to see. Options are "table" and "interactive_map".
#'
#' @returns Table or interactive map object of routes with stops in the selected area.
#'
#' @export

get_routes_by_area <- function(
  area,
  gtfs_date,
  tbird_connection,
  return_type
) {
  #get gtfs data that best matches gtfs_date from tbird

  stops <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT stop_id, 
      stop_lat, 
      stop_lon, 
      capture_date
      from gtfs.stops
      where capture_date = (SELECT top 1 capture_date
      from gtfs.stops
      where capture_date <= cast({(vals1)} as date)
      order by capture_date desc)",
      vals1 = gtfs_date,
      .con = tbird_connection
    )
  ) |>
    sf::st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
    sf::st_transform(2926)

  stop_times <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT trip_id, 
      arrival_time, 
      departure_time, 
      stop_id,
      capture_date
      from gtfs.stop_times
      where capture_date = (SELECT top 1 capture_date
      from gtfs.stop_times
      where capture_date <= cast({(vals1)} as date)
      order by capture_date desc)",
      vals1 = gtfs_date,
      .con = tbird_connection
    )
  )
  trips <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT trip_id, 
      route_id, 
      service_id, 
      block_id,
      shape_id,
      direction_id,
      capture_date
      from gtfs.trips
      where capture_date = (SELECT top 1 capture_date
      from gtfs.trips
      where capture_date <= cast({(vals1)} as date)
      order by capture_date desc)",
      vals1 = gtfs_date,
      .con = tbird_connection
    )
  ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      direction = dplyr::case_when(
        direction_id == 1 ~ 'I',
        direction_id == 0 ~ 'O',
        TRUE ~ ""
      )
    )

  gtfs_routes <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT *
      from gtfs.routes
      where capture_date = (SELECT top 1 capture_date
      from gtfs.routes
      where capture_date <= cast({(vals1)} as date)
      order by capture_date desc)",
      vals1 = gtfs_date,
      .con = tbird_connection
    )
  ) |>
    janitor::clean_names() |>
    dplyr::select(-capture_date)

  gtfs_clean_routes <- ServicePlanningFunctions::clean_service_rte_num(
    gtfs_routes,
    netplan_gtfs = FALSE
  )

  shapes <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT *
      from gtfs.shapes
      where capture_date = (SELECT top 1 capture_date
      from gtfs.shapes
      where capture_date <= cast({(vals1)} as date)
      order by capture_date desc)",
      vals1 = gtfs_date,
      .con = tbird_connection
    )
  ) |>
    janitor::clean_names()

  shapes <- tidytransit::shapes_as_sf(shapes, crs = 4326)
  # get area boundary from raw_data folder
  # filter to polygon(s) identified

  geography <- sf::read_sf(here::here('data_raw', 'SASR_LocusZones.shp')) |>
    dplyr::filter(NAME %in% area) |>
    sf::st_transform(2926)

  #id stops & routes within boundary

  filtered_stops <- sf::st_filter(
    x = stops,
    y = geography,
    join = sf::st_intersects
  )

  stop_ids <- as.vector(filtered_stops$stop_id)

  filtered_trips <- stop_times |>
    dplyr::filter(stop_id %in% stop_ids) |>
    dplyr::left_join(trips) |>
    dplyr::left_join(gtfs_clean_routes) |>
    dplyr::select(
      trip_id,
      route_id,
      shape_id,
      direction,
      service_rte_num,
      route_short_name,
      route_long_name
    ) |>
    dplyr::distinct(
      trip_id,
      route_id,
      shape_id,
      service_rte_num,
      direction,
      route_short_name,
      route_long_name
    ) |>
    dplyr::mutate(shape_id = as.character(shape_id))

  routes_in_area <- shapes |>
    dplyr::filter(shape_id %in% filtered_trips$shape_id) |>
    dplyr::left_join(
      filtered_trips
    ) |>
    dplyr::select(
      service_rte_num,
      route_short_name,
      route_long_name,
      shape_id,
      direction,
      geometry
    )

  if (return_type == "table") {
    routes_in_area
  } else if (return_type == "interactive_map") {
    mapview::mapview(routes_in_area, zcol = "service_rte_num")
  } else {
    cli::cli_abort(
      message = "Incorrect return type parameter. Options are 'table' or 'interactive_map'. The 'table' option returns a spatial dataframe."
    )
  }
}
