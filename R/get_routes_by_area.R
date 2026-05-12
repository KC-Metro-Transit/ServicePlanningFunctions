#' Get Routes by Area
#'
#' @description Returns a map or table of stops within a specified area. Areas are based on the LOCUS districts shapefile. Use show_areas() to see a map of all available areas.
#'
#' @param area Character. The name of the LOCUS or KC district of interest. Can accept multiples.
#' @param gtfs_date 'YYYY-MM-DD' Date of GTFS file to use for stop locations. Defaults to nearest dataset released before or on specified date.
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param return_type Character. Specify what you want to see. Options are "table" and "interactive_map".
#' @param data_source Character. Specify which areas to show. Options are 'LOCUS' or 'King County Council Districts'
#' @returns Table or interactive map object of routes with stops in the selected area.
#'
#' @export

get_routes_by_area <- function(
  area,
  gtfs_date,
  tbird_connection,
  return_type = 'interactive_map',
  data_source = 'LOCUS'
) {
  #get gtfs data that best matches gtfs_date from tbird

  if (length(gtfs_date) > 1) {
    min_date <- lubridate::as_date(min(gtfs_date)) - 14
    min_date <- as.character(min_date)
    max_date <- as.character(max(gtfs_date))
    capture_dates <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        "select distinct
      capture_date
      from gtfs.agency
       where (capture_date <= cast({(vals1)} as date) and
      capture_date >= cast({(vals2)} as date))",
        vals1 = max_date,
        vals2 = min_date,
        .con = tbird_connection
      )
    )

    gtfs_date_df <- tibble::as_tibble_col(
      gtfs_date,
      column_name = "selected_dates"
    ) |>
      dplyr::mutate(selected_dates = lubridate::as_date(selected_dates)) |>
      dplyr::left_join(
        capture_dates,
        by = dplyr::join_by(closest(selected_dates >= capture_date))
      ) |>
      dplyr::mutate(capture_date = paste0("'", capture_date, "'"))

    return_dates <- toString(gtfs_date_df$capture_date)

    cli::cli_alert(
      text = "Returning GTFS data for capture dates ({return_dates})"
    )
  }

  if (length(gtfs_date) > 1) {
    stops <- DBI::dbGetQuery(
      tbird_connection,
      paste0(
        "SELECT stop_id, 
      stop_lat, 
      stop_lon, 
      capture_date
      from gtfs.stops
        where capture_date in (",
        return_dates,
        ")"
      )
    ) |>
      sf::st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
      sf::st_transform(2926)
  } else {
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
  }

  if (length(gtfs_date) > 1) {
    capture_dates <- unique(stops$capture_date)

    stop_times <- DBI::dbGetQuery(
      tbird_connection,
      paste0(
        "SELECT trip_id, 
      arrival_time, 
      departure_time, 
      stop_id,
      capture_date
      from gtfs.stop_times
      where capture_date in (",
        return_dates,
        ")"
      )
    )
  } else {
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
  }

  if (length(gtfs_date) > 1) {
    trips <- DBI::dbGetQuery(
      tbird_connection,
      paste0(
        "SELECT trip_id, 
      route_id, 
      service_id, 
      block_id,
      shape_id,
      direction_id,
      capture_date
      from gtfs.trips
      where capture_date in (",
        return_dates,
        ")"
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
  } else {
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
  }
  if (length(gtfs_date) > 1) {
    gtfs_routes <- DBI::dbGetQuery(
      tbird_connection,
      paste0(
        "SELECT *
      from gtfs.routes
    where capture_date in (",
        return_dates,
        ")"
      )
    ) |>
      janitor::clean_names()
  } else {
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
      janitor::clean_names()
  }

  gtfs_routes <- gtfs_routes %>%
    dplyr::group_split(capture_date) %>%
    purrr::map(ServicePlanningFunctions::clean_service_rte_num) %>%
    dplyr::bind_rows()

  if (length(gtfs_date) > 1) {
    shapes <- DBI::dbGetQuery(
      tbird_connection,
      paste0(
        "SELECT *
      from gtfs.shapes
    where capture_date in (",
        return_dates,
        ")"
      )
    ) |>
      janitor::clean_names()
  } else {
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
  }

  # get area boundary from raw_data folder
  # filter to polygon(s) identified

  if (data_source == "LOCUS") {
    geography <- sf::read_sf(fs::path_package(
      'extdata',
      'SASR_LocusZones.shp',
      package = "ServicePlanningFunctions"
    )) |>
      dplyr::filter(NAME %in% area) |>
      sf::st_transform(2926)
  } else if (data_source == "King County Council Districts") {
    geography <- sf::read_sf(fs::path_package(
      'extdata',
      'king_county_council_districts.shp',

      package = "ServicePlanningFunctions"
    )) |>
      dplyr::rename(name = area) |>
      dplyr::filter(name %in% area) |>
      sf::st_transform(2926)
  } else {
    cli::cli_abort(
      message = "Incorrect data_source parameter. Options are 'LOCUS' or 'King County Council Districts'."
    )
  }

  #id stops & routes within boundary

  filtered_stops <- sf::st_filter(
    x = stops,
    y = geography,
    join = sf::st_intersects
  )

  stop_ids <- as.vector(filtered_stops$stop_id)

  filtered_trips <- stop_times |>
    dplyr::filter(stop_id %in% stop_ids) |>
    dplyr::inner_join(trips, by = c("capture_date", "trip_id")) |>
    dplyr::left_join(gtfs_routes) |>
    dplyr::select(
      trip_id,
      route_id,
      shape_id,
      direction,
      route_short_name,
      route_long_name,
      capture_date
    ) |>
    dplyr::distinct(
      route_id,
      shape_id,
      direction,
      route_short_name,
      route_long_name,
      capture_date,
      .keep_all = TRUE
    ) |>
    dplyr::mutate(shape_id = as.character(shape_id))

  routes_in_area <- shapes |>
    dplyr::mutate(shape_id = as.character(shape_id)) |>
    dplyr::right_join(filtered_trips) |>
    dplyr::distinct(shape_id, capture_date, .keep_all = TRUE) |>
    dplyr::mutate(
      route_name = dplyr::coalesce(route_short_name, route_long_name)
    ) |>
    dplyr::select(
      route_name,
      route_short_name,
      route_long_name,
      shape_id,
      direction,
      capture_date
    ) |>
    dplyr::group_by(shape_id, capture_date) |>
    dplyr::slice_head(n = 1)

  routes_in_area_geo <- shapes |>
    dplyr::mutate(shape_id = as.character(shape_id)) |>
    dplyr::right_join(routes_in_area) |>
    tidytransit::shapes_as_sf(crs = 4326) |>
    dplyr::left_join(routes_in_area) |>
    dplyr::mutate(
      route_date = paste(route_short_name, capture_date, sep = ": ")
    )

  if (return_type == "table") {
    routes_in_area_geo
  } else if (return_type == "interactive_map") {
    mapview::mapviewOptions(basemaps = "CartoDB.Positron")
    mapview::mapview(
      routes_in_area_geo,
      zcol = "route_date",
      layer.name = "Routes by GTFS Date"
    )
  } else {
    cli::cli_abort(
      message = "Incorrect return type parameter. Options are 'table' or 'interactive_map'. The 'table' option returns a spatial dataframe."
    )
  }
}
