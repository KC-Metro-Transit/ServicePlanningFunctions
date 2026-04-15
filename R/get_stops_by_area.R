#' Get Stops by Area
#'
#' @description Returns a map or table of stops within a specified area. Areas are based on the LOCUS districts shapefile. Use show_areas() to see a map of all available areas.
#'
#' @param area Character. The name of the LOCUS district of interest. Can accept multiples.
#' @param gtfs_date 'YYYY-MM-DD' Date of GTFS file to use for stop locations. Defaults to nearest dataset released before or on specified date. Can accept multiples.
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param return_type Character. Specify what you want to see. Options are "table" and "interactive_map".
#' @param data_source Character. Specify which areas to show. Options are 'LOCUS' or 'King County Council Districts'
#' @returns Interactive map object or table of stops in selected area.
#'
#' @export

get_stops_by_area <- function(
  area,
  gtfs_date,
  tbird_connection,
  return_type,
  data_source
) {
  #get gtfs data that best matches gtfs_date from tbird
  min_date <- as.character(min(gtfs_date))
  max_date <- as.character(max(gtfs_date))

  if (length(gtfs_date) > 1) {
    stops <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        "select * from (
      SELECT stop_id, 
      stop_lat, 
      stop_lon, 
      capture_date,
      ROW_NUMBER() OVER(PARTITION BY STOP_ID ORDER BY [capture_date] DESC) AS rn
      from gtfs.stops
      where (capture_date <= cast({(vals1)} as date) and
      capture_date >= cast({(vals2)} as date))) A
      WHERE rn = 1 ",
        vals1 = max_date,
        vals2 = min_date,
        .con = tbird_connection
      )
    ) |>
      sf::st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
      sf::st_transform(2926) |>
      dplyr::select(-rn)
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

  #get area boundary from raw_data folder
  #filter to polygon(s) identified

  if (data_source == "LOCUS") {
    geography <- sf::read_sf(fs::path_package(
      'extdata',
      'SASR_LocusZones.shp',
      package = "ServicePlanningFunctions"
    )) |>
      janitor::clean_names() |>
      dplyr::filter(name %in% .env$area) |>
      sf::st_transform(2926)
  } else if (data_source == "King County Council Districts") {
    geography <- sf::read_sf(fs::path_package(
      'extdata',
      'king_county_council_districts.shp'
    )) |>
      dplyr::rename(name = area) |>
      dplyr::filter(name %in% .env$area) |>
      sf::st_transform(2926)
  } else {
    cli::cli_abort(
      message = "Incorrect data_source  parameter. Options are 'LOCUS' or 'King County Council Districts'."
    )
  }

  #id stops & routes within boundary

  filtered_stops <- sf::st_filter(
    x = stops,
    y = geography,
    join = sf::st_intersects
  ) |>
    sf::st_transform(4326)

  if (return_type == "table") {
    filtered_stops
  } else if (return_type == "interactive_map") {
    mapview::mapviewOptions(basemaps = "CartoDB.Positron")
    mapview::mapview(map_data, zcol = "stop_id")
  } else {
    cli::cli_abort(
      message = "Incorrect return type parameter. Options are 'table' or 'interactive_map'. The 'table' option returns a spatial dataframe."
    )
  }
}
