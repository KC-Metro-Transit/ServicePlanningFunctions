#' Show Areas
#' Returns either a table of available district names or a map showing the district boundaries.
#'  Based on LOCUS districts. Designed to be used with get_stops_by_area() and get_routes_by_area().
#'
#' @param return_type Character. Specify what you want to see. Options are "table" and "interactive_map".
#' @param data_source Character. Specify which areas to show. Options are 'LOCUS' or 'King County Council Districts'
#' @returns A list of area names or an interactive map of area boundaries.
#'
#' @export

show_areas <- function(return_type = "interactive_map", data_source = "LOCUS") {
  if (data_source == "LOCUS") {
    geography <- sf::read_sf(here::here('data_raw', 'SASR_LocusZones.shp'))
  } else if (data_source == "King County Council Districts") {
    geography <- sf::read_sf(here::here(
      'data_raw',
      'king_county_council_districts.shp'
    )) |>
      dplyr::rename(name = area)
  } else {
    cli::cli_abort(
      message = "Incorrect data_source  parameter. Options are 'LOCUS' or 'King County Council Districts'."
    )
  }

  if (return_type == "table") {
    out <- geography |>
      sf::st_drop_geometry() |>
      janitor::clean_names() |>
      dplyr::select(name)
    out
  } else if (return_type == "interactive_map") {
    out <- geography |>
      sf::st_transform(2926) |>
      janitor::clean_names()

    mapview::mapview(out, zcol = "name")
  } else {
    cli::cli_abort(
      message = "Incorrect return type parameter. Options are 'table' or 'interactive_map'."
    )
  }
}
