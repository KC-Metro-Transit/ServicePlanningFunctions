#' Show Areas
#' Returns either a table of available district names or a map showing the district boundaries.
#'  Based on LOCUS districts. Designed to be used with get_stops_by_area() and get_routes_by_area().
#'
#' @param return_type Character. Specify what you want to see. Options are "table" and "interactive_map".
#'
#' @returns A list of area names or an interactive map of area boundaries.
#'
#' @export

show_areas <- function(return_type = "interactive_map") {
  geography <- sf::read_sf(here::here('data_raw', 'SASR_LocusZones.shp'))

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
