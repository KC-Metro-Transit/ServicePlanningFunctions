#' Get Block Group Equity Classification
#' This function pulls the dp.block_group_classification table in TBIRD.
#'
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param acs_year Numeric. The last year of the 5 year ACS rolling average for demographic info. Can accept multiples.
#' @param return_type Character. Options are "table", "spatial" and "interactive_map"
#'
#' @returns A dataframe of equity data by block group and year.
#'
#' @export

get_epa_scores <- function(
  tbird_connection,
  acs_year = 2024,
  return_type = 'table'
) {
  scores <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT GEOID,
        EQUITY_PRIORITY_AREA_SCORE, 
        ACS_YR
  FROM DP.BLOCK_GROUP_CLASSIFICATION
  WHERE ACS_YR IN ({vals1*})
      ",
      vals1 = acs_year,
      .con = tbird_connection
    )
  ) |>
    janitor::clean_names() |>
    dplyr::rename(acs_year = acs_yr) |>
    dplyr::mutate(geoid = as.character(geoid))

  if (length(acs_year) > 1) {
    scores <- tidyr::pivot_wider(
      scores,
      id_cols = geoid,
      names_from = acs_year,
      values_from = equity_priority_area_score,
      names_prefix = "epa_score_"
    )
  } else {
    scores
  }

  if (return_type == 'table') {
    return(scores)
  } else if (return_type == 'spatial' | return_type == "interactive_map") {
    geography <- sf::read_sf(fs::path_package(
      'extdata',
      'blkgrp20_shore.shp',
      package = "ServicePlanningFunctions"
    )) |>
      sf::st_transform(2926) |>
      dplyr::rename(geoid = GEO_ID_GRP)
    bg_scores <- geography |>
      dplyr::left_join(scores, by = "geoid")
  } else {
    cli::cli_abort(
      message = "Incorrect return_type parameter. Options are 'spatial', 'interative_map' and 'table'."
    )
  }
  if (return_type == 'spatial') {
    bg_scores
  } else if (return_type == 'interactive_map' & length(acs_year) == 1) {
    mapview::mapviewOptions(basemaps = "Stadia.AlidadeSmooth")

    mapview::mapview(
      bg_scores,
      zcol = "equity_priority_area_score",
      layer.name = paste("Equity Priority Area Scores", acs_year)
    )
  } else {
    cli::cli_abort(
      message = "Select only 1 ACS year at a time if you want an interactive map."
    )
  }
}
