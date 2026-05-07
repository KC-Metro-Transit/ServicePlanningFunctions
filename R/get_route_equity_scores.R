#' Get Route Equity
#' This function pulls the dp.route_classification table in TBIRD.
#'
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param route Numeric. The route identifiers of interest. Values to be treated as characters to allow for non-numeric route identifiers.
#' Can accept multiple values as a vector.
#' @param service_change Numeric. The three-digit identifier of the service change.
#' Can accept multiple values as a vector.
#'
#' @returns A dataframe of equity data by route and service change.
#'
#' @export
#| !exists(route)
get_route_equity_scores <- function(
  tbird_connection,
  route = NULL,
  service_change = 253
) {
  if (is.null(route)) {
    routes <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        "SELECT SERVICE_RTE_NUM, 
SERVICE_CHANGE_NUM, 
EXPRESS_LOCAL_CD, 
SVC_FAMILY, TOTAL_STOP_CNT, 
MEAN_STOP_OPPORTUNITY_SCORE, 
EQUITY_STOPS_CNT, 
NON_EQUITY_STOPS_CNT, 
PCT_STOPS_IN_PRIORITY_BG, 
RTE_OPPORTUNITY_INDEX_SCORE
  FROM DP.ROUTE_CLASSIFICATION
  WHERE SERVICE_CHANGE_NUM IN ({vals1*})
      ",
        vals1 = service_change,
        .con = tbird_connection
      )
    )
  } else {
    routes <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        "SELECT SERVICE_RTE_NUM, 
SERVICE_CHANGE_NUM, 
EXPRESS_LOCAL_CD, 
SVC_FAMILY, TOTAL_STOP_CNT, 
MEAN_STOP_OPPORTUNITY_SCORE, 
EQUITY_STOPS_CNT, 
NON_EQUITY_STOPS_CNT, 
PCT_STOPS_IN_PRIORITY_BG, 
RTE_OPPORTUNITY_INDEX_SCORE
  FROM DP.ROUTE_CLASSIFICATION
  WHERE SERVICE_CHANGE_NUM IN ({vals1*})
    and service_rte_num in ({vals2*})
      ",
        vals1 = service_change,
        vals2 = route,
        .con = tbird_connection
      )
    )
  }

  route_equity_df <- routes |>
    janitor::clean_names() |>
    dplyr::mutate(
      yr = stringr::str_replace(service_change_num, "[1-3]$", ""), # Extract Year Digits
      season_num = stringr::str_extract(service_change_num, "[1-3]$"), # Extract Season Digit
      # Convert Season Digit to Text
      season = dplyr::case_match(
        season_num,
        "1" ~ "Spring",
        "2" ~ "Summer",
        "3" ~ "Fall"
      ),
      # Convert Year Digits to YYYY
      year = dplyr::case_when(
        yr > 90 ~ stringr::str_c("19", yr), # 1991-1999
        yr == "" ~ "2000", # 2000
        yr %in% 1:10 ~ stringr::str_c("200", yr), # 2001-2009
        yr >= 10 ~ stringr::str_c("20", yr), # > 2010
        TRUE ~ NA
      ),
      service = stats::reorder(
        paste(season, year),
        as.numeric(stringr::str_c(year, season_num))
      )
    ) %>%
    ServicePlanningFunctions::clean_service_rte_name(as.character(
      service_rte_num
    )) %>%
    dplyr::rename(route_name = clean_route) %>%
    dplyr::select(-c(yr, season, year, season_num))
}
