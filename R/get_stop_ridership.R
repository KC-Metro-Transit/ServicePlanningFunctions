#' Get Stop Ridership from T-BIRD
#' Export stop level ons, offs and load from DP.STOP_ACTIVITY_DETAIL
#' @param service_change_num Numeric. The three-digit identifier of the service change.
#'  Can accept multiple values as a vector.
#' @param route Numeric. The route identifiers of interest or "All" to return all routes. Values to be treated as characters to allow for non-numeric route identifiers.
#' Can accept multiple values as a vector.
#' @param stop_id A vector of stop IDs or "All" if all stops on a route are of interest.
#' @param tbird_connection The connection object created by connect_to_tbird()
#'
#' @returns A dataframe of stop activity for selected routes, service changes and stops.
#'
#' @export

get_stop_ridership <- function(
  service_change_num,
  route,
  stop_id,
  tbird_connection
) {
  return_all_stops <- if ("All" %in% stop_id) {
    TRUE
  } else {
    FALSE
  }

  return_all_routes <- if ("All" %in% route) {
    TRUE
  } else {
    FALSE
  }

  if (return_all_stops == TRUE & return_all_routes == TRUE) {
    cli::cli_alert(text = "Returning system.")
    stop_data <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        " SELECT [SERVICE_CHANGE_NUM]
      ,[SERVICE_RTE_NUM] as route
      ,[TRIP_ID]
      ,[INBD_OUTBD_CD] as direction
      ,[STOP_ID]
      ,[BEARING_CD]
      ,[HOST_STREET_NM]
      ,[CROSS_STREET_NM]
      ,[JURISDICTION_CD]
      ,[STOP_SEQUENCE_NUM]
      ,[AVG_ARRIVAL_CLOCK_TIME]
      ,[AVG_ARRIVAL_MNTS_AFTER_MIDNT] as arrive
      ,[DAY_PART_CD]
      ,[AVG_PSNGR_BOARDINGS] as ons
      ,[AVG_PSNGR_ALIGHTINGS] as offs
      ,[AVG_LOAD] as avg_load
      ,[OBSERVED_TRIPS]
  FROM [DP].[STOP_ACTIVITY_DETAIL]
  WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
  ",
        vals1 = service_change_num,
        .con = tbird_connection
      )
    )
  } else if (return_all_stops == TRUE & return_all_routes == FALSE) {
    cli::cli_alert(text = "Returning all stops on selected routes")
    stop_data <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        "SELECT [SERVICE_CHANGE_NUM]
      ,[SERVICE_RTE_NUM] as route
      ,[TRIP_ID]
      ,[INBD_OUTBD_CD] as direction
      ,[STOP_ID]
      ,[BEARING_CD]
      ,[HOST_STREET_NM]
      ,[CROSS_STREET_NM]
      ,[JURISDICTION_CD]
      ,[STOP_SEQUENCE_NUM]
      ,[AVG_ARRIVAL_CLOCK_TIME]
      ,[AVG_ARRIVAL_MNTS_AFTER_MIDNT] as arrive
      ,[DAY_PART_CD]
      ,[AVG_PSNGR_BOARDINGS] as ons
      ,[AVG_PSNGR_ALIGHTINGS] as offs
      ,[AVG_LOAD] as avg_load
      ,[OBSERVED_TRIPS]
  FROM [DP].[STOP_ACTIVITY_DETAIL]
  WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
AND SERVICE_RTE_NUM IN ({vals2*})
  ",
        vals1 = service_change_num,
        vals2 = route,
        .con = tbird_connection
      )
    )
  } else if (return_all_stops == FALSE & return_all_routes == TRUE) {
    cli::cli_alert(text = "Returning data for selected stops.")
    stop_data <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        "SELECT [SERVICE_CHANGE_NUM]
      ,[SERVICE_RTE_NUM] as route
      ,[TRIP_ID]
      ,[INBD_OUTBD_CD] as direction
      ,[STOP_ID]
      ,[BEARING_CD]
      ,[HOST_STREET_NM]
      ,[CROSS_STREET_NM]
      ,[JURISDICTION_CD]
      ,[STOP_SEQUENCE_NUM]
      ,[AVG_ARRIVAL_CLOCK_TIME]
      ,[AVG_ARRIVAL_MNTS_AFTER_MIDNT] as arrive
      ,[DAY_PART_CD]
      ,[AVG_PSNGR_BOARDINGS] as ons
      ,[AVG_PSNGR_ALIGHTINGS] as offs
      ,[AVG_LOAD] as avg_load
      ,[OBSERVED_TRIPS]
  FROM [DP].[STOP_ACTIVITY_DETAIL]
  WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
AND stop_id IN ({vals2*})
  ",
        vals1 = service_change_num,
        vals2 = stop_id,
        .con = tbird_connection
      )
    )
  } else {
    cli::cli_alert(
      text = "Returning data filtered by route and stop."
    )
    stop_data <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        " SELECT [SERVICE_CHANGE_NUM]
      ,[SERVICE_RTE_NUM] as route
      ,[TRIP_ID]
      ,[INBD_OUTBD_CD] as direction
      ,[STOP_ID]
      ,[BEARING_CD]
      ,[HOST_STREET_NM]
      ,[CROSS_STREET_NM]
      ,[JURISDICTION_CD]
      ,[STOP_SEQUENCE_NUM]
      ,[AVG_ARRIVAL_CLOCK_TIME]
      ,[AVG_ARRIVAL_MNTS_AFTER_MIDNT] as arrive
      ,[DAY_PART_CD]
      ,[AVG_PSNGR_BOARDINGS] as ons
      ,[AVG_PSNGR_ALIGHTINGS] as offs
      ,[AVG_LOAD] as avg_load
      ,[OBSERVED_TRIPS]
  FROM [DP].[STOP_ACTIVITY_DETAIL]
  WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
  AND SERVICE_RTE_NUM IN ({vals2*})
  AND STOP_ID IN ({vals3*})
  ",
        vals1 = service_change_num,
        vals2 = route,
        vals3 = stop_id,
        .con = tbird_connection
      )
    )
  }

  stop_data |>
    janitor::clean_names() %>%
    dplyr::mutate(
      period = factor(
        dplyr::case_when(
          trip_time >= 300 & trip_time < 540 ~ 'AM Peak',
          trip_time >= 540 & trip_time < 900 ~ 'Midday',
          trip_time >= 900 & trip_time < 1140 ~ 'PM Peak',
          trip_time >= 1140 & trip_time < 1320 ~ 'Evening',
          TRUE ~ 'Night'
        ),
        levels = c("AM Peak", "Midday", "PM Peak", 'Evening', 'Night')
      ),
      hour = as.character(arrive / 60),
      day = 'Weekday'
    ) %>%
    tidyr::separate_wider_delim(
      hour,
      delim = ".",
      names = c("hour", "min"),
      too_few = "align_start"
    ) %>%
    dplyr::mutate(hour = as.integer(hour)) %>%
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
      day = factor(day, levels = c("Weekday", "Saturday", "Sunday")),
      service = stats::reorder(
        paste(season, year),
        as.numeric(stringr::str_c(year, season_num))
      )
    ) %>%
    ServicePlanningFunctions::clean_service_rte_name(as.character(route)) %>%
    dplyr::rename(route_name = clean_route) %>%
    dplyr::select(-c(min, yr, season, year, season_num))
}
