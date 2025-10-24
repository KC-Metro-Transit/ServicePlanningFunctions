#' Get Trip Ridership from T-BIRD
#' Export route level ons, offs and load for specified service change,
#' route and day from DP.VW_TRIP_SUMMARY
#'
#' @param service_change_num Numeric. The three-digit identifier of the service change.
#'  Can accept multiple values as a vector.
#' @param route Numeric. The route identifiers of interest. Values to be treated as characters to allow for non-numeric route identifiers.
#' Can accept multiple values as a vector.
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param day Character. The day type name. Options are "Weekday", "Saturday", "Sunday". Can accept multiple values as a vector.
#'
#' @returns A dataframe of trip ridership activity for selected routes, service changes and days.
#'
#' @export

get_trip_ridership <- function(
  service_change_num,
  route,
  day,
  tbird_connection
) {
  day_codes <- tibble::tibble(day, column_name = "day") |>
    dplyr::mutate(
      sched_day_type_coded_num = dplyr::case_when(
        day == 'Weekday' ~ 0,
        day == 'Saturday' ~ 1,
        day == 'Sunday' ~ 2
      )
    )

  trip_ridership <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT [SERVICE_CHANGE_NUM]
      ,[SERVICE_RTE_NUM] as route
      ,[INBD_OUTBD_CD] as direction
      ,[SCHED_DAY_TYPE_CODED_NUM] as day_code
      ,[TRIP_ID]
      ,[DAY_PART_CD]
      ,[SCHED_START_TIME_MNTS_AFTER_MIDNT] as trip_time
      ,[SCHED_START_TIME]
      ,[OBSERVED_TRIPS]
      ,[AVG_PSNGR_BOARDINGS] as ons
      ,[AVG_PSNGR_ALIGHTINGS] as offs
      ,[AVG_OF_MAX_PSNGR_LOAD] as avg_load
      ,[BUS_TYPE_NUM]
      ,[SEAT_CNT]
      ,[LOAD_FACTOR]
      ,[CROWDING_THRESHOLD_NBR]
      ,[AVG_PSNGR_MILES]
      ,[PLATFORM_HOURS]
      ,[REVENUE_HOURS]
  FROM [DP].[VW_TRIP_SUMMARY]
  WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
  AND SERVICE_RTE_NUM IN ({vals2*})
  AND SCHED_DAY_TYPE_CODED_NUM IN ({vals3*})
  ",
      vals1 = service_change,
      vals2 = route,
      vals3 = day_codes$sched_day_type_coded_num,
      .con = tbird_connection
    )
  ) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      period = dplyr::case_when(
        trip_time >= 300 & trip_time < 540 ~ 'AM Peak',
        trip_time >= 540 & trip_time < 900 ~ 'Midday',
        trip_time >= 900 & trip_time < 1140 ~ 'PM Peak',
        trip_time >= 1140 & trip_time < 1320 ~ 'Evening',
        TRUE ~ 'Night'
      ),
      hour = as.character(trip_time / 60),
      Day = dplyr::case_when(
        day_code == 0 ~ 'Weekday',
        day_code == 1 ~ 'Saturday',
        day_code == 2 ~ 'Sunday'
      )
    ) %>%
    tidyr::separate_wider_delim(
      hour,
      delim = ".",
      names = c("hour", "min"),
      too_few = "align_start"
    ) %>%
    dplyr::mutate(hour = as.integer(hour)) %>%
    dplyr::select(-min)
}
