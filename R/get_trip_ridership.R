#' Get Trip Ridership from T-BIRD Export trip level ons, offs, and load from DP.VW_TRIP_SUMMARY
#'
#' @param service_change_num Numeric. The three-digit identifier of the service change. Can accept multiple values as a vector.
#' @param route Numeric. The route identifiers of interest. Values to be treated as characters to allow for non-numeric route identifiers. Can accept multiple values as a vector.
#' @param sched_day_type_coded_num Numeric. Day of the week. 0 - Weekday, 1 - Saturday, 2 - Sunday.
#' @param tbird_connection The connection object created by connect_to_tbird()
#'
#' @returns
#'
#' @export
#' @examples
get_trip_ridership <- function(
  service_change_num,
  route,
  sched_day_type_coded_num,
  tbird_connection
) {
  DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "
SELECT [SERVICE_CHANGE_NUM]
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
      vals1 = service_change_num,
      vals2 = route,
      vals3 = sched_day_type_coded_num,
      .con = tbird_connection
    )
  ) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      period = case_when(
        trip_time >= 300 & trip_time < 540 ~ 'AM Peak',
        trip_time >= 540 & trip_time < 900 ~ 'Midday',
        trip_time >= 900 & trip_time < 1140 ~ 'PM Peak',
        trip_time >= 1140 & trip_time < 1320 ~ 'Evening',
        TRUE ~ 'Night'
      ),
      hour = as.character(trip_time / 60),
      Day = case_when(
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
    filter(route != 907) %>%
    # Convert Service Change Num to Service Change Name (Spring/Summer/Fall YYYY)
    dplyr::mutate(
      yr = str_replace(service_change_num, "[1-3]$", ""), # Extract Year Digits
      season_num = str_extract(service_change_num, "[1-3]$"), # Extract Season Digit
      # Convert Season Digit to Text
      season = case_match(
        season_num,
        "1" ~ "Spring",
        "2" ~ "Summer",
        "3" ~ "Fall"
      ),
      # Convert Year Digits to YYYY
      year = case_when(
        yr > 90 ~ str_c("19", yr), # 1991-1999
        yr == "" ~ "2000", # 2000
        yr %in% 1:10 ~ str_c("200", yr), # 2001-2009
        yr >= 10 ~ str_c("20", yr), # > 2010
        TRUE ~ NA
      ),
      Day = factor(Day, levels = c("Weekday", "Saturday", "Sunday")),
      Service = reorder(
        paste(season, year),
        as.numeric(str_c(year, season_num))
      )
    ) %>%
    clean_service_rte_name(as.character(route)) %>%
    dplyr::rename(Route = clean_route) %>%
    dplyr::select(-c(min, yr, season, year))
}
