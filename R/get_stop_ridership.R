get_stop_ridership <- function(
  service_change_num,
  route,
  stop_id,
  tbird_connection
) {
  if (stop_id == "All") {
    stop_data <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        "
  SELECT [SERVICE_CHANGE_NUM]
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
  } else {
    DBI::dbGetQuery(
      con,
      glue::glue_sql(
        "

  stop_data <-  SELECT [SERVICE_CHANGE_NUM]
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

    stop_data |>
      janitor::clean_names() %>%
      dplyr::mutate(
        time_period = case_when(
          arrive >= 300 & arrive < 540 ~ 'AM Peak',
          arrive >= 540 & arrive < 900 ~ 'Midday',
          arrive >= 900 & arrive < 1140 ~ 'PM Peak',
          arrive >= 1140 & arrive < 1320 ~ 'Evening',
          TRUE ~ 'Night'
        ),
        hour = as.character(arrive / 60),
        Day = 'Weekday'
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
}
