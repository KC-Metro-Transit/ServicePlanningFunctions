#'
#' Generate a dataframe of
#' productivity thresholds for both riders per platform hour and
#' passenger miles per platform mile. This function allows the user
#' to either return results for the whole system for a service change
#' or return a subset of results. The subsets are evaluated against all
#' routes before being returned. The function also supports both Metro
#' productivity period analysis and day part level analysis. Multiple
#' service changes are also supported.
#'
#' @param service_change Numeric. The three-digit identifier of the service change.
#' Can accept multiple values as a vector.
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param period_type Character. Level of time aggregation. Options are "day_part_cd" or "service_guidelines".
#' @param sched_day_type_coded_num Numeric. Day of the week. 0 - Weekday, 1 - Saturday, 2 - Sunday. Can accept multiples.
#'
#' @returns Dataframe of productivity thresholds for specified days and service changes. Includes both riders per platform hour and
#' passenger miles per platform mile.
#'
#' @export

get_productivity_thresholds <- function(
  service_change,
  tbird_connection,
  period_type,
  sched_day_type_coded_num
) {
  route_classification <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT [SERVICE_CHANGE_NUM], 
    SERVICE_RTE_NUM, 
    SVC_FAMILY, 
    operator_agency_cd
    from dp.route_classification
    WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
     ",
      vals1 = service_change,
      .con = tbird_connection
    )
  ) |>
    janitor::clean_names() %>%
    dplyr::arrange(service_change_num, service_rte_num) |>
    dplyr::distinct(service_rte_num, service_change_num, .keep_all = T)

  all_trips <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      " SELECT [SERVICE_CHANGE_NUM], 
        [SERVICE_RTE_NUM] ,
        [SCHED_DAY_TYPE_CODED_NUM],
        [DAY_PART_CD],
        sum(trip_duration_mnts) / 60.0 as platform_hours , 
        sum(trip_miles) as platform_miles
          FROM [DP].[ALL_TRIPS]
          where [SERVICE_CHANGE_NUM] IN ({vals1*})
          AND [WORKING_LOAD_STATUS] = 'FINAL'
          AND minor_change_num = '0'
          AND (service_rte_num <500 OR (service_rte_num > 670 
          and service_rte_num < 690))
          AND SCHED_DAY_TYPE_CODED_NUM IN ({vals2*})
          GROUP BY [SERVICE_CHANGE_NUM], [SERVICE_RTE_NUM], [SCHED_DAY_TYPE_CODED_NUM], [DAY_PART_CD]
  ",
      vals1 = service_change,
      vals2 = sched_day_type_coded_num,
      .con = tbird_connection
    )
  ) %>%
    janitor::clean_names()

  trip_productivity <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT [SERVICE_CHANGE_NUM]
      ,[SCHED_DAY_TYPE_CODED_NUM]
      ,[DAY_PART_CD]
      ,[SERVICE_RTE_NUM]
      ,[EXPRESS_LOCAL_CD]
      ,sum([AVG_PSNGR_MILES]) as avg_psngr_miles
      ,count([OBSERVED_TRIPS]) as trip_count
      ,sum([AVG_PSNGR_BOARDINGS]) as ons
  FROM [DP].[VW_TRIP_SUMMARY]
 WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
  AND (service_rte_num <500 OR (service_rte_num > 670 
  and service_rte_num < 690))
    AND SCHED_DAY_TYPE_CODED_NUM IN ({vals2*})
  GROUP BY [SERVICE_CHANGE_NUM], 
  [SERVICE_RTE_NUM],  
   [EXPRESS_LOCAL_CD],
  [SCHED_DAY_TYPE_CODED_NUM], [DAY_PART_CD]",
      vals1 = service_change,
      vals2 = sched_day_type_coded_num,
      .con = tbird_connection
    )
  ) %>%
    dplyr::mutate(
      productivity_period = dplyr::case_when(
        DAY_PART_CD == "AM" & SCHED_DAY_TYPE_CODED_NUM == 0 ~ "Peak",
        DAY_PART_CD == "MID" & SCHED_DAY_TYPE_CODED_NUM == 0 ~ "Off-Peak",
        DAY_PART_CD == "PM" & SCHED_DAY_TYPE_CODED_NUM == 0 ~ "Peak",
        DAY_PART_CD == "XEV" & SCHED_DAY_TYPE_CODED_NUM == 0 ~ "Night",
        DAY_PART_CD == "XNT" & SCHED_DAY_TYPE_CODED_NUM == 0 ~ "Night",
        DAY_PART_CD == "AM" & SCHED_DAY_TYPE_CODED_NUM != 0 ~ "Off-Peak",
        DAY_PART_CD == "MID" & SCHED_DAY_TYPE_CODED_NUM != 0 ~ "Off-Peak",
        DAY_PART_CD == "PM" & SCHED_DAY_TYPE_CODED_NUM != 0 ~ "Off-Peak",
        DAY_PART_CD == "XEV" & SCHED_DAY_TYPE_CODED_NUM != 0 ~ "Night",
        DAY_PART_CD == "XNT" & SCHED_DAY_TYPE_CODED_NUM != 0 ~ "Night"
      )
    ) |>
    janitor::clean_names() %>%
    dplyr::arrange(
      service_change_num,
      service_rte_num,
      sched_day_type_coded_num
    ) |>
    dplyr::left_join(route_classification) |>
    dplyr::left_join(all_trips) |>
    dplyr::filter(!(operator_agency_cd == 'HPL' | operator_agency_cd == 'SVT'))

  if (period_type == "day_part_cd") {
    cli::cli_inform(
      message = "Grouping productivity results by AM, MID, PM, XEV, XNT definitions. Whole-day productivity included in results as well."
    )
    # weekday by period
    route_productivity_day_part <- trip_productivity %>%
      dplyr::group_by(
        service_change_num,
        service_rte_num,
        express_local_cd,
        sched_day_type_coded_num,
        svc_family,
        day_part_cd,
        trip_count
      ) %>%
      dplyr::summarise(
        ons = sum(ons, na.rm = T),
        platform_miles = sum(platform_miles, na.rm = T),
        avg_psngr_miles = sum(avg_psngr_miles, na.rm = T),
        platform_hours = sum(platform_hours, na.rm = T)
      ) %>%
      dplyr::mutate(
        rides_per_platform_hour = (ons / platform_hours),
        psngr_miles_per_platform_mile = (avg_psngr_miles / platform_miles)
      )

    day_part_thresholds <- route_productivity_day_part %>%
      dplyr::group_by(
        service_change_num,
        sched_day_type_coded_num,
        svc_family,
        day_part_cd
      ) %>%
      dplyr::summarise(
        bottom_25_threshold_rides = stats::quantile(
          rides_per_platform_hour,
          probs = c(.25),
          na.rm = T
        ),
        top_25_threshold_rides = stats::quantile(
          rides_per_platform_hour,
          probs = c(.75),
          na.rm = T
        ),
        bottom_25_threshold_miles = stats::quantile(
          psngr_miles_per_platform_mile,
          probs = c(.25),
          na.rm = T
        ),
        top_25_threshold_miles = stats::quantile(
          psngr_miles_per_platform_mile,
          probs = c(.75),
          na.rm = T
        )
      )

    #  day level stats
    route_productivity_day <- trip_productivity %>%
      janitor::clean_names() %>%
      dplyr::group_by(
        service_change_num,
        service_rte_num,
        express_local_cd,
        sched_day_type_coded_num,
        svc_family,
        trip_count
      ) %>%
      dplyr::summarise(
        ons = sum(ons, na.rm = T),
        platform_miles = sum(platform_miles, na.rm = T),
        avg_psngr_miles = sum(avg_psngr_miles, na.rm = T),
        platform_hours = sum(platform_hours, na.rm = T)
      ) %>%
      dplyr::mutate(
        rides_per_platform_hour = (ons / platform_hours),
        psngr_miles_per_platform_mile = (avg_psngr_miles / platform_miles)
      ) |>
      dplyr::mutate(day_part_cd = "DAY")

    day_thresholds <- route_productivity_day %>%
      dplyr::group_by(
        service_change_num,
        sched_day_type_coded_num,
        svc_family
      ) %>%
      dplyr::summarise(
        bottom_25_threshold_rides = stats::quantile(
          rides_per_platform_hour,
          probs = c(.25),
          na.rm = T
        ),
        top_25_threshold_rides = stats::quantile(
          rides_per_platform_hour,
          probs = c(.75),
          na.rm = T
        ),
        bottom_25_threshold_miles = stats::quantile(
          psngr_miles_per_platform_mile,
          probs = c(.25),
          na.rm = T
        ),
        top_25_threshold_miles = stats::quantile(
          psngr_miles_per_platform_mile,
          probs = c(.75),
          na.rm = T
        )
      )

    ## put it all together
    productivity_table <- dplyr::bind_rows(
      route_productivity_day_part,
      route_productivity_day
    ) %>%
      dplyr::mutate(
        weekday = dplyr::case_when(
          sched_day_type_coded_num == 0 ~ "Weekday",
          sched_day_type_coded_num == 1 ~ "Saturday",
          sched_day_type_coded_num == 2 ~ "Sunday"
        )
      )

    lookup_thresholds <- day_part_thresholds %>%
      dplyr::bind_rows(day_thresholds) %>%
      dplyr::mutate(
        day_part_cd = dplyr::case_when(
          is.na(day_part_cd) ~ 'DAY',
          TRUE ~ day_part_cd
        )
      )

    lookup_thresholds
  } else if (period_type == "service_guidelines") {
    cli::cli_inform(
      message = "Grouping productivity results by Peak, Off-Peak, and Night definitions."
    )
    # weekday by period
    route_productivity <- trip_productivity %>%
      dplyr::group_by(
        service_change_num,
        service_rte_num,
        express_local_cd,
        sched_day_type_coded_num,
        svc_family,
        productivity_period,
        trip_count
      ) %>%
      dplyr::summarise(
        ons = sum(ons, na.rm = T),
        platform_miles = sum(platform_miles, na.rm = T),
        avg_psngr_miles = sum(avg_psngr_miles, na.rm = T),
        platform_hours = sum(platform_hours, na.rm = T)
      ) %>%
      dplyr::mutate(
        rides_per_platform_hour = (ons / platform_hours),
        psngr_miles_per_platform_mile = (avg_psngr_miles / platform_miles)
      )

    producitivity_period_thresholds <- route_productivity %>%
      dplyr::group_by(
        service_change_num,
        sched_day_type_coded_num,
        svc_family,
        productivity_period
      ) %>%
      dplyr::summarise(
        bottom_25_threshold_rides = stats::quantile(
          rides_per_platform_hour,
          probs = c(.25),
          na.rm = T
        ),
        top_25_threshold_rides = stats::quantile(
          rides_per_platform_hour,
          probs = c(.75),
          na.rm = T
        ),
        bottom_25_threshold_miles = stats::quantile(
          psngr_miles_per_platform_mile,
          probs = c(.25),
          na.rm = T
        ),
        top_25_threshold_miles = stats::quantile(
          psngr_miles_per_platform_mile,
          probs = c(.75),
          na.rm = T
        )
      ) %>%
      dplyr::mutate(
        day = dplyr::case_when(
          sched_day_type_coded_num == 0 ~ "Weekday",
          sched_day_type_coded_num == 1 ~ "Saturday",
          sched_day_type_coded_num == 2 ~ "Sunday"
        )
      )
  } else {
    cli::cli_abort(
      message = "Missing period_type parameter. Options are 'day_part_cd' or 'service_guidelines'."
    )
  }
}
