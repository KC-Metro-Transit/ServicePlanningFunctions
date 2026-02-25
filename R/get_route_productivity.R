get_route_productivity <- function(
  service_change,
  tbird_connection,
  period_type,
  filter_routes,
  route
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
      " with t01 as(
select service_change_num,
service_rte_num,
mapped_trip_id,
day_part_cd,
SCHED_DAY_TYPE_CODED_NUM,
BUS_BASE_ID, 
BUS_TYPE_NUM,
count(case when trip_kind_cd in('S') THEN 1 ELSE 0 END) as revenue_trips,
sum(trip_miles) as revenue_miles,
sum(trip_duration_mnts) as revenue_minutes
FROM DP.ALL_TRIPS
WHERE SERVICE_CHANGE_NUM in ({vals1*})
AND WORKING_LOAD_STATUS = 'FINAL'
AND MINOR_CHANGE_NUM = 0
AND TRIP_KIND_CD = 'S'
AND (service_rte_num <500 OR (service_rte_num > 670 
and service_rte_num < 690))
GROUP BY service_change_num, 
service_rte_num,
mapped_trip_id,
day_part_cd,
SCHED_DAY_TYPE_CODED_NUM,
BUS_BASE_ID, 
BUS_TYPE_NUM), 

t02 as ( SELECT mapped_trip_id,
day_part_cd, 
SERVICE_CHANGE_NUM, 
SCHED_DAY_TYPE_CODED_NUM,
count(case when trip_kind_cd not in('S') THEN 1 ELSE 0 END) as nonrevenue_trips,
sum(trip_miles) as platform_miles,
sum(trip_duration_mnts) as platform_minutes
FROM DP.ALL_TRIPS
WHERE SERVICE_CHANGE_NUM in ({vals1*})
AND  WORKING_LOAD_STATUS = 'FINAL'
AND MINOR_CHANGE_NUM = 0
 AND (service_rte_num <500 OR (service_rte_num > 670 
 and service_rte_num < 690))
GROUP BY SERVICE_CHANGE_NUM,mapped_trip_id,
day_part_cd,  SCHED_DAY_TYPE_CODED_NUM)

select t02.service_change_num,
service_rte_num,
t01.mapped_trip_id,
t01.day_part_cd,
t01.SCHED_DAY_TYPE_CODED_NUM, 
BUS_BASE_ID, 
BUS_TYPE_NUM,
revenue_trips, 
nonrevenue_trips, 
revenue_miles,
platform_miles, 
case
	when t01.SCHED_DAY_TYPE_CODED_NUM = 0 then ((t02.platform_minutes/60.0))
	when t01.SCHED_DAY_TYPE_CODED_NUM = 1 then ((t02.platform_minutes/60.0))
	when t01.SCHED_DAY_TYPE_CODED_NUM = 2 then ((t02.platform_minutes/60.0))
	end as platform_hours, 
case
	when t01.SCHED_DAY_TYPE_CODED_NUM = 0 then ((platform_miles) *255)
	when t01.SCHED_DAY_TYPE_CODED_NUM = 1 then ((platform_miles) *52)
	when t01.SCHED_DAY_TYPE_CODED_NUM = 2 then ((platform_miles) *58) 
	end as annl_platform_miles
FROM t01, t02
WHERE t01.mapped_trip_id= t02.mapped_trip_id 
ORDER BY  SCHED_DAY_TYPE_CODED_NUM, service_rte_num, day_part_cd",
      vals1 = service_change,
      .con = tbird_connection
    )
  ) |>
    janitor::clean_names() |>
    dplyr::rename(trip_id = mapped_trip_id)

  trip_productivity <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT [SERVICE_CHANGE_NUM]
      ,[SCHED_DAY_TYPE_CODED_NUM]
      ,[DAY_PART_CD]
      ,[SERVICE_RTE_NUM]
      ,[EXPRESS_LOCAL_CD]
      ,[INBD_OUTBD_CD]
      ,[TRIP_ID]
      ,[AVG_PSNGR_MILES]
      ,[OBSERVED_TRIPS]
      ,[AVG_PSNGR_BOARDINGS] as ons
  FROM [DP].[VW_TRIP_SUMMARY]
 WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
  AND (service_rte_num <500 OR (service_rte_num > 670 
  and service_rte_num < 690))",
      vals1 = service_change,
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
        day_part_cd
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
        svc_family
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
          is.na(day_part_cd) ~ 'Day',
          TRUE ~ day_part_cd
        )
      )

    productivity <- productivity_table %>%
      dplyr::left_join(
        lookup_thresholds,
        by = dplyr::join_by(
          service_change_num,
          svc_family,
          day_part_cd,
          sched_day_type_coded_num
        )
      ) %>%
      dplyr::mutate(
        bottom_rides = dplyr::case_when(
          rides_per_platform_hour <= bottom_25_threshold_rides ~ 1,
          TRUE ~ 0
        ),
        top_rides = dplyr::case_when(
          rides_per_platform_hour >= top_25_threshold_rides ~ 1,
          TRUE ~ 0
        ),
        bottom_miles = dplyr::case_when(
          psngr_miles_per_platform_mile <= bottom_25_threshold_miles ~ 1,
          TRUE ~ 0
        ),
        top_miles = dplyr::case_when(
          psngr_miles_per_platform_mile >= top_25_threshold_miles ~ 1,
          TRUE ~ 0
        )
      )

    if (filter_routes == FALSE) {
      cli::cli_inform(message = "Returning all routes.")
      productivity
    } else {
      cli::cli_inform(message = "Filtering by provided route list.")
      out <- productivity |>
        dplyr::filter(service_rte_num %in% route)
    }
  } else {
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
        productivity_period
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
      ) %>%
      dplyr::mutate(
        weekday = dplyr::case_when(
          sched_day_type_coded_num == 0 ~ "Weekday",
          sched_day_type_coded_num == 1 ~ "Saturday",
          sched_day_type_coded_num == 2 ~ "Sunday"
        )
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
      )

    productivity_period <- route_productivity %>%
      dplyr::left_join(
        producitivity_period_thresholds,
        by = dplyr::join_by(
          service_change_num,
          svc_family,
          productivity_period,
          sched_day_type_coded_num
        )
      ) %>%
      dplyr::mutate(
        bottom_rides = dplyr::case_when(
          rides_per_platform_hour <= bottom_25_threshold_rides ~ 1,
          TRUE ~ 0
        ),
        top_rides = dplyr::case_when(
          rides_per_platform_hour >= top_25_threshold_rides ~ 1,
          TRUE ~ 0
        ),
        bottom_miles = dplyr::case_when(
          psngr_miles_per_platform_mile <= bottom_25_threshold_miles ~ 1,
          TRUE ~ 0
        ),
        top_miles = dplyr::case_when(
          psngr_miles_per_platform_mile >= top_25_threshold_miles ~ 1,
          TRUE ~ 0
        )
      )
    if (filter_routes == FALSE) {
      cli::cli_inform(message = "Returning all routes.")
      productivity_period
    } else {
      cli::cli_inform(message = "Filtering by provided route list.")
      out <- productivity_period |>
        dplyr::filter(service_rte_num %in% route)
    }
  }
}
