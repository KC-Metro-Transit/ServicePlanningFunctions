#' Get Route Productivity
#'
#' @description get_route_productivity() generates a dataframe of
#' productivity metrics for both riders per platform hour and
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
#' @param filter_routes T/F. Do you want to return results for the entire system or for a selection of routes.
#' @param route Numeric. Required only if filter_routes == TRUE. The route identifiers of interest. Values to be treated as characters to allow for non-numeric route identifiers.
#' Can accept multiple values as a vector.
#'
#' @returns Dataframe of productivity metrics for specified routes, periods and service changes. Includes both riders per platform hour and
#' passenger miles per platform mile.
#'
#' @export

get_route_productivity <- function(
  service_change = 251,
  tbird_connection,
  period_type = "service_guidelines",
  sched_day_type_coded_num = c(0, 1, 2),
  filter_routes = FALSE,
  route = NULL
) {
  current_service_change_df <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "select service_change_num 
            from edw.dim_date
             where full_date =  cast(({vals1*}) as date)
            ",
      vals1 = lubridate::today(),
      .con = tbird_connection
    )
  )

  current_service_change <- as.numeric(current_service_change_df$service_change)

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

  # if you are getting data for the current service change or if you want data to be grouped by day part, you need to use dp.vw_trip_summary.
  if (
    (period_type == "service_guidelines" &
      current_service_change %in% service_change &
      length(service_change) == 1) |
      period_type == "day_part_cd"
  ) {
    trip_productivity <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        "SELECT [SERVICE_CHANGE_NUM] 
                                ,SCHED_DAY_TYPE_CODED_NUM
                                ,[DAY_PART_CD]
                                ,[SERVICE_RTE_NUM]
                                ,[EXPRESS_LOCAL_CD]
                                , max(sched_start_time_mnts_after_midnt) as last_trip
                                , min(sched_start_time_mnts_after_midnt) as first_trip
                                ,sum(trip_miles) as platform_miles
                                ,sum(platform_hours) as platform_hours
                                ,sum([AVG_PSNGR_MILES]) as avg_psngr_miles
                                ,count(trip_id) as trip_count
                                ,sum([AVG_PSNGR_BOARDINGS]) as ons
                                FROM [DP].[VW_TRIP_SUMMARY]
                                 WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
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
      ) %>%
      dplyr::mutate(dplyr::across(platform_miles:ons, ~ as.numeric(.))) %>%
      janitor::clean_names() %>%
      dplyr::arrange(
        service_change_num,
        service_rte_num,
        sched_day_type_coded_num
      ) |>
      dplyr::left_join(route_classification) %>%
      dplyr::group_by(
        service_change_num,
        service_rte_num,
        #express_local_cd,
        sched_day_type_coded_num,
        svc_family,
        productivity_period,
        .drop = F
      ) %>%
      dplyr::summarise(
        # this summarize clause is duplicated to sum across am/pm peak periods before reassigning productivity periods based on trip counts and trip start times.
        ons = sum(ons, na.rm = T),
        platform_miles = sum(as.numeric(platform_miles), na.rm = T),
        avg_psngr_miles = sum(as.numeric(avg_psngr_miles), na.rm = T),
        platform_hours = sum(as.numeric(platform_hours), na.rm = T),
        trip_count = sum(as.numeric(trip_count), na.rm = T),
        last_trip = max(last_trip, na.rm = T),
        first_trip = min(first_trip, na.rm = T)
      ) %>%
      dplyr::mutate(
        productivity_period = dplyr::case_when(
          productivity_period == "Night" &
            sched_day_type_coded_num == 0 &
            trip_count < 5 &
            last_trip < 1200 ~ "Peak",
          productivity_period == "Night" &
            sched_day_type_coded_num != 0 &
            trip_count < 5 &
            last_trip < 1200 ~ "Off-Peak",
          productivity_period == "Peak" &
            sched_day_type_coded_num == 0 &
            trip_count < 8 &
            last_trip < 1020 &
            first_trip > 450 ~ "Off-Peak",
          TRUE ~ productivity_period
        )
      )
  } else if (
    period_type == "service_guidelines" &
      !(current_service_change %in% service_change)
  ) {
    #use dp.trip_productivity for official productivity numbers for past service changes
    trip_productivity <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        "Select [SERVICE_CHANGE_NUM] 
                     ,SCHED_DAY_TYPE_CODED_NUM
                     ,[SERVICE_RTE_NUM]
                     ,[EXPRESS_LOCAL_CD]
                     ,productivity_period
                     ,sum(annl_sched_plat_miles) as platform_miles
                     ,sum(annl_sched_plat_hrs) as platform_hours
                     ,sum([Annl_PSNGR_MILES]) as avg_psngr_miles
                     ,sum([Annl_PSNGR_BOARDINGS]) as ons
                     ,count(trip_id) as trip_count
                     FROM [DP].[trip_productivity]
                     WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
                     AND SCHED_DAY_TYPE_CODED_NUM IN ({vals2*})
                     GROUP BY [SERVICE_CHANGE_NUM], 
                     [SERVICE_RTE_NUM],  
                     [EXPRESS_LOCAL_CD],
                     [SCHED_DAY_TYPE_CODED_NUM], 
                        productivity_period",
        vals1 = service_change,
        vals2 = sched_day_type_coded_num,
        .con = tbird_connection
      )
    ) %>%
      janitor::clean_names() %>%
      # Assuming standard annualization factor when converting to daily averages (some routes can have different annualization factor per trip)
      dplyr::mutate(dplyr::across(
        platform_miles:ons,
        ~ dplyr::case_when(
          sched_day_type_coded_num == 0 ~ . / 255,
          sched_day_type_coded_num == 1 ~ . / 52,
          sched_day_type_coded_num == 2 ~ . / 58
        )
      )) %>%
      dplyr::arrange(
        service_change_num,
        service_rte_num,
        sched_day_type_coded_num
      ) |>
      dplyr::left_join(route_classification) %>%
      dplyr::mutate(dplyr::across(platform_miles:ons, ~ as.numeric(.)))
  } else if (
    (period_type == "service_guidelines" &
      current_service_change %in% service_change &
      length(service_change) != 1)
  ) {
    #get both trip_productivity data for past sc and vw_trip_summary for current sc. Warn user about multiple data sources
    past_service_changes <- service_change[
      !service_change %in% current_service_change
    ]

    trip_productivity_past <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        "Select [SERVICE_CHANGE_NUM] 
                     ,SCHED_DAY_TYPE_CODED_NUM
                     ,[SERVICE_RTE_NUM]
                     ,[EXPRESS_LOCAL_CD]
                     ,productivity_period
                     ,sum(annl_sched_plat_miles) as platform_miles
                     ,sum(annl_sched_plat_hrs) as platform_hours
                     ,sum([Annl_PSNGR_MILES]) as avg_psngr_miles
                     ,sum([Annl_PSNGR_BOARDINGS]) as ons
                     ,count(trip_id) as trip_count
                     FROM [DP].[trip_productivity]
                     WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
                     AND SCHED_DAY_TYPE_CODED_NUM IN ({vals2*})
                     GROUP BY [SERVICE_CHANGE_NUM], 
                     [SERVICE_RTE_NUM],  
                     [EXPRESS_LOCAL_CD],
                     [SCHED_DAY_TYPE_CODED_NUM], 
                        productivity_period",
        vals1 = past_service_changes,
        vals2 = sched_day_type_coded_num,
        .con = tbird_connection
      )
    ) %>%
      janitor::clean_names() %>%
      # Assuming standard annualization factor when converting to daily averages (some routes can have different annualization factor per trip)
      dplyr::mutate(dplyr::across(
        platform_miles:ons,
        ~ dplyr::case_when(
          sched_day_type_coded_num == 0 ~ . / 255,
          sched_day_type_coded_num == 1 ~ . / 52,
          sched_day_type_coded_num == 2 ~ . / 58
        )
      )) %>%
      dplyr::arrange(
        service_change_num,
        service_rte_num,
        sched_day_type_coded_num
      ) |>
      dplyr::left_join(route_classification)

    trip_productivity_current <- DBI::dbGetQuery(
      tbird_connection,
      glue::glue_sql(
        "SELECT [SERVICE_CHANGE_NUM] 
                                ,SCHED_DAY_TYPE_CODED_NUM
                                ,[DAY_PART_CD]
                                ,[SERVICE_RTE_NUM]
                                ,[EXPRESS_LOCAL_CD]
                                , max(sched_start_time_mnts_after_midnt) as last_trip
                                , min(sched_start_time_mnts_after_midnt) as first_trip
                                ,sum(trip_miles) as platform_miles
                                ,sum(platform_hours) as platform_hours
                                ,sum([AVG_PSNGR_MILES]) as avg_psngr_miles
                                ,count(trip_id) as trip_count
                                ,sum([AVG_PSNGR_BOARDINGS]) as ons
                                FROM [DP].[VW_TRIP_SUMMARY]
                                 WHERE [SERVICE_CHANGE_NUM] IN ({vals1*})
                                  AND SCHED_DAY_TYPE_CODED_NUM IN ({vals2*})
                                  GROUP BY [SERVICE_CHANGE_NUM], 
                                  [SERVICE_RTE_NUM],  
                                   [EXPRESS_LOCAL_CD],
                                  [SCHED_DAY_TYPE_CODED_NUM], [DAY_PART_CD]",
        vals1 = current_service_change,
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
      ) %>%

      janitor::clean_names() %>%
      dplyr::arrange(
        service_change_num,
        service_rte_num,
        sched_day_type_coded_num
      ) |>
      dplyr::left_join(route_classification) %>%
      dplyr::mutate(dplyr::across(platform_miles:ons, ~ as.numeric(.))) %>%
      dplyr::group_by(
        service_change_num,
        service_rte_num,
        #express_local_cd,
        sched_day_type_coded_num,
        svc_family,
        productivity_period,
        .drop = F
      ) %>%
      dplyr::summarise(
        # this summarize clause is duplicated to sum across am/pm peak periods before reassigning productivity periods based on trip counts and trip start times.
        ons = sum(ons, na.rm = T),
        platform_miles = sum(as.numeric(platform_miles), na.rm = T),
        avg_psngr_miles = sum(as.numeric(avg_psngr_miles), na.rm = T),
        platform_hours = sum(as.numeric(platform_hours), na.rm = T),
        trip_count = sum(as.numeric(trip_count), na.rm = T),
        last_trip = max(last_trip, na.rm = T),
        first_trip = min(first_trip, na.rm = T)
      ) %>%
      dplyr::mutate(
        productivity_period = dplyr::case_when(
          productivity_period == "Night" &
            sched_day_type_coded_num == 0 &
            trip_count < 5 &
            last_trip < 1200 ~ "Peak",
          productivity_period == "Night" &
            sched_day_type_coded_num != 0 &
            trip_count < 5 &
            last_trip < 1200 ~ "Off-Peak",
          productivity_period == "Peak" &
            sched_day_type_coded_num == 0 &
            trip_count < 8 &
            last_trip < 1020 &
            first_trip > 450 ~ "Off-Peak",
          TRUE ~ productivity_period
        )
      )
  }

  if (period_type == "day_part_cd") {
    cli::cli_inform(
      message = "Grouping productivity results by AM, MID, PM, XEV, XNT definitions. Whole-day productivity included in results as well."
    )
    cli::cli_warn(
      message = "Data drawn from dp.vw_trip_summary which can produce numbers slightly different than office system evaluation productivity figures."
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
        median_threshold_rides = stats::quantile(
          rides_per_platform_hour,
          probs = c(.50),
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
        median_threshold_miles = stats::quantile(
          psngr_miles_per_platform_mile,
          probs = c(.50),
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
        platform_hours = sum(platform_hours, na.rm = T),
        trip_count = sum(trip_count, na.rm = T)
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
        median_threshold_rides = stats::quantile(
          rides_per_platform_hour,
          probs = c(.50),
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
        median_threshold_miles = stats::quantile(
          psngr_miles_per_platform_mile,
          probs = c(.50),
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
        day = dplyr::case_when(
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
        rides_quantile = dplyr::case_when(
          rides_per_platform_hour < bottom_25_threshold_rides ~ 1,
          rides_per_platform_hour >= bottom_25_threshold_rides &
            rides_per_platform_hour < median_threshold_rides ~ 2,
          rides_per_platform_hour >= median_threshold_rides &
            rides_per_platform_hour < top_25_threshold_rides ~ 3,
          rides_per_platform_hour >= top_25_threshold_rides ~ 4,
          TRUE ~ 0
        ),

        miles_quantile = dplyr::case_when(
          psngr_miles_per_platform_mile < bottom_25_threshold_miles ~ 1,
          psngr_miles_per_platform_mile >= bottom_25_threshold_miles &
            psngr_miles_per_platform_mile < median_threshold_miles ~ 2,
          psngr_miles_per_platform_mile >= median_threshold_miles &
            psngr_miles_per_platform_mile < top_25_threshold_miles ~ 3,
          psngr_miles_per_platform_mile >= top_25_threshold_miles ~ 4,
          TRUE ~ 0
        )
      ) |>
      dplyr::select(
        -c(
          dplyr::starts_with("bottom_25") |
            dplyr::starts_with("top_25") |
            dplyr::starts_with("median")
        )
      ) |>
      # Convert Service Change Num to Service Change Name (Spring/Summer/Fall YYYY)
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
      ServicePlanningFunctions::clean_service_rte_name(as.character(
        service_rte_num
      )) %>%
      dplyr::rename(route_name = clean_route) %>%
      dplyr::select(-c(yr, season, year, season_num))

    if (filter_routes == FALSE) {
      cli::cli_inform(message = "Returning all routes.")
      productivity
    } else {
      cli::cli_inform(message = "Filtering by provided route list: {route}.")
      out <- productivity |>
        dplyr::filter(service_rte_num %in% route)
    }
  } else if (period_type == "service_guidelines") {
    cli::cli_inform(
      message = "Grouping productivity results by Peak, Off-Peak, and Night definitions."
    )

    if (
      (period_type == "service_guidelines" &
        current_service_change %in% service_change &
        length(service_change) != 1)
    ) {
      route_productivity_df <- dplyr::bind_rows(
        trip_productivity_past,
        trip_productivity_current
      )

      cli::cli_warn(
        message = "Results include data for current service change which may differ from final System Evaluation results."
      )
    } else if (
      (period_type == "service_guidelines" &
        current_service_change %in% service_change &
        length(service_change) == 1)
    ) {
      route_productivity_df <- trip_productivity
      cli::cli_warn(
        message = "Returning data for current service change which may differ from final System Evaluation results."
      )
    } else {
      route_productivity_df <- trip_productivity
    }

    route_productivity <- route_productivity_df %>%
      dplyr::group_by(
        service_change_num,
        service_rte_num,
        # express_local_cd, pbi dashboard does not group by E/L
        sched_day_type_coded_num,
        svc_family,
        productivity_period
      ) %>%
      dplyr::summarise(
        ons = sum(ons, na.rm = T),
        platform_miles = sum(platform_miles, na.rm = T),
        avg_psngr_miles = sum(avg_psngr_miles, na.rm = T),
        platform_hours = sum(platform_hours, na.rm = T),
        trip_count = sum(trip_count, na.rm = T)
      ) %>%
      dplyr::mutate(
        rides_per_platform_hour = (ons / platform_hours),
        psngr_miles_per_platform_mile = (avg_psngr_miles / platform_miles)
      ) %>%
      dplyr::mutate(
        day = dplyr::case_when(
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
        median_threshold_rides = stats::quantile(
          rides_per_platform_hour,
          probs = c(.50),
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
        median_threshold_miles = stats::quantile(
          psngr_miles_per_platform_mile,
          probs = c(.50),
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
        rides_quantile = dplyr::case_when(
          rides_per_platform_hour < bottom_25_threshold_rides ~ 1,
          rides_per_platform_hour >= bottom_25_threshold_rides &
            rides_per_platform_hour < median_threshold_rides ~ 2,
          rides_per_platform_hour >= median_threshold_rides &
            rides_per_platform_hour < top_25_threshold_rides ~ 3,
          rides_per_platform_hour >= top_25_threshold_rides ~ 4,
          TRUE ~ 0
        ),

        miles_quantile = dplyr::case_when(
          psngr_miles_per_platform_mile < bottom_25_threshold_miles ~ 1,
          psngr_miles_per_platform_mile >= bottom_25_threshold_miles &
            psngr_miles_per_platform_mile < median_threshold_miles ~ 2,
          psngr_miles_per_platform_mile >= median_threshold_miles &
            psngr_miles_per_platform_mile < top_25_threshold_miles ~ 3,
          psngr_miles_per_platform_mile >= top_25_threshold_miles ~ 4,
          TRUE ~ 0
        )
      ) |>
      dplyr::select(
        -c(
          dplyr::starts_with("bottom_25") |
            dplyr::starts_with("top_25") |
            dplyr::starts_with("median")
        )
      ) |>
      # Convert Service Change Num to Service Change Name (Spring/Summer/Fall YYYY)
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
      ServicePlanningFunctions::clean_service_rte_name(as.character(
        service_rte_num
      )) %>%
      dplyr::rename(route_name = clean_route) %>%
      dplyr::select(-c(yr, season, year, season_num))

    if (filter_routes == FALSE) {
      cli::cli_inform(message = "Returning all routes.")
      productivity_period
    } else {
      cli::cli_inform(message = "Filtering by provided route list: {route}")
      out <- productivity_period |>
        dplyr::filter(service_rte_num %in% route)
    }
  } else {
    cli::cli_abort(
      message = "Missing period_type parameter. Options are 'day_part_cd' or 'service_guidelines'."
    )
  }
}
