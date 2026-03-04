#' Generate plot of ons, offs, and load by Route and Service Change from get_trip_ridership()
#'
#' @param dataframe Dataframe. Output from get_trip_ridership().
#' @param service_change_num Numeric. The three-digit identifier of the service change. Can accept multiple values as a vector.
#' @param route Numeric. The route identifiers of interest. Values to be treated as characters to allow for non-numeric route identifiers. Can accept multiple values as a vector.
#' @param day Character. Day of the week. Weekday, Saturday, Sunday.
#' @param time_period Character. AM, PM, MID, XEV. XNT.
#' @param activity_type Character. ons - Average Daily Boarding, offs - Average Daily Alightings, avg_lod - Average Max Load.
#'
#' @returns ggplot2 object of Export trip level ons, offs, and load from DP.VW_TRIP_SUMMARY
#'
#' @export

plot_route_by_service_change <- function(
  dataframe,
  service_change_num,
  route,
  day = c("Weekday", "Saturday", "Sunday"),
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  activity_type = 'ons'
) {
  data <- dataframe %>%
    dplyr::filter(
      day %in% day,
      day_part_cd %in% time_period,
      service_change_num %in% .env$service_change_num,
      route %in% .env$route
    ) %>%
    dplyr::group_by(service_change_num, service, route, day) %>%
    dplyr::summarise(
      dplyr::across(ons:offs, sum, na.rm = TRUE),
      dplyr::across(avg_load, mean, na.rm = TRUE),
      .groups = 'keep'
    ) %>%
    tidyr::pivot_longer(
      cols = ons:avg_load,
      names_to = 'variable',
      values_to = 'value'
    ) %>%
    dplyr::mutate(value = round(value, digits = 1)) %>%
    dplyr::filter(
      variable == activity_type,
    )

  sub_title <- ''

  period_title <- ifelse(
    length(setdiff(
      c("AM", "PM", "MID", "XEV", "XNT"),
      unique(dplyr::filter(dataframe, day_part_cd %in% time_period)$day_part_cd)
    )) ==
      0,
    paste0('All Day'),
    paste0(
      sort(unique(
        dplyr::filter(dataframe, day_part_cd %in% time_period)$day_part_cd
      )),
      collapse = ", "
    )
  )

  if (length(route) > 15) {
    # top 15 routes in latest service change
    data1 <- data %>%
      dplyr::group_by(service_change_num, service, route) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::desc(service_change_num), dplyr::desc(value)) %>%
      head(15)

    subset_route_list <- unlist(list(unique(data1$route)))

    data <- data %>%
      dplyr::group_by(service_change_num, service, route, day) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
      dplyr::filter(route %in% subset_route_list)

    sub_title <- ', Top 15'
  }

  var_title <- dplyr::case_when(
    activity_type == 'ons' ~ 'Average Daily Boardings',
    activity_type == 'offs' ~ 'Average Daily Alightings',
    activity_type == 'avg_load' ~ 'Average Max Load'
  )

  day_title <- ifelse(
    length(setdiff(c("Weekday", "Saturday", "Sunday"), unique(data$day))) == 0,
    paste0('All Week'),
    paste0(unique(data$Day), collapse = ", ")
  )

  plt <- ggplot2::ggplot(
    data,
    aes(
      x = stats::reorder(route, dplyr::desc(value)),
      y = value,
      fill = service
    )
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::ggtitle(paste0(var_title, ' by Route', sub_title)) +
    ggplot2::scale_x_discrete(
      labels = scales::label_wrap(10),
      guide = ggplot2::guide_axis(angle = 45)
    ) +
    ggplot2::labs(subtitle = paste(day_title, period_title, sep = ", ")) +
    ggplot2::facet_wrap(~day) +
    ServicePlanningFunctions::style_kcm()
  plt
}
