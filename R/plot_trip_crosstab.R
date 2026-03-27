#' Generate plot of ons, offs, and load by Select Variable and Service Change from get_trip_ridership()
#'
#' @param dataframe Dataframe. Output from get_trip_ridership().
#' @param service_change_num Numeric. The three-digit identifier of the service change. Can accept multiple values as a vector.
#' @param route Numeric. The route identifiers of interest. Values to be treated as characters to allow for non-numeric route identifiers. Can accept multiple values as a vector.
#' @param day Character. Day of the week. Weekday, Saturday, Sunday.
#' @param time_period Character. AM, PM, MID, XEV. XNT.
#' @param x_axis Character. Grouping variable based on columns found in output from get_trip_ridership(). day, period, hour, route, route_name.
#' @param activity_type Character. ons - Average Daily Boarding, offs - Average Daily Alightings, avg_lod - Average Max Load.
#'
#' @returns ggplot2 plot of ons, offs, and load by Select Variable and Service Change from get_trip_ridership()
#'
#' @export

plot_trip_crosstab <- function(
  dataframe,
  service_change_num,
  route,
  day = c("Weekday", "Saturday", "Sunday"),
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  x_axis,
  activity_type = 'ons'
) {
  data <- dataframe %>%
    dplyr::filter(
      day %in% day,
      day_part_cd %in% time_period,
      service_change_num %in% .env$service_change_num,
      route %in% .env$route
    )

  plot_data <- data %>%
    dplyr::group_by_at(dplyr::vars(service_change_num, service, x_axis)) %>%
    dplyr::select(
      service_change_num,
      service,
      'axis' = x_axis,
      ons:avg_load
    ) %>%
    dplyr::summarise(
      dplyr::across(ons:offs, sum, na.rm = TRUE),
      dplyr::across(avg_load, mean, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(rider = ons + offs) %>%
    tidyr::pivot_longer(
      cols = ons:rider,
      names_to = 'variable',
      values_to = 'value'
    ) %>%
    dplyr::filter(
      variable == activity_type,
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_match(
        variable,
        'ons' ~ 'Average Daily Boardings',
        'offs' ~ 'Average Daily Alightings',
        'rider' ~ 'Average Daily Ridership',
        'avg_load' ~ 'Average Max Load',
        .default = variable
      )
    )

  var_title <- unique(plot_data$variable)

  axis_title <- dplyr::case_match(
    x_axis,
    'neighborhood' ~ 'Neighborhood',
    'period' ~ 'Period',
    'hour' ~ 'Hour',
    'route' ~ 'Route',
    'route_name' ~ 'Route',
    'stop' ~ 'Stop',
    .default = stringr::str_to_title(x_axis)
  )

  day_title <- ifelse(
    length(setdiff(c("Weekday", "Saturday", "Sunday"), unique(data$day))) == 0,
    paste0('All Week'),
    paste0(unique(data$day), collapse = ", ")
  )

  period_title <- ifelse(
    length(setdiff(
      c("AM", "PM", "MID", "XEV", "XNT"),
      unique(data$day_part_cd)
    )) ==
      0,
    paste0('All Day'),
    paste0(
      sort(unique(
        data$day_part_cd
      )),
      collapse = ", "
    )
  )

  plt <- ggplot2::ggplot(
    plot_data,
    aes(
      x = stats::reorder(axis, dplyr::desc(value)),
      y = value,
      fill = stats::reorder(service, service_change_num)
    )
  )

  if (x_axis == 'period') {
    plt <- ggplot2::ggplot(
      plot_data,
      aes(
        x = axis,
        y = value,
        fill = stats::reorder(service, service_change_num)
      )
    )
  }
  if (x_axis == 'hour') {
    plot_data <- plot_data %>%
      dplyr::mutate(hour_label = as.character(axis))

    plt <- ggplot2::ggplot(
      plot_data,
      aes(
        x = stats::reorder(hour_label, axis),
        y = value,
        fill = stats::reorder(service, service_change_num)
      )
    )
  }

  plt <- plt +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    viridis::scale_fill_viridis(discrete = TRUE, name = 'Legend') +
    ggplot2::ggtitle(paste0(
      var_title,
      ' by ',
      axis_title,
      ', ',
      'Trip Ridership'
    )) +
    ggplot2::labs(subtitle = paste(day_title, period_title, sep = ", ")) +
    ggplot2::scale_x_discrete(
      labels = scales::label_wrap(10),
      guide = guide_axis(angle = 45)
    ) +
    ServicePlanningFunctions::style_kcm()
  plt
}
