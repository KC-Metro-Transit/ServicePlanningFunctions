#' Generate plot of ons, offs, and load by Select Variable and Service Change from get_stop_ridership()
#'
#' @param dataframe Dataframe. Output from get_trip_ridership().
#' @param service_change_num Numeric. The three-digit identifier of the service change. Can accept multiple values as a vector.
#' @param route Numeric. The route identifiers of interest. Values to be treated as characters to allow for non-numeric route identifiers. Can accept multiple values as a vector.
#' @param day Character. Day of the week. Weekday, Saturday, Sunday.
#' @param time_period Character. AM, PM, MID, XEV. XNT.
#' @param x_axis Character. Grouping variable based on columns found in output from get_trip_ridership(). Day, period, hour.
#' @param activity_type Character. ons - Average Daily Boarding, offs - Average Daily Alightings, avg_lod - Average Max Load.
#'
#' @returns
#'
#' @export
#' @examples
plot_stop_crosstab <- function(
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
      Day %in% day,
      day_part_cd %in% time_period,
      service_change_num %in% .env$service_change_num,
      route %in% .env$route
    ) %>%
    dplyr::rename(period = time_period_at_stop)

  plot_data <- data %>%
    dplyr::group_by_at(vars(service_change_num, Service, x_axis)) %>%
    dplyr::select(service_change_num, Service, 'axis' = x_axis, ons, offs) %>%
    dplyr::summarise(across(ons:offs, sum, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::mutate(rider = ons + offs) %>%
    pivot_longer(
      cols = ons:rider,
      names_to = 'variable',
      values_to = 'value'
    ) %>%
    filter(
      variable %in% activity_type,
    ) %>%
    mutate(
      variable = case_when(
        variable == 'ons' ~ 'Average Daily Stop Boardings',
        variable == 'offs' ~ 'Average Daily Stop Alightings',
        variable == 'rider' ~ 'Average Daily Stop Ridership',
        TRUE ~ variable
      )
    )

  var_title <- unique(plot_data$variable)

  axis_title <- dplyr::case_match(
    x_axis,
    'neighborhood' ~ 'Neighborhood',
    'period' ~ 'Period',
    'hour' ~ 'Hour',
    'route' ~ 'Route',
    'stop' ~ 'Stop',
    .default = str_to_title(x_axis)
  )

  day_title <- ifelse(
    length(setdiff(c("Weekday", "Saturday", "Sunday"), unique(data$Day))) == 0,
    paste0('All Week'),
    paste0(unique(data$Day), collapse = ", ")
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
      x = reorder(axis, desc(value)),
      y = value,
      fill = reorder(Service, service_change_num)
    )
  )

  if (x_axis == 'period') {
    plt <- ggplot2::ggplot(
      plot_data,
      aes(x = axis, y = value, fill = reorder(Service, service_change_num))
    )
  }
  if (x_axis == 'hour') {
    plot_data <- plot_data %>%
      mutate(hour_label = as.character(axis))

    plt <- ggplot(
      plot_data,
      aes(
        x = reorder(hour_label, axis),
        y = value,
        fill = reorder(Service, service_change_num)
      )
    )
  }

  plt <- plt +
    geom_col(position = position_dodge()) +
    viridis::scale_fill_viridis(discrete = TRUE, name = 'Legend') +
    ggtitle(paste0(
      var_title,
      ' by ',
      axis_title,
      ', ',
      'Stop Ridership'
    )) +
    ggplot2::labs(subtitle = paste(day_title, period_title, sep = ", ")) +
    scale_x_discrete(
      labels = scales::label_wrap(10),
      guide = guide_axis(angle = 45)
    ) +
    style_kcm()
  plt
}
