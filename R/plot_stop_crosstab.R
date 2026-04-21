#' ggplot engine for stop level ridership plots
#' @description Generate plot of ons, offs, and load by Select Variable and Service Change from get_stop_ridership()
#'
#' @param dataframe Dataframe. Output from get_stop_ridership().
#' @param service_change_num Numeric. The three-digit identifier of the service change. Can accept multiple values as a vector.
#' @param route Numeric. The route identifiers of interest. Values to be treated as characters to allow for non-numeric route identifiers. Can accept multiple values as a vector.
#' @param stop_id A vector of stop IDs or "All" if all stops on a route are of interest. Defaults to "All"
#' @param time_period Character. AM, PM, MID, XEV. XNT.
#' @param x_axis Character. Grouping variable based on columns found in output from get_stop_ridership(). period, hour, route, route_name.
#' @param activity_type Character. ons - Average Daily Boarding, offs - Average Daily Alightings, rider - Total Stop Activity
#'
#' @returns A ggplot2 plot of ons, offs, and load by Select Variable and Service Change from get_stop_ridership()
#'
#' @export
plot_stop_crosstab <- function(
  dataframe,
  service_change_num,
  route,
  stop_id = "All",
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  x_axis,
  activity_type = 'ons'
) {
  if ("All" %in% stop_id) {
    data <- dataframe %>%
      dplyr::filter(
        day_part_cd %in% .env$time_period,
        service_change_num %in% .env$service_change_num,
        route %in% .env$route
      ) %>%
      dplyr::rename(period = time_period_at_stop)
  } else {
    data <- dataframe %>%
      dplyr::filter(
        day_part_cd %in% .env$time_period,
        service_change_num %in% .env$service_change_num,
        route %in% .env$route,
        stop_id %in% .env$stop_id
      ) %>%
      dplyr::rename(period = time_period_at_stop)
  }
  plot_data <- data %>%
    dplyr::group_by_at(dplyr::vars(service_change_num, service, x_axis)) %>%
    dplyr::select(service_change_num, service, 'axis' = x_axis, ons, offs) %>%
    dplyr::summarise(
      dplyr::across(ons:offs, ~ sum(.x, na.rm = TRUE)),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(rider = ons + offs) %>%
    tidyr::pivot_longer(
      cols = ons:rider,
      names_to = 'variable',
      values_to = 'value'
    ) %>%
    dplyr::filter(
      variable %in% activity_type,
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        variable == 'ons' ~ 'Average Daily Stop Boardings',
        variable == 'offs' ~ 'Average Daily Stop Alightings',
        variable == 'rider' ~ 'Average Daily Stop Activity',
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
    'route_name' ~ 'Route',
    'stop' ~ 'Stop',
    .default = stringr::str_to_title(x_axis)
  )

  day_title <- ifelse(
    length(setdiff(c("Weekday", "Saturday", "Sunday"), unique(data$day))) == 0,
    paste0('All Week'),
    paste0(unique(data$day), collapse = ", ")
  )

  stop_title <- ifelse(
    "All" %in% stop_id,
    paste0('all stops on route(s)'),
    paste0(
      sort(unique(
        data$stop_id
      )),
      collapse = ", "
    )
  )

  route_title <- paste0(unique(data$route), collapse = ", ")

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
    ggplot2::aes(
      x = stats::reorder(axis, dplyr::desc(value)),
      y = value,
      fill = stats::reorder(service, service_change_num)
    )
  )

  if (x_axis == 'period') {
    plt <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
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
      ggplot2::aes(
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
      'Stop Ridership'
    )) +
    ggplot2::labs(
      subtitle = paste(day_title, period_title, sep = ", "),
      caption = stringr::str_wrap(
        paste(
          "Plot shows data for stops on route(s)",
          route_title,
          "at stops",
          stop_title,
          width = 50,
          sep = " "
        ),
        width = 100
      )
    ) +
    ggplot2::scale_x_discrete(
      labels = scales::label_wrap(10),
      guide = ggplot2::guide_axis(angle = 45)
    ) +
    ServicePlanningFunctions::style_kcm()
  plt
}
