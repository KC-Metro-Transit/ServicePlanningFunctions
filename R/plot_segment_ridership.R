#' ggplot engine for stop level ridership plots
#' Generate plot of ons, offs, and load by Select Variable and Service Change from get_stop_ridership()
#'
#' @param dataframe Dataframe. Output from get_stop_ridership().
#' @param service_change_num Numeric. The three-digit identifier of the service change. Can accept multiple values as a vector.
#' @param route Numeric. The route identifiers of interest. Values to be treated as characters to allow for non-numeric route identifiers. Can accept multiple values as a vector.
#' @param time_period Character. AM, PM, MID, XEV. XNT.
#' @param direction Character. "I" or "O". Can accept both.
#' @param x_axis Character. Grouping variable based on columns found in output from get_stop_ridership(). period, hour, route, route_name.
#' @param activity_type Character. ons - Average Daily Boarding, offs - Average Daily Alightings, rider - Total Stop Activity
#' @param start_stop Numeric. Stop at the beginning of the segment of interest. Will be included in results.
#' @param end_stop Numeric. Stop at the end of the segment of interest. Will be included in results.
#' @returns A ggplot2 plot of ons, offs, and load by Select Variable and Service Change from get_stop_ridership()
#'
#' @export
plot_segment_ridership <- function(
  dataframe,
  service_change_num,
  route,
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  direction = c("I", "O"),
  x_axis = "rider",
  activity_type = 'ons',
  start_stop,
  end_stop
) {
  data <- dataframe %>%
    dplyr::filter(
      day_part_cd %in% .env$time_period,
      service_change_num %in% .env$service_change_num,
      route %in% .env$route,
      direction %in% .env$direction
    ) %>%
    dplyr::rename(period = time_period_at_stop) |>
    dplyr::mutate(route = paste0(route, express_local_cd))

  start_stop_id_df <- dplyr::filter(data, stop_id %in% start_stop) |>
    dplyr::distinct(
      service_change_num,
      stop_id,
      route,
      express_local_cd,
      direction,
      stop_sequence_num
    ) |>
    dplyr::rename(start_stop_seq = stop_sequence_num) |>
    dplyr::select(-stop_id)

  if (
    length(unique(start_stop_id_df$start_stop_seq)) > 1 &
      length(unique(start_stop_id_df$route)) == 1
  ) {
    cli::cli_abort(
      message = "Value provided for start_stop is used multiple times in route pattern."
    )
  }

  end_stop_id_df <- dplyr::filter(data, stop_id %in% end_stop) |>
    dplyr::distinct(
      service_change_num,
      stop_id,
      route,
      express_local_cd,
      direction,
      stop_sequence_num
    ) |>
    dplyr::rename(end_stop_seq = stop_sequence_num) |>
    dplyr::select(-stop_id)

  if (
    length(unique(end_stop_id_df$end_stop_seq)) > 1 &
      length(unique(end_stop_id_df$route)) == 1
  ) {
    cli::cli_abort(
      message = "Value provided for end_stop is used multiple times in route pattern."
    )
  }

  filtered_data <- data %>%
    dplyr::left_join(
      start_stop_id_df,
      by = c("service_change_num", "route", "express_local_cd", "direction")
    ) |>
    dplyr::left_join(
      end_stop_id_df,
      c("service_change_num", "route", "express_local_cd", "direction")
    ) |>
    dplyr::mutate(
      include_stop = dplyr::case_when(
        stop_sequence_num >= start_stop_seq &
          stop_sequence_num <= end_stop_seq ~ 1,
        TRUE ~ 0
      )
    ) |>
    dplyr::filter(
      include_stop == 1
    ) |>
    dplyr::arrange(stop_id)

  plot_data <- filtered_data |>
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

  stop_title <- stringr::str_wrap(
    paste0(unique(filtered_data$stop_id), collapse = ", "),
    width = 50
  )

  route_title <- paste0(sort(unique(data$route)), collapse = ", ")

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
      'Segment Ridership'
    )) +
    ggplot2::labs(
      subtitle = paste(
        day_title,
        period_title,
        sep = ", "
      ),
      caption = stringr::str_wrap(
        paste(
          "Plot shows data for stops on route(s)",
          route_title,
          "at stops",
          stringr::str_wrap(stop_title, width = 50),
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
