#' Generate plot of ons, offs, and load
#'
#' @description Create plots of trip and route ridership by Select Variable and Service Change from get_trip_ridership()
#'
#' @param dataframe Dataframe. Output from get_trip_ridership().
#' @param service_change_num Numeric. The three-digit identifier of the service change. Can accept multiple values as a vector.
#' @param route Numeric. The route identifiers of interest. Values to be treated as characters to allow for non-numeric route identifiers. Can accept multiple values as a vector.
#' @param day Character. Day of the week. Weekday, Saturday, Sunday.
#' @param time_period Character. AM, PM, MID, XEV. XNT.
#' @param x_axis Character. Grouping variable based on columns found in output from get_trip_ridership(). day, period, hour, route, route_name.
#' @param activity_type Character. ons - Average Daily Boarding, offs - Average Daily Alightings, avg_lod - Average Max Load,
#' weekly_trips - Total of  Weekly Trips on selected routes, day_type_trips - Count of Trips by day type on selected routes
#' @param split_by Character. Variable to split plots by based on columns found in output from get_trip_ridership. day, period. Defaults to no splitting.
#' @param color_palette Character.A character string indicating the color map option to use. Nine options are available: "kcm", "magma", "inferno", "plasma",
#' "viridis", "cividis", "rocket" , "mako"  or "turbo" .
#' @param color_palette_direction Numeric. 	Sets the order of colors in the scale. If 1, the default,
#' colors are as output by viridis_pal. If -1, the order of colors is reversed. If the color_palette is "kcm", options are 0 - 4 and will control the order
#' of colors assigned to groups.
#' @returns ggplot2 plot of ons, offs, and load by Select Variable and Service Change from get_trip_ridership()
#'
#' @export

plot_trip_crosstab <- function(
  dataframe,
  service_change_num,
  route,
  day = c("Weekday", "Saturday", "Sunday"),
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  x_axis = 'period',
  activity_type = 'ons',
  split_by = NULL,
  color_palette = "viridis",
  color_palette_direction = 1
) {
  if (x_axis == split_by) {
    cli::cli_abort(c(
      "X" = "x_axis and split_by variable is the same: {x_axis}. Please choose different variables for each."
    ))
  } else if (split_by %in% c('service_change_num', 'service')) {
    cli::cli_abort(c(
      "X" = "Cannot split by {split_by}."
    ))
  }

  data <- dataframe %>%
    dplyr::filter(
      as.character(day) %in% .env$day,
      day_part_cd %in% time_period,
      service_change_num %in% .env$service_change_num,
      route %in% .env$route
    )
  if (is.null(split_by)) {
    plot_data <- data %>%
      dplyr::group_by_at(dplyr::vars(service_change_num, service, x_axis)) %>%
      dplyr::select(
        service_change_num,
        service,
        'axis' = x_axis,
        ons:avg_load,
        weekly_trips,
        day_type_trips
      ) %>%
      dplyr::summarise(
        dplyr::across(ons:offs, ~ sum(.x, na.rm = TRUE)),
        dplyr::across(avg_load, ~ mean(.x, na.rm = TRUE)),
        dplyr::across(weekly_trips:day_type_trips, ~ sum(.x, na.rm = TRUE)),
        .groups = 'drop'
      ) %>%
      tidyr::pivot_longer(
        cols = ons:day_type_trips,
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
          'avg_load' ~ 'Average Max Load',
          'weekly_trips' ~ 'Total Weekly Trips',
          'day_type_trips' ~ 'Total Trips Per Day',
          .default = variable
        )
      )
  } else {
    plot_data <- data %>%
      dplyr::group_by_at(dplyr::vars(
        service_change_num,
        service,
        x_axis,
        split_by
      )) %>%
      dplyr::select(
        service_change_num,
        service,
        'axis' = x_axis,
        'facet' = split_by,
        ons:avg_load,
        weekly_trips,
        day_type_trips
      ) %>%
      dplyr::summarise(
        dplyr::across(ons:offs, ~ sum(.x, na.rm = TRUE)),
        dplyr::across(avg_load, ~ mean(.x, na.rm = TRUE)),
        dplyr::across(weekly_trips:day_type_trips, ~ sum(.x, na.rm = TRUE)),
        .groups = 'drop'
      ) %>%
      tidyr::pivot_longer(
        cols = ons:day_type_trips,
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
          'avg_load' ~ 'Average Max Load',
          'weekly_trips' ~ 'Total Weekly Trips',
          'day_type_trips' ~ 'Total Trips Per Day',
          .default = variable
        )
      )
  }

  route_title <- paste0(sort(unique(data$route)), collapse = ", ")

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
  if (is.null(split_by)) {
    plt <- plt +
      ggplot2::geom_col(position = ggplot2::position_dodge()) +
      ggplot2::ggtitle(paste0(
        var_title,
        ' by ',
        axis_title,
        ', ',
        'Trip Ridership'
      )) +
      ggplot2::labs(
        subtitle = paste(day_title, period_title, sep = ", "),
        caption = stringr::str_wrap(
          paste(
            "Plot shows data for trips on route(s)",
            route_title,
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
  } else {
    plt <- plt +
      ggplot2::geom_col(position = ggplot2::position_dodge()) +
      ggplot2::facet_wrap(ggplot2::vars(facet)) +
      ggplot2::ggtitle(paste0(
        var_title,
        ' by ',
        axis_title,
        ', ',
        'Trip Ridership'
      )) +
      ggplot2::labs(
        subtitle = paste(day_title, period_title, sep = ", "),
        caption = stringr::str_wrap(
          paste(
            "Plot shows data for trips on route(s)",
            route_title,
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
  }

  if (color_palette == "kcm") {
    plt <- plt +
      ServicePlanningFunctions::style_kcm_colors_default(
        color_palette_direction = color_palette_direction
      ) +
      ggplot2::scale_y_continuous(labels = scales::label_comma())
    plt
  } else {
    plt <- plt +
      viridis::scale_fill_viridis(
        discrete = TRUE,
        name = 'Legend',
        option = color_palette,
        direction = color_palette_direction
      ) +
      ggplot2::scale_y_continuous(labels = scales::label_comma())
    plt
  }
}
