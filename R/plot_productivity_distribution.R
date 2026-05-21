plot_productivity_distribution <- function(
  service_change,
  tbird_connection,
  route = NULL,
  activity_type = 'rides_per_platform_hour',
  binwidth = 2,
  point_size = 20,
  label_size = 3,
  style_size = NULL
) {
  trip_productivity <- get_route_productivity(
    service_change,
    tbird_connection,
    'day_part_cd',
    0,
    FALSE
  ) |>
    tidyr::pivot_longer(
      cols = c(rides_per_platform_hour:psngr_miles_per_platform_mile),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::filter(variable == activity_type & day_part_cd == 'DAY') |>
    tidyr::unite(
      route,
      service_rte_num:express_local_cd,
      sep = "",
      remove = FALSE
    ) |>
    dplyr::filter(ons > 0)

  binwidth_seq <- seq(0, 100, binwidth)

  data <- trip_productivity |>
    dplyr::mutate(
      group = factor(
        ifelse(
          service_rte_num %in% .env$route | is.null(.env$route),
          'Route',
          'System'
        ),
        levels = c('Route', 'System')
      ),
      binwidth = sapply(value, function(x) {
        binwidth_seq[which.min(ifelse(
          binwidth_seq - x < 0,
          NA,
          binwidth_seq - x
        ))]
      }),
      binwidth2 = sapply(value, function(x) {
        binwidth_seq[which.min(abs(binwidth_seq - x))]
      })
    )

  percentiles <- quantile(data$value, probs = c(0.25, 0.5, 0.75))

  selection <- data.frame(percentile = names(percentiles), value = percentiles)

  dotplot <- ggplot2::ggplot(data, ggplot2::aes(x = value, fill = group)) +
    ggplot2::geom_dotplot(
      method = 'histodot',
      binwidth = binwidth,
      binpositions = 'all',
      stackgroups = TRUE,
      color = NA
    )

  built <- ggplot2::ggplot_build(dotplot)
  point.pos <- built$data[[1]]

  data2 <- dplyr::arrange(data, binwidth2, group, value)

  data2$ytext <- point.pos$stackpos * (0.07)
  data2$xtext <- point.pos$x

  color_legend <- c(
    'Route' = '#0072BC',
    'System' = '#EFEFEF',
    "L" = "white",
    "E" = "#FDB71A"
  )

  plt <- ggplot2::ggplot(data2, ggplot2::aes(x = value, fill = group)) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = xtext,
        y = ytext,
        color = group
      ),
      size = point_size,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = xtext,
        y = ytext,
        label = ifelse(group != 'System', service_rte_num, ''),
        family = 'inter',
        fontface = 'bold',
        color = express_local_cd
      ),
      size = label_size
    ) +
    ggplot2::scale_x_continuous(limits = c(0, NA)) +
    ggplot2::scale_y_continuous(name = NULL, breaks = NULL) +
    ggplot2::geom_vline(
      data = selection,
      ggplot2::aes(xintercept = value),
      linetype = 'dashed'
    ) +
    ggplot2::ggtitle(paste0(
      stringr::str_to_title(stringr::str_replace_all(activity_type, "_", " ")),
      ' Distribution'
    )) +
    ggplot2::labs(
      y = "",
      x = "",
      subtitle = paste0(
        unique(data2$service),
        ' (',
        unique(data2$day),
        ')'
      )
    ) +
    ggplot2::scale_color_manual(
      values = color_legend,
      breaks = c('L', 'E'),
      labels = c('Local', 'Express')
    ) +
    ggplot2::annotate(
      "text",
      x = percentiles["25%"],
      y = Inf,
      label = "25th",
      vjust = 2,
      hjust = -0.25,
      size = label_size
    ) +
    ggplot2::annotate(
      "text",
      x = percentiles["50%"],
      y = Inf,
      label = "50th",
      vjust = 2,
      hjust = -0.25,
      size = label_size
    ) +
    ggplot2::annotate(
      "text",
      x = percentiles["75%"],
      y = Inf,
      label = "75th",
      vjust = 2,
      hjust = -0.25,
      size = label_size
    ) +
    ggplot2::labs(
      caption = paste0(
        'Plot showing ',
        length(unique(trip_productivity$route)),
        ' routes'
      )
    ) +
    style_kcm(textsize = style_size) +
    ggplot2::theme(legend.key = ggplot2::element_rect(fill = "#0072BC"))

  plt
}
