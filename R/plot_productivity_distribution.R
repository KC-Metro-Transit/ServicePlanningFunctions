plot_productivity_distribution <- function(
  service_change,
  tbird_connection,
  route = NULL,
  activity_type = 'rides_per_platform_hour',
  binwidth = 1.5,
  label_just = 0
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

  data2$ytext <- point.pos$stackpos * (0.065 + 0.005 * label_just)
  data2$xtext <- point.pos$x

  color_legend <- c('Route' = '#0072BC', 'System' = '#EFEFEF')

  plt <- ggplot2::ggplot(data2, ggplot2::aes(x = value, fill = group)) +
    ggplot2::geom_dotplot(
      method = 'histodot',
      binwidth = binwidth,
      binpositions = 'all',
      stackgroups = TRUE,
      color = NA
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        xtext,
        ytext,
        label = ifelse(group != 'System', service_rte_num, ''),
        size = 3,
        family = 'inter',
        fontface = 'bold',
        color = express_local_cd
      ),
      show.legend = FALSE
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
    ggplot2::scale_fill_manual(values = color_legend) +
    ggplot2::scale_color_manual(values = c("L" = "white", "E" = "#FDB71A")) +
    ggplot2::annotate(
      "text",
      x = percentiles["25%"],
      y = Inf,
      label = "25th",
      vjust = 2,
      hjust = -0.25,
      size = 5
    ) +
    ggplot2::annotate(
      "text",
      x = percentiles["50%"],
      y = Inf,
      label = "50th",
      vjust = 2,
      hjust = -0.25,
      size = 5
    ) +
    ggplot2::annotate(
      "text",
      x = percentiles["75%"],
      y = Inf,
      label = "75th",
      vjust = 2,
      hjust = -0.25,
      size = 5
    ) +
    style_kcm(textsize = 'large')

  plt
}
