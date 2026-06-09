#' ggplot engine for route-level productivity distribution plots
#'
#' @description Generate a distribution dotplot of route productivity metrics from get_route_productivity(). Rides per Platform Hour or Passenger Miles per Platform Mile.
#'
#' @param service_change Numeric. The three-digit identifier of the service change.
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param svc_family Character. Service Family. Options are 'Urban', 'Suburban', and 'ST'.
#' @param route Numeric. The route identifiers of interest to highlight and label. Values to be treated as characters to allow for non-numeric route identifiers. Can accept multiple values as a vector. Defaults to show all routes. If any of the route_gain, route_maintain, or route_lose paramenters are populated, this parameter is ignored.
#' @param route_gain Numeric. Routes to color as gaining service. Overrides route parameter if used.
#' @param route_maintain Numeric. Routes to color as maintaining service. Overrides route parameter if used.
#' @param route_lose Numeric. Routes to color as losing service. Overrides route parameter if used.
#' @param activity_type Character. rides_per_platform_hour - Rides per Platform Hour, psngr_miles_per_platform_mile - Passenger Miles per Platform Mile
#' @param binwidth Numeric. Specifies bin width. Decrease if dots are too close to each other vertically. Increase if dots are too close to each other horizontally. Defaults to 2 for rides per platform hour and 1 for passenger miles per platform miles.
#' @param point_size Numeric. The diameter of the dots. Adjust so that dots do not overlap one another. Interacts with binwidth.
#' @param label_size Numeric. The font size of the dot labels. Adjust so that labels fit within each dot.
#' @param style_size Numeric. Specifies the sizing style to use with style_kcm(). Defaults to default size. "large" to increase sizing style for pdf/png/jpeg/markdown exports.
#'
#' @returns A ggplot2 plot showing distribution dotplot of route productivity metrics.
#'
#' @export
#' @examples
#' con <- connect_to_tbird()
#'
#' # Generate Rides per Platform Hour distribution dotplot for Fall 2025 Service Change
#' plot_productivity_distribution(service_change = 253, tbird_connection = con)
#'
#' # Highlight Routes based on Service Gain/Loss
#' plot_productivity_distribution(service_change = 253, tbird_connection = con, route_gain = c(8), route_maintain = c(1, 2), route_lose = c(107, 673))
#'
#' # Generate Passenger Miles per Platform Mile distribution dotplot for Fall 2025 Service Change highlighting RapidRide routes only.
#' # Sizing parameters formatted for png download size.
#' plot_productivity_distribution(service_change = 253,
#'  tbird_connection = con,
#'  route = c(671, 672, 673, 674, 675, 676, 677, 678),
#'  activity_type = 'psngr_miles_per_platform_mile',
#'  binwidth = 1,
#'  point_size = 6.5,
#'  label_size = 8,
#'  style_size = 'large')
#'
#' ggplot2::ggsave("RapidRide Passenger Miles per Platform Mile.png", width = 12.5, height = 6.9, units = "in")
plot_productivity_distribution <- function(
  service_change,
  tbird_connection,
  svc_family = NULL,
  route = NULL,
  route_gain = NULL,
  route_maintain = NULL,
  route_lose = NULL,
  activity_type = 'rides_per_platform_hour',
  binwidth = NULL,
  point_size = 20,
  label_size = 4,
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
    # Exclude school routes and routes that are missing ridership data (DART Routes)
    dplyr::filter(ons > 0 & svc_family != 'Other') |>
    dplyr::filter(ifelse(
      is.null(.env$svc_family),
      TRUE,
      svc_family == .env$svc_family
    ))

  productivity_thresholds <- get_productivity_thresholds(
    service_change,
    tbird_connection,
    'day_part_cd',
    0
  ) |>
    dplyr::filter(day_part_cd == 'DAY' & svc_family == .env$svc_family)

  # Set default binwidth values based on activity_type if not set
  if (is.null(binwidth)) {
    if (activity_type == 'rides_per_platform_hour') {
      binwidth <- 2
    } else if (activity_type == 'psngr_miles_per_platform_mile') {
      binwidth <- 1
    }
  }

  binwidth_seq <- seq(0, 100, binwidth)

  data <- trip_productivity |>
    dplyr::mutate(
      group = factor(
        ifelse(
          is.null(c(route_gain, route_maintain, route_lose, .env$route)),
          'Route',
          dplyr::case_when(
            service_rte_num %in% route_gain ~ 'Gains Service',
            service_rte_num %in% route_maintain ~ 'Maintains Service',
            service_rte_num %in% route_lose ~ 'Loses Service',
            service_rte_num %in%
              .env$route &
              is.null(c(route_gain, route_maintain, route_lose)) ~ 'Route',
            .default = 'System'
          )
        ),
        levels = c(
          'Route',
          'Gains Service',
          'Maintains Service',
          'Loses Service',
          'System'
        )
      ),
      selection_group = ifelse(group == 'System', 'System', 'Selection'),
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

  # Calculate percentiles based on whole network
  percentiles <- quantile(data$value, probs = c(0.25, 0.5, 0.75))

  selection <- data.frame(percentile = names(percentiles), value = percentiles)

  # Generate dotplot and save x-y coordinates
  # See https://stackoverflow.com/questions/44991607/how-do-i-label-the-dots-of-a-geom-dotplot-in-ggplot2
  dotplot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = value, fill = selection_group)
  ) +
    ggplot2::geom_dotplot(
      method = 'histodot',
      binwidth = binwidth,
      binpositions = 'all',
      stackgroups = TRUE,
      color = NA
    )

  built <- ggplot2::ggplot_build(dotplot)
  point.pos <- built$data[[1]]

  data2 <- dplyr::arrange(data, binwidth2, selection_group, value)

  data2$ytext <- point.pos$stackpos * (0.07)
  data2$xtext <- point.pos$x

  color_legend <- c(
    'Route' = '#0072BC',
    'System' = '#EFEFEF',
    'Gains Service' = '#006633',
    'Maintains Service' = '#0072BC',
    'Loses Service' = '#F57F29',
    "L" = "white",
    "E" = "#FDB71A",
    "Urban" = "#006848",
    "Suburban" = "#D67619",
    "Rural and DART" = "#4B2884",
    "DART/Shuttle" = "#4B2884",
    "ST" = "#264d5e"
  )

  # Plot points and labels using x-y coordinates from dotplot generated above
  plt <- ggplot2::ggplot(data2, ggplot2::aes(x = value)) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = xtext,
        y = ytext,
        color = ifelse(selection_group == 'Selection', svc_family, 'System')
      ),
      size = point_size
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
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, NA)) +
    ggplot2::scale_y_continuous(name = NULL, breaks = NULL, limits = c(0, NA)) +
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
        ') ',
        svc_family
      )
    ) +
    ggplot2::scale_color_manual(
      values = color_legend,
      breaks = c(
        'Urban',
        'Suburban',
        'Rural and DART',
        'Gains Service',
        'Maintains Service',
        'Loses Service',
        'L',
        'E'
      ),
      labels = c(
        'Urban',
        'Suburban',
        'Rural and DART',
        'Gains Service',
        'Maintains Service',
        'Loses Service',
        'Local',
        'Express'
      )
    ) +
    ggplot2::geom_vline(
      data = productivity_thresholds,
      ggplot2::aes(xintercept = bottom_25_threshold_rides, color = svc_family),
      linetype = 'dashed',
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(
      data = productivity_thresholds,
      ggplot2::aes(xintercept = median_threshold_rides, color = svc_family),
      linetype = 'dashed',
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(
      data = productivity_thresholds,
      ggplot2::aes(xintercept = top_25_threshold_rides, color = svc_family),
      linetype = 'dashed',
      show.legend = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x = productivity_thresholds$bottom_25_threshold_rides,
      y = Inf,
      label = "25th",
      vjust = 2,
      hjust = -0.25,
      size = label_size
    ) +
    ggplot2::annotate(
      "text",
      x = productivity_thresholds$median_threshold_rides,
      y = Inf,
      label = "50th",
      vjust = 2,
      hjust = -0.25,
      size = label_size
    ) +
    ggplot2::annotate(
      "text",
      x = productivity_thresholds$top_25_threshold_rides,
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
    ggplot2::theme(legend.key = ggplot2::element_rect(fill = "#EFEFEF"))

  plt
}
