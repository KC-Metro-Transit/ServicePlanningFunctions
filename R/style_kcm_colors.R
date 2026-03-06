#' Use Designated KCM Color Scheme for Plots
#'
#' @returns utility list of categories and default colors
#'
#' @export
#' @examples
#' #ggplot(mtcars, aes(x = wt, y = factor(cyl), color = factor(cyl))) +
#' #geom_boxplot() +
#' #facet_grid(. ~ gear) +
#' #style_kcm() +
#' #style_kcm_colors()
style_kcm_colors <- function() {
  kcm_custom_colors <- c(
    # Day Type
    "Weekday" = "#FDB71A",
    "Saturday" = "#31859F",
    "Sunday" = "#006633",

    # Productivity Period
    "Peak" = "#FDB71A",
    "Off-Peak" = "#006633",
    "Night" = "#390854",

    # Day Period
    "AM" = "#FDB71A",
    "MID" = "#F57F29",
    "PM" = "#31859F",
    "XEV" = "#006633",
    "XNT" = "#390854",

    # Before/Post
    "Before" = "#31859F",
    "Post" = "#F57F29",

    # Inbound/Outbound
    "Inbound" = "#FDB71A",
    "Outbound" = "#31859F",
    "I" = "#FDB71A",
    "O" = "#31859F",

    # Service Family
    "Urban" = "#006848",
    "Suburban" = "#D67619",
    "Rural and DART" = "#4B2884",
    "DART/Shuttle" = "#4B2884",
    "ST" = "#264d5e",
    "Other" = "#784885",
    "System" = "#FDB71A"
  )

  list(
    ggplot2::scale_color_manual(values = kcm_custom_colors),
    ggplot2::scale_fill_manual(values = kcm_custom_colors)
  )
}
