#' Title
#'
#' @param textsize Specifies the font size to use. Defaults to default size. "large" to increase font size for pdf/png/jpeg/markdown exports.
#'
#' @returns
#'
#' @export
#' @examples
#' ggplot(mtcars, aes(x = factor(cyl), y = wt, color = factor(cyl))) +
#' geom_boxplot() +
#' facet_grid(. ~ gear) +
#' style_kcm_flip()
style_kcm_flip <- function(textsize = NULL) {
  # Download inter font if not installed already
  if (!"inter" %in% sysfonts::font_families()) {
    sysfonts::font_add_google("inter", "inter")
  }
  showtext::showtext_auto()

  font <- 'inter'

  # Declare font size based on textsize parameter
  if (is.null(textsize)) {
    font_size <- 16
    title_size <- 20
    subtitle_size <- 18
    legend_size <- 15
  } else if (textsize == 'large') {
    font_size <- 28
    title_size <- 36
    subtitle_size <- 32
    legend_size <- 27
  }

  # Generate Theme
  list(ggplot2::theme(
    # Text, title, subtitle and caption fonts
    text = element_text(size = font_size, family = font),
    plot.title = ggplot2::element_text(
      size = title_size,
      family = font,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      size = subtitle_size,
      family = font,
      color = "#242424"
    ),
    plot.caption = ggplot2::element_text(
      size = font_size,
      hjust = 0.02,
      vjust = 2,
      family = font,
      color = "#585860"
    ),

    # Title and caption position
    plot.title.position = "plot",
    plot.caption.position = "plot",

    # Legend
    legend.position = 'top',
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.justification = 'left',
    legend.margin = margin(t = 0, b = 0, l = 0, r = 0),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font, size = legend_size),

    # Axes
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(
      size = font_size,
      family = font,
      color = "black"
    ),
    axis.ticks = ggplot2::element_blank(),
    axis.line = element_line(
      linewidth = 0.6,
      linetype = 'solid',
      color = "black"
    ),

    # Facet Lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(
      color = '#585860',
      linewidth = 0.35,
      linetype = 2
    ),
    panel.grid.major.y = ggplot2::element_blank(),

    panel.background = ggplot2::element_rect(fill = "white"),
    panel.border = element_blank(),

    panel.spacing.y = unit(1.5, "line"),

    # Other
    strip.background = ggplot2::element_rect(fill = 'white'),
    strip.text = ggplot2::element_text(
      size = title_size,
      family = font,
      hjust = 0
    ),
  ))
}
