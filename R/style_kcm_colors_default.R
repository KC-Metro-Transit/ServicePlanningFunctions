#' Use Default KCM Color Scheme for Plots
#'
#' @returns utility list of default Metro colors
#'
#' @export
#' @examples
#' #ggplot(mtcars, aes(x = wt, y = factor(cyl), color = factor(cyl))) +
#' #geom_boxplot() +
#' #facet_grid(. ~ gear) +
#' #style_kcm() +
#' #style_kcm_colors()
style_kcm_colors_default <- function(color_palette_direction = 0) {
  kcm_colors_list_0 <- c(
    "#FDB71A",
    "#FF7B21",
    "#31859F",
    "#006633",
    "#390854",
    "#FFE089"
  )

  kcm_colors_list_1 <- c(
    "#FF7B21",
    "#31859F",
    "#006633",
    "#390854",
    "#FFE089",
    "#FDB71A"
  )

  kcm_colors_list_2 <- c(
    "#31859F",
    "#006633",
    "#390854",
    "#FFE089",
    "#FDB71A",
    "#FF7B21"
  )

  kcm_colors_list_3 <- c(
    "#006633",
    "#390854",
    "#FFE089",
    "#FDB71A",
    "#FF7B21",
    "#31859F"
  )

  kcm_colors_list_4 <- c(
    "#390854",
    "#FFE089",
    "#FDB71A",
    "#FF7B21",
    "#31859F",
    "#006633"
  )

  if (color_palette_direction == 0) {
    list(
      ggplot2::scale_color_manual(values = kcm_colors_list_0),
      ggplot2::scale_fill_manual(values = kcm_colors_list_0)
    )
  } else if (color_palette_direction == 1) {
    list(
      ggplot2::scale_color_manual(values = kcm_colors_list_1),
      ggplot2::scale_fill_manual(values = kcm_colors_list_1)
    )
  } else if (color_palette_direction == 2) {
    list(
      ggplot2::scale_color_manual(values = kcm_colors_list_2),
      ggplot2::scale_fill_manual(values = kcm_colors_list_2)
    )
  } else if (color_palette_direction == 3) {
    list(
      ggplot2::scale_color_manual(values = kcm_colors_list_3),
      ggplot2::scale_fill_manual(values = kcm_colors_list_3)
    )
  } else if (color_palette_direction == 4) {
    list(
      ggplot2::scale_color_manual(values = kcm_colors_list_4),
      ggplot2::scale_fill_manual(values = kcm_colors_list_4)
    )
  } else {
    list(
      ggplot2::scale_color_manual(values = kcm_colors_list_0),
      ggplot2::scale_fill_manual(values = kcm_colors_list_0)
    )
  }
}
