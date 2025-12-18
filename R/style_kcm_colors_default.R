#' Use Default KCM Color Scheme for Plots
#'
#' @returns
#'
#' @export
#' @examples
style_kcm_colors_default <- function() {
  kcm_colors_list <- c(
    "#FDB71A",
    "#FF7B21",
    "#31859F",
    "#006633",
    "#390854",
    "#FFE089"
  )
  list(
    ggplot2::scale_color_manual(values = kcm_colors_list),
    ggplot2::scale_fill_manual(values = kcm_colors_list)
  )
}
