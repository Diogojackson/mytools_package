#' A theme for ggplot2 with a minimalist design
#'
#' @description
#' The `theme_ds` function is an extension of `theme_bw` from ggplot2
#' that provides a minimalist design with no major grid lines, minor grid lines,
#' panel background, and plot background. It is useful for creating simple and clean visualizations.
#'
#' @usage
#' theme_ds(base_size = 11, base_family = "")
#'
#' @param base_size The base font size for text elements in the plot. Default is 11.
#' @param base_family The base font family for text elements in the plot. Default is "".
#'
#' @return A ggplot2 theme object with the minimalist design.
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage
#' ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   theme_ds()
#'
#' # Setting font size and font family
#' ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   theme_ds(base_size = 14, base_family = "Arial")
#'
#' @export
theme_ds <- function(base_size = 11, base_family = ""){

  library(ggplot2)
  theme_bw(base_size = base_size, base_family = base_family) +

    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank())

}

