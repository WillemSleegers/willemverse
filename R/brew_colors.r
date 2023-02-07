#' Brew colors
#'
#' Generate colors from a palette.
#'
#' @param palette A vector of color values. By default it will use the Hiroshige
#'   palette from the MetBrewer package.
#' @param n The number of colors to generate from the palette.
#' @param discrete Whether to generate a discrete palette. The default is FALSE,
#'   which will generate a continuous palette.
#' @param direction Either 1 or -1. If -1 the palette will be reversed before
#'   generating the colors.
#' @param `begin, end` The interval within the palette to sample colours from.
#'   Defaults to 0 and 1 respectively.
#'
#' @export
brew_colors <- function(
    n = 5,
    discrete = FALSE,
    direction = 1,
    begin = 0, end = 1,
    palette = NULL) {
  if (is.null(palette)) {
    requireNamespace("MetBrewer", quietly = TRUE)
    palette <- MetBrewer::met.brewer("Hiroshige", n = 10)
  }

  if (direction == -1) {
    palette <- rev(palette)
  }

  if (discrete) {
    x <- round(seq(1, length(palette), length.out = n))

    x1 <- x[1:ceiling(length(x) / 2)]
    x2 <- x[(ceiling(length(x) / 2) + 1):length(x)]
    x2 <- rev(x2)

    x <- c(x1, x2)[order(c(seq_along(x1), seq_along(x2)))]

    palette[x]
  } else {
    color_fn <- grDevices::colorRamp(palette)
    rgbs <- color_fn(seq(begin, end, length.out = n))
    rgb(rgbs[, 1], rgbs[, 2], rgbs[, 3], maxColorValue = 255)
  }
}
