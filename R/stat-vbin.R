#' @param binwidth The width of the bins. The default is 1.
#' @param bins Number of bins. Overridden by `binwidth`.
#' @param center If `TRUE`, the bins will be centered on their midpoint.
#'   This is useful for visualizing Likert-scale data.
#' @param scale A numeric value to scale the (horizontal) width of the bars.

#' @export
#' @rdname geom_vhistogram
stat_vbin <- function(mapping = NULL, data = NULL, geom = "vhistogram",
                      position = "identity", bins = NULL, binwidth = NULL,
                      center = FALSE, scale = 1,
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                      ...) {
  ggplot2::layer(
    mapping = mapping,
    data = data,
    stat = StatVbin,
    geom = GeomVhistogram,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      center = center,
      scale = scale,
      na.rm = na.rm,
      ...
    )
  )
}

#' @format NULL
#' @usage NULL
#' @export
StatVbin <- ggplot2::ggproto(
  "StatVbin",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data,
                           scales,
                           params,
                           bins = NULL,
                           binwidth = NULL,
                           center = FALSE,
                           scale = 1) {
    coords <- vbins(
      data$y,
      bins = bins,
      binwidth = binwidth,
      center = center,
      scale = scale
    )

    # Set a constant value to y because y is a required aes, even though we
    # don't need it anymore
    coords$y <- 1

    coords$x <- data$x[1]

    coords$xmin <- coords$xmin + data$x[1]
    coords$xmax <- coords$xmax + data$x[1]

    coords
  }
)

vbins <- function(x,
                  bins = NULL,
                  binwidth = NULL,
                  center = FALSE,
                  scale = 1) {
  if (!is.null(bins) && !is.null(binwidth)) {
    warning("Provided both bins and binwidth; using binwidth.")
  }

  if (is.null(bins) && is.null(binwidth)) {
    # Sturges method as the default
    breaks <- pretty(range(x), n = nclass.Sturges(x), min.n = 1)
  }

  if (!is.null(binwidth)) {
    bins <- round(max(x) / binwidth) - round(min(x) / binwidth) + 1
  }

  if (!is.null(bins)) {
    if (!is.null(center)) {
      if (!center) bins <- bins + 1
    }

    breaks <- seq(from = min(x), to = max(x), length.out = bins)

    if (is.null(binwidth)) binwidth <- diff(breaks[1:2])
  }

  if (center) {
    breaks <- breaks - binwidth / 2
    breaks <- c(breaks, max(breaks) + binwidth)
  }

  hist <- hist(x, plot = FALSE, breaks = breaks)

  xmin <- 0
  xmax <- hist$counts / max(hist$counts) * (scale / 2)
  ymin <- hist$breaks[-length(hist$breaks)]
  ymax <- hist$breaks[-1]

  tibble::tibble(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  )
}
