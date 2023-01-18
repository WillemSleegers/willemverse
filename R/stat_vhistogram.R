#' Plot vertical histograms per group
#'
#' This stat plots the distribution of a variable for each group. It is
#' particularly suitable for visualizing distributions of categorical data such
#' as Likert-scale data.
#'
#' @param x A character indicating whom to say hello to. 'World' by default.
#'
#' @export
stat_vhistogram <- function(mapping = NULL, data = NULL, geom = "polygon",
                            position = "identity", bins = NULL, binwidth = NULL,
                            center = FALSE,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                            ...) {
  ggplot2::layer(
    stat = StatVHistogram,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins, binwidth = binwidth, center = center, na.rm = na.rm, ...
    )
  )
}

StatVHistogram <- ggproto(
  "StatVHistogram",
  Stat,
  compute_group = function(data,
                           scales,
                           params,
                           bins = NULL, binwidth = NULL, center = FALSE) {
    coords <- vhist_coords(
      data$y,
      bins = bins,
      binwidth = binwidth,
      center = center
    )
    coords$x <- coords$x + data$x[1]

    coords
  },
  required_aes = c("x", "y")
)

vhist_coords <- function(x,
                         bins = NULL,
                         binwidth = NULL,
                         center = FALSE) {
  # Determine bins and binwidth
  if (!is.null(bins) && !is.null(binwidth)) {
    warning("Provided both bins and binwidth; using binwidth.")
  }

  if (is.null(bins) && is.null(binwidth)) {
    # Sturges method as the default
    breaks <- pretty(range(x), n = nclass.Sturges(x), min.n = 1)
  }

  if (!is.null(binwidth)) {
    bins <- round(max(x) / binwidth)
  }

  if (!is.null(bins)) {
    # Number of bins depends on whether the bars are centered or not
    if (!is.null(center)) {
      if (!center) bins <- bins + 1
    }

    breaks <- seq(from = min(x), to = max(x), length.out = bins)
  }

  binwidth <- diff(breaks[1:2])

  hist <- hist(x, plot = FALSE, breaks = breaks)

  x <- c()
  y <- c()

  if (center) {
    for (i in seq_along(hist$mids)) {
      x <- c(x, 0, hist$density[i], hist$density[i], 0)
      y <- c(
        y,
        hist$mids[i] - binwidth,
        hist$mids[i] - binwidth,
        hist$mids[i],
        hist$mids[i]
      )
    }

    # Add last bar
    x <- c(x, 0, hist$density[i], hist$density[i], 0)
    y <- c(
      y,
      hist$mids[i],
      hist$mids[i],
      hist$mids[i] + binwidth,
      hist$mids[i] + binwidth
    )

    # Connect last point to first point
    x <- c(x, 0)
    y <- c(y, hist$mids[1] - binwidth)
  } else {
    for (i in seq_along(hist$breaks)) {
      if (i == 1 || i == length(hist$breaks)) {
        y <- c(y, hist$breaks[i], hist$breaks[i])

        if (i == 1) {
          x <- c(x, 0, hist$density[i])
        } else {
          x <- c(x, hist$density[i - 1], 0)

          # Connect last point to first point
          x <- c(x, 0)
          y <- c(y, hist$breaks[1])
        }
      } else {
        x <- c(x, hist$density[i - 1], 0, hist$density[i])
        y <- c(y, hist$breaks[i], hist$breaks[i], hist$breaks[i])
      }
    }
  }

  tibble::tibble(
    x = x,
    y = y
  )
}
