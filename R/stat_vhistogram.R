#' Plot vertical histograms per group
#'
#' This stat plots the distribution of a variable for each group. It is
#' particularly suitable for visualizing distributions of categorical data such
#' as Likert-scale data.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. If specified and
#'   `inherit.aes = TRUE` (the default), it is combined with the default mapping
#'   at the top level of the plot. You must supply `mapping` if there is no plot
#'   mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data. A `function` can be created
#'    from a `formula` (e.g. `~ head(.x, 10)`).
#' @param geom The geometric object to use to display the data, either as a
#'   `ggproto` `Geom` subclass or as a string naming the geom stripped of the
#'   `geom_` prefix (e.g. `"point"` rather than `"geom_point"`)
#' @param position Position adjustment, either as a string naming the adjustment
#'   (e.g. `"jitter"` to use `position_jitter`), or the result of a call to a
#'   position adjustment function. Use the latter if you need to change the
#'   settings of the adjustment.
#' @param bins Number of bins.
#' @param binwidth The width of the bins.
#' @param center Whether the bins should be centered on their category.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE``, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#'
#' @seealso `hist()`
#'
#' @export
stat_vhistogram <- function(mapping = NULL, data = NULL, geom = "polygon",
                            position = "identity", bins = NULL, binwidth = NULL,
                            center = FALSE,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                            ...) {
  ggplot2::layer(
    stat = StatVhistogram,
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

StatVhistogram <- ggplot2::ggproto(
  "StatVHistogram",
  ggplot2::Stat,
  required_aes = c("x", "y"),
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
  }
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

vhist_coords2 <- function(x,
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
