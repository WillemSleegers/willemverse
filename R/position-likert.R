#' Position Likert-type data
#'
#' `position_likert()` positions bars to improve the legibility of Likert-type
#' data. This means it will stack bars on top of each other but center them at
#' the mid-point. The positioning works for both bar geoms and text labels.
#' @export
position_likert <- function(nudge = 0) {
  ggplot2::ggproto(NULL, PositionLikert, nudge = nudge)
}

#' @format NULL
#' @usage NULL
#' @export
PositionLikert <- ggplot2::ggproto("PositionLikert", ggplot2::Position,
  nudge = 0,
  required_aes = c("x", "y", "fill"),
  setup_params = function(self, data) {
    list(
      nudge = self$nudge
    )
  },
  setup_data = function(self, data, params) {
    data$nudge <- self$nudge

    return(data)
  },
  compute_panel = function(data, params, scales) {
    levels <- levels(data$fill)
    len <- length(levels)
    even <- len %% 2 == 0
    mid <- ceiling(len / 2)

    y_min <- c()
    y_max <- c()
    y_label <- c()

    for (x in unique(data$x)) {
      y <- data$y[data$x == x]
      fill <- data$fill[data$x == x]

      y_bottom <- y[as.numeric(fill) <= mid]
      y_top <- y[as.numeric(fill) > mid]

      y_min_bottom <- rev(cumsum(rev(y_bottom))) * -1
      y_min_top <- cumsum(c(0, y_top[-length(y_top)]))

      if (!even & mid %in% as.numeric(fill)) {
        y_mid <- y[as.numeric(fill) == mid]
        y_min_bottom <- y_min_bottom + y_mid / 2
        y_min_top <- y_min_top + y_mid / 2
      }

      y_min_new <- c(y_min_bottom, y_min_top)
      y_max_new <- c(y_min_bottom, y_min_top) + y

      y_min <- c(y_min, y_min_new)
      y_max <- c(y_max, y_max_new)

      if ("label" %in% names(data)) {
        y_label_new <- y_min_new + diff(c(y_min_new[1], y_max_new)) / 2
        y_label <- c(y_label, y_label_new)
      }
    }

    data$ymin <- y_min
    data$ymax <- y_max

    if ("label" %in% names(data)) {
      data$y <- y_label
    }

    data$x <- data$x + data$nudge

    return(data)
  }
)
