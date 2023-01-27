#' Position Likert-type data
#'
#' `position_likert()` positions bars to improve the legibility of Likert-type
#' data. This means it will stack bars on top of each other but center them at
#' the mid-point. The positioning works for both bar geoms and text labels.
#'
#' @export
position_likert <- function() {
  ggplot2::ggproto(NULL, PositionLikert)
}

PositionLikert <- ggplot2::ggproto("PositionLikert", ggplot2::Position,
  required_aes = c("x", "y", "fill"),
  setup_params = function(self, data) {
    data
  },
  setup_data = function(self, data, params) {
    data
  },
  compute_panel = function(data, params, scales) {
    levels <- sort(unique(data$fill), na.last = TRUE)
    len <- length(levels)
    even <- len %% 2 == 0
    mid <- ceiling(len / 2)

    y_min <- c()
    y_max <- c()

    for (x in unique(data$x)) {
      y <- data$y[data$x == x]
      fill <- data$fill[data$x == x]

      y_bottom <- y[as.numeric(fill) <= mid]
      y_top <- y[as.numeric(fill) > mid]

      y_min_bottom <- rev(cumsum(rev(y_bottom))) * -1
      y_min_top <- cumsum(c(0, y_top[-length(y_top)]))

      if (!even & mid %in% fill) {
        y_mid <- y[fill == mid]
        y_min_bottom <- y_min_bottom + y_mid / 2
        y_min_top <- y_min_top + y_mid / 2
      }

      y_min <- c(y_min, c(y_min_bottom, y_min_top))
      y_max <- c(y_max, c(y_min_bottom, y_min_top) + y)
    }

    data$ymin <- y_min
    data$ymax <- y_max

    return(data)
  }
)
