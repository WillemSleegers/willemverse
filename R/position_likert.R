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
  setup_params = function(self, data) {
    data
  },
  setup_data = function(self, data, params) {
    data
  },
  compute_panel = function(data, params, scales) {
    print(as_tibble(data))
    print(levels(data$fill))
    ymin <- c()
    ymax <- c()
    y_label <- c()

    for (group in unique(data$x)) {
      y <- data$y[data$x == group]
      len <- length(y)

      if (len %% 2 == 0) {
        mid <- len / 2
        y_bottom <- c(y[1:mid], 0)
        y_top <- y[(mid + 1):len]
      } else {
        mid <- ceiling(len / 2)
        y_bottom <- c(y[1:(mid - 1)], y[mid] / 2)
        y_top <- c(y[mid] / 2, y[(mid + 1):len])
      }

      y <- c(rev(cumsum(rev(y_bottom))) * -1, cumsum(y_top))
      ymin <- c(ymin, y[-length(y)])
      ymax <- c(ymax, y[-1])

      if ("label" %in% names(data)) {
        y_label <- c(y_label, y[-length(y)] + diff(y) / 2)
      }
    }

    if ("label" %in% names(data)) {
      data$y <- y_label
    } else {
      data$ymin <- ymin
      data$ymax <- ymax
    }

    print(data)

    return(data)
  }
)
