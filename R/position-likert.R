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
    data <- subset(data, !is.na(fill))

    return(data)
  },
  compute_panel = function(data, params, scales) {
    levels <- levels(data$fill)
    len <- length(levels)
    even <- len %% 2 == 0
    mid <- ceiling(len / 2)

    for (x in unique(data$x)) {
      y <- data$y[data$x == x]
      fill <- data$fill[data$x == x]

      y_bottom <- y[as.numeric(fill) < mid]

      # If there are no responses in the bottom half, set the nudge to 0
      if (length(y_bottom) == 0) {
        nudge <- 0
      } else {
        nudge <- sum(y_bottom)
      }

      # Add half of the middle category to the nudge, if there is one
      if (!even & mid %in% as.numeric(fill)) {
        y_mid <- y[as.numeric(fill) == mid]
        nudge <- nudge + y_mid / 2
      }

      if ("label" %in% names(data)) {
        data$y[data$x == x] <- (cumsum(c(0, y[-length(y)])) - nudge) + (y / 2)
      } else {
        ymin <- data$ymin[data$x == x]
        ymax <- data$ymax[data$x == x]

        ymin <- c(0, cumsum(ymax)[-length(ymax)])
        ymax <- cumsum(ymax)

        ymin <- ymin - nudge
        ymax <- ymax - nudge

        data$ymin[data$x == x] <- ymin
        data$ymax[data$x == x] <- ymax
      }
    }

    data$x <- data$x + data$nudge

    return(data)
  }
)
