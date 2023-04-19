#' Position text on top of a bar
#'
#' `position_bar()` positions text on top of bar geoms. The positioning works
#' for both text geoms and label geoms.
#' @export
position_bar <- function(nudge = 0) {
  ggplot2::ggproto(NULL, PositionBar, nudge = nudge)
}

#' @format NULL
#' @usage NULL
#' @export
PositionBar <- ggplot2::ggproto("PositionBar", ggplot2::Position,
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
    return(data)
  }
)
