#' Position Likert-type data
#'
#' `position_likert()` positions bars to improve the legibility of Likert-type
#' data. This means it will stack bars on top of each other but center them at
#' the mid-point. The positioning works for both bar geoms and text labels.
#'
#' @param nudge_x A horizontal adjustment useful for positing text labels next
#'   to or on top of the bars rather than inside them.
#' @param reverse If TRUE, will reverse the default stacking order. This is
#'   useful if you're rotating both the plot and legend.
#'
#' @export
likert <- function(nudge_y = 0, reverse = FALSE) {
  ggplot2::ggproto(NULL, PositionLikert, nudge_y = nudge_y, reverse = reverse)
}

PositionLikert <- ggplot2::ggproto("PositionLikert", ggplot2::Position,
  type = NULL,
  nudge_y = 0,
  fill = FALSE,
  reverse = FALSE,
  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    list(
      var = self$var %||% stack_var(data),
      fill = self$fill,
      nudge_y = self$nudge_y,
      reverse = self$reverse,
      flipped_aes = flipped_aes
    )
  },
  setup_data = function(self, data, params) {
    data <- flip_data(data, params$flipped_aes)
    if (is.null(params$var)) {
      return(data)
    }

    data$ymax <- switch(params$var,
      y = data$y,
      ymax = as.numeric(ifelse(data$ymax == 0, data$ymin, data$ymax))
    )

    data$nudge_y <- self$nudge_y

    data <- remove_missing(
      data,
      vars = c("x", "xmin", "xmax", "y"),
      name = "position_likert"
    )
    flip_data(data, params$flipped_aes)
  },
  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)

    groups <- unique(data$x)

    if ("y" %in% names(data)) outcome <- data$y
    if ("count" %in% names(data)) outcome <- data$count

    ymin <- c()
    ymax <- c()
    y_label <- c()

    for (group in groups) {
      counts <- outcome[data$x == group]
      mid <- ceiling(length(counts) / 2)

      counts_bottom <- c(counts[1:(mid - 1)], counts[mid] / 2)
      counts_top <- c(counts[mid] / 2, counts[(mid + 1):length(counts)])

      y <- c(rev(cumsum(rev(counts_bottom))) * -1, cumsum(counts_top))
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

    data$x <- data$x + data$nudge_y

    flip_data(data, params$flipped_aes)
  }
)

stack_var <- function(data) {
  if (!is.null(data$ymax)) {
    if (any(data$ymin != 0 & data$ymax != 0, na.rm = TRUE)) {
      cli::cli_warn("Stacking not well defined when not anchored on the axis")
    }
    "ymax"
  } else if (!is.null(data$y)) {
    "y"
  } else {
    cli::cli_warn(
      c(
        paste(
          "Stacking requires either the {.field ymin} {.emph and}",
          "{.field ymin} or the {.field y} aesthetics"
        ),
        "i" = "Maybe you want {.code position = \"identity\"}?"
      )
    )
    NULL
  }
}
