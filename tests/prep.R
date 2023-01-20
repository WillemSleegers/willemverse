
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(viridis)

# stat_vhistogram() -------------------------------------------------------

# Create some data
set.seed(1)

data <- tibble(
  condition = sample(c("control", "treatment"), 50, replace = TRUE),
  outcome = sample(1:7, 50, replace = TRUE)
)

# data$outcome[1] <- NA

# Test out the stat
ggplot(data, aes(x = condition, y = outcome)) +
  stat_vhistogram()

ggplot(data, aes(x = condition, y = outcome)) +
  stat_vhistogram(alpha = .5)

ggplot(data, aes(x = condition, y = outcome)) +
  stat_vhistogram(alpha = .5, color = "black")

ggplot(data, aes(x = condition, y = outcome)) +
  stat_vhistogram(alpha = .5, color = "black", center = TRUE) +
  scale_y_continuous(breaks = 1:7)

ggplot(data, aes(x = condition, y = outcome)) +
  stat_vhistogram(center = TRUE, alpha = .25) +
  scale_y_continuous(breaks = 1:7) +
  stat_summary(geom = "errorbar", width = .1) +
  stat_summary(geom = "point", fun = "mean") +
  theme_minimal()

# geom_likert() -----------------------------------------------------------

unique0 <- function(x, ...) if (is.null(x)) x else vctrs::vec_unique(x, ...)

StatProp <- ggproto("StatProp", Stat,
  required_aes = "x|y",
  default_aes = aes(x = after_stat(count), y = after_stat(count), weight = 1),
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(
      data, params,
      main_is_orthogonal = FALSE
    )

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort(
        paste(
          "{.fn {snake_class(self)}} requires an {.field x}",
          "or {.field y} aesthetic."
        )
      )
    }
    if (has_x && has_y) {
      cli::cli_abort(
        paste(
          "{.fn {snake_class(self)}} must only have an {.field x}",
          "{.emph or} {.field y} aesthetic."
        )
      )
    }

    if (is.null(params$width)) {
      x <- if (params$flipped_aes) "y" else "x"
      params$width <- resolution(data[[x]]) * 0.9
    }

    params
  },
  extra_params = c("na.rm", "orientation"),
  compute_group = function(self,
                           data,
                           scales,
                           width = NULL,
                           flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    x <- data$x
    weight <- data$weight %||% rep(1, length(x))

    count <- as.numeric(tapply(weight, x, sum, na.rm = TRUE))
    count[is.na(count)] <- 0

    bars <- data_frame(
      count = count,
      prop = count / sum(abs(count)),
      x = sort(unique0(x)),
      width = width,
      flipped_aes = flipped_aes,
      .size = length(count),
      .name_repair = "minimal"
    )
    flip_data(bars, flipped_aes)
  },
  dropped_aes = "weight"
)

stat_prop <- function(mapping = NULL, data = NULL,
                      geom = "bar", position = "stack",
                      ...,
                      width = NULL,
                      na.rm = FALSE,
                      orientation = NA,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  params <- rlang::list2(
    na.rm = na.rm,
    orientation = orientation,
    width = width,
    ...
  )

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatProp,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

GeomLikert <- ggproto("GeomBar", GeomRect,
  required_aes = c("x", "y"),

  # These aes columns are created by setup_data(). They need to be listed here
  # so that GeomRect$handle_na() properly removes any bars that fall outside
  # the defined limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },
  extra_params = c("na.rm", "orientation"),
  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

    data <- transform(
      data,
      ymin = pmin(y, -5), ymax = pmax(y, 0),
      xmin = x - width / 2, xmax = x + width / 2,
      width = NULL
    )

    flip_data(data, params$flipped_aes)
    print(data)
  },
  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", width = NULL, flipped_aes = FALSE) {
    print(data)
    # Hack to ensure that width is detected as a parameter
    ggproto_parent(GeomRect, self)$draw_panel(
      data,
      panel_params,
      coord,
      lineend = lineend,
      linejoin = linejoin
    )
  }
)

geom_likert <- function(mapping = NULL, data = NULL,
                        stat = "count", position = "stack",
                        ...,
                        width = NULL,
                        na.rm = FALSE,
                        orientation = NA,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLikert,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}



likert <- function(vjust = 1, reverse = FALSE) {
  ggproto(NULL, PositionLikert, vjust = vjust, reverse = reverse)
}

stack_var <- function(data) {
  if (!is.null(data$ymax)) {
    if (any(data$ymin != 0 & data$ymax != 0, na.rm = TRUE)) {
      cli::cli_warn("Stacking not well defined when not anchored on the axis")
    }
    "ymax"
  } else if (!is.null(data$y)) {
    "y"
  } else {
    cli::cli_warn(c(
      "Stacking requires either the {.field ymin} {.emph and} {.field ymin} or the {.field y} aesthetics",
      "i" = "Maybe you want {.code position = \"identity\"}?"
    ))
    NULL
  }
}

PositionLikert <- ggproto("PositionLikert", Position,
  type = NULL,
  vjust = 1,
  fill = FALSE,
  reverse = FALSE,
  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    list(
      var = self$var %||% stack_var(data),
      fill = self$fill,
      vjust = self$vjust,
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

    data <- remove_missing(
      data,
      vars = c("x", "xmin", "xmax", "y"),
      name = "position_stack"
    )
    flip_data(data, params$flipped_aes)
  },
  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)

    print(data)

    ymin <- c()
    ymax <- c()
    groups <- unique(data$x)

    if ("y" %in% names(data)) outcome <- data$y
    if ("count" %in% names(data)) outcome <- data$count

    for (group in groups) {
      counts <- outcome[data$x == group]
      mid <- ceiling(length(counts) / 2)

      counts_bottom <- c(counts[1:(mid - 1)], counts[mid] / 2)
      counts_top <- c(counts[mid] / 2, counts[(mid + 1):length(counts)])

      y <- c(rev(cumsum(rev(counts_bottom))) * -1, cumsum(counts_top))
      ymin <- c(ymin, y[-length(y)])
      ymax <- c(ymax, y[-1])
    }

    data$ymin <- ymin
    data$ymax <- ymax

    flip_data(data, params$flipped_aes)
  }
)

ggplot(data, aes(x = condition, fill = factor(outcome))) +
  geom_bar(position = "likert") +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip()

counts <- count(data, condition, outcome) %>%
  group_by(condition) %>%
  mutate(pct = n / sum(n))

ggplot(counts, aes(x = condition, y = pct, fill = factor(outcome))) +
  # geom_col(position = "likert") +
  geom_text(aes(label = pct), position = "likert") +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip()
