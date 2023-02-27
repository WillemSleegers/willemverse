#' Plot vertical histograms per group
#'
#' This geom plots the distribution of a variable for each group by creating
#' vertical histograms using [stat_vbin()]. It is particularly suitable for
#' visualizing distributions of categorical data such as Likert-scale data.
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
#' @param position Position adjustment, either as a string naming the adjustment
#'   (e.g. `"jitter"` to use `position_jitter`), or the result of a call to a
#'   position adjustment function. Use the latter if you need to change the
#'   settings of the adjustment.
#' @param ... Other arguments passed on to layer(). These are often aesthetics,
#'   used to set an aesthetic to a fixed value, like colour = "red" or size = 3.
#'   They may also be parameters to the paired geom/stat.
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

#' @param geom,stat Use to override the default connection between
#'   geom_vhistogram() and stat_vhistogram().
#'
#' @seealso [hist()] and [ggplot2::geom_histogram()]
#'
#' @export
geom_vhistogram <- function(mapping = NULL, data = NULL, stat = "vbin",
                            position = "identity", na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    mapping = mapping, data = data, geom = GeomVhistogram, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomVhistogram <- ggplot2::ggproto(
  "GeomVhistogram",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = NA, fill = "grey35", linewidth = 0.5, linetype = 1,
    alpha = NA
  ),
  extra_params = c("na.rm"),
  setup_data = function(data, params) {
    data
  },
  draw_panel = function(data, panel_params, coord) {
    coords <- coord$transform(data, panel_params)

    grid::rectGrob(
      x = coords$xmin,
      y = coords$ymax,
      width = coords$xmax - coords$xmin,
      height = coords$ymax - coords$ymin,
      default.units = "native",
      just = c("left", "top"),
      gp = grid::gpar(
        col = coords$colour,
        fill = ggplot2::alpha(coords$fill, coords$alpha)
      )
    )
  },
  draw_key = ggplot2::draw_key_rect
)
