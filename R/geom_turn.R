#' Show turn-taking in visualized conversations
#'
#' @param mapping Set of aesthetic mappings created by `ggplot2::aes()`.
#'    Requires specification of `begin` and `end` of turns. Inherits from the default mapping at the
#'    top level of the plot, if `inherit.aes` is set to `TRUE` (the default).
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @param height The height of the turn-taking rectangles
#' @export
geom_turn <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ..., na.rm = FALSE, height = 0.5, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomTurn,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  height = height,
                  ...)
  )
}

#' GeomTurn
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTurn <- ggproto(
  "GeomTurn", Geom,
  required_aes = c("begin", "end"),

  default_aes = aes(
    fill = "grey80",
    linewidth = 0,
    alpha = 1
  ),

  extra_params = c("na.rm", "height"),

  setup_data = function(data, params) {

    data$height <- params$height

    data <- transform(data,
                      ymin = y - 0.5*height,
                      ymax = y + 0.5*height)

    data
  },

  draw_panel = function(data, panel_params, coord, ...) {
    rect_data <- transform(data,
                           xmin = begin,
                           xmax = end,
                           ymin = ymin,
                           ymax = ymax)
    ggplot2::GeomRect$draw_panel(rect_data, panel_params, coord)
  }
)

