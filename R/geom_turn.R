#' Show turn-taking in visualized conversations
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_turn <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomTurn,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTurn <- ggproto(
  "GeomTurn", Geom,
  required_aes = c("xmin", "xmax", "ymin", "ymax", "xpoint", "ypoint", "fillpoint"),

  default_aes = aes(
    fill = "lightgrey",
    linewidth = 0,
    alpha = 1,
    colour = "white", # geom_point
    size = 2,
    shape = 21,
    stroke = 1
  ),

  draw_panel = function(data, panel_params, coord, ...) {
    rect_data <- transform(data, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    point_data <- transform(data, x = xpoint, y = -ypoint, fill = fillpoint)

    grid::grobTree(
      ggplot2::GeomRect$draw_panel(rect_data, panel_params, coord),
      ggplot2::GeomPoint$draw_panel(point_data, panel_params, coord)
    )
  }
)

