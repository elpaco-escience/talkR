#' Show turn-taking in visualized conversations
#'
#' @param mapping Set of aesthetic mappings created by aes(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options: If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot().
#' @param stat The statistical transformation to use on the data for this layer, either as a ggproto Geom subclass or as a string naming the stat stripped of the stat_ prefix (e.g. "count" rather than "stat_count")
#' @param position Position adjustment, either as a string naming the adjustment (e.g. "jitter" to use position_jitter), or the result of a call to a position adjustment function. Use the latter if you need to change the settings of the adjustment.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @export
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

