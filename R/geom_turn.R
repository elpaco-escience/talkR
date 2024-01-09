#' Show turn-taking in visualized conversations
#'
#' @param mapping Set of aesthetic mappings created by aes(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options: If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot().
#' @param stat The statistical transformation to use on the data for this layer, either as a ggproto Geom subclass or as a string naming the stat stripped of the stat_ prefix (e.g. "count" rather than "stat_count")
#' @param position Position adjustment, either as a string naming the adjustment (e.g. "jitter" to use position_jitter), or the result of a call to a position adjustment function. Use the latter if you need to change the settings of the adjustment.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param height The height of the rectangles representing turns
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @export
#' @rdname geom_turn
geom_turn <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ..., na.rm = FALSE, height = 0.8, show.legend = NA, inherit.aes = TRUE) {
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
  required_aes = c("begin", "end", "participant"),

  default_aes = aes(
    fill = "grey80",
    linewidth = 0,
    alpha = 1
  ),

  extra_params = c("na.rm", "height"),

  setup_data = function(data, params) {
    data$participant_int <- as.integer(as.factor(data$participant))
    data$ymin <- data$participant_int - (0.5*params$height)
    data$ymax <- data$participant_int + (0.5*params$height)

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

