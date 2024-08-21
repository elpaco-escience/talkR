#' Plot individual tokens
#'
#' From a separate data frame containing tokenized data, plot individual tokens
#' at their estimated time. Data must be provided separately, and should
#' contain a column with the participant (y) and a column with the time (x).
#'
#' @param data A tokenized data frame (see `tokenize()`).
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @export
geom_token <- function(data, mapping = NULL,
                          stat = "identity", position = "identity",
                          ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomToken,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...)
  )
}

#' GeomToken
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#'
#'
#' @export
GeomToken <- ggplot2::ggproto(
  "GeomToken", Geom,
  required_aes = c("x", "y"),

  default_aes = aes(
    fill = "grey90",
    colour = "grey40",
    alpha = 1,
    size = 1,
    shape = 19,
    stroke = 1
  ),

  draw_panel = function(data, panel_params, coord, ...) {
    ggplot2::GeomPoint$draw_panel(data, panel_params, coord)
  }
)

