#' Coordinate system that stacks a wide plot to multiple layers
#'
#' @param width the length of a single layer
#' @export
#' @inheritParams ggplot2::coord_cartesian
coord_stack <- function(width, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordStack,
          width,
          expand = expand,
          clip = clip
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordStack <- ggproto("CoordStack", CoordCartesian,

)
