#' Coordinate system that stacks a wide plot to multiple layers
#'
#' @param width Width of the stacked layers in seconds
#' @export
#' @inheritParams ggplot2::coord_trans
coord_stack <- function(width, ...) {
  width <- width * 1000 # convert to ms
  ggplot2::coord_trans(x = stacking_x_axis(width), ...)
}


stacking_x_axis <- function(width){
  force(width)
  trans <- function(x) {
    x <- ifelse(x < width, x, x %% width)
    x
  }

  scales::trans_new(
    name = "stackx",
    transform = trans,
    inverse = function(x){
      x/width
    }
  )
}
