#' Coordinate system that stacks a wide plot to multiple layers
#'
#' @export
#' @inheritParams ggplot2::coord_trans
coord_stack <- function(...) {
  ggplot2::coord_trans(x = "stackx", ...) # y and x probably with different transformation objects
}

#' Transformation function for x axis stacking
#'
#' @export
stackx_trans <- function(){
  scales::trans_new(
    name = "stackx",
    transform = function(x){
      -x
    },
    inverse = function(x){
      -x
    }
  )
}
