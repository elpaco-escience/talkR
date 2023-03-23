#' Find UID
#'
#' @param data a dataframe
#' @param string a string
#'
#' @export
finduid <- function(data, string) {
  data[data$uid %in% string,names(data) %in% c("uid","source","begin","end")]
}
