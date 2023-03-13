#' Find UID
#'
#' @param string a string
#' @param data a dataframe
#'
#' @export
finduid <- function(string, data) {
  data[data$uid %in% string,names(data) %in% c("uid","source","begin","end")]
}
