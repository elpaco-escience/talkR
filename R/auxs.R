#' Get IFADV data
#'
#' IFA Dialog Video corpus data
#' Available in the public repository:
#' https://github.com/elpaco-escience/ifadv
#'
#' This function requires an internet connection.
#'
#' @param source (default = "https://raw.githubusercontent.com/elpaco-escience/ifadv/csv/data/ifadv.csv")
#'
#' @return A data frame containing the IFADV dataset
#' @export
#'
get_ifadv <- function(source="https://raw.githubusercontent.com/elpaco-escience/ifadv/csv/data/ifadv.csv") {
  return(utils::read.csv(source))
}
