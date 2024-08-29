#' Get IFADV data
#'
#' IFA Dialog Video corpus data
#' Available in the public repository:
#' https://github.com/elpaco-escience/ifadv
#'
#' @return A tibble containing the data
#' @export
#'
get_ifadv <- function() {

  devtools::install_github("elpaco-escience/ifadv", quiet=TRUE)
  data <- ifadv::ifadv

  return(data)
}
