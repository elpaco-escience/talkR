#' Get IFADV data
#'
#' IFA Dialog Video corpus data
#' Available in the public repository:
#' https://github.com/elpaco-escience/ifadv
#'
#' @param destfile (default = "data/ifadv.rda") Destination file
#' @param clean_after (default = TRUE) Cleans the downloaded file after using it
#'
#' @return Nothing, but it loads the data into the global environment
#' @export
#'
get_ifadv <- function(destfile = "data/ifadv.rda", clean_after = TRUE) {

  download.file("https://raw.github.com/elpaco-escience/ifadv/main/data/ifadv.rda", destfile, quiet = TRUE)

  load(destfile, envir = .GlobalEnv) # Load it to the outside environment

  on.exit( # Clean your mess after yourself
    if(clean_after & file.exists(destfile)) file.remove(destfile)
  )
}
