#' Get IFADV data
#'
#' IFA Dialog Video corpus data
#' Available in the public repository:
#' https://github.com/elpaco-escience/ifadv
#'
#' @param destfile (default = "ifadv.rda") Destination file
#' @param clean_after (default = TRUE) Cleans the downloaded file after using it
#'
#' @return A tibble containing the IFADV dataset
#' @importFrom utils download.file
#' @export
#'
get_ifadv <- function(destfile = "ifadv.rda", clean_after = TRUE) {

  # We are forced to download the data into a temporary file
  # and load the file into memory locally
  # The snippet below makes this easy to do
  if(file.exists(destfile)) {
    stop("The file already exists. Choose a different destfile")
  } else {
      file.create(destfile) # Create a dummy file
  }
  # Let's not forget cleaning our mess after ourselves
  on.exit(if(clean_after & file.exists(destfile)) file.remove(destfile))

  download.file("https://raw.github.com/elpaco-escience/ifadv/main/data/ifadv.rda", destfile, quiet = TRUE)

  e <- new.env()
  load(destfile, envir = e) # Load it to the function environment ...
  return(e[["ifadv"]]) # ... and return it as an output
}
