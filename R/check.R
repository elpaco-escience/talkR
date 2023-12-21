#' Check the presence of necessary columns in a dataset
#'
#' @param data dataset to check
#' @param columns a vector of column names that must be present
#'
#' @return nothing, but throws an error if a column is missing
check_columns <- function(data, columns){
  for (column in columns){
    if(!column %in% colnames(data)){
      stop(paste0("Column `",column,"` was not found in the dataset."))
    }
  }
}


#' Verify that timing columns are numeric and likely indicate milliseconds.
#'
#' @param column vector with timing information
#' @param name name of the column
#'
#' @return nothing, but throws an error if the column is not numeric and warns if the column may not indicate milliseconds
check_time <- function(column, name){
  if(!is.numeric(column)){
    stop("Column `",name,"` must be numeric.")
  }
  if(max_na(column) < 5*60*1000){ # expected conversation duration is more than 5 minutes, corresponding to 5*60*1000 milliseconds
    warning("Verify that column `",name,"` is in milliseconds.")
  }
}


#' Check the presence of talkr-workflow columns in the dataset.
#'
#' Uses check_columns() to check for:
#' - begin
#' - end
#' - participant
#' - utterance
#' - source
#' - uid
#'
#' Verifies that begin and end columns are numeric, and likely indicate milliseconds.
#'
#' @param data dataset to check
check_talkr <- function(data) {
  required_cols <- c("begin", "end", "participant", "utterance", "source", "uid")
  check_columns(data, required_cols)

  numeric_cols <- c("begin", "end")
  for (col in numeric_cols){
    check_time(data[[col]], col)
  }
}
