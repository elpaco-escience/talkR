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


#' Check the presence of talkr-workflow columns in the dataset.
#'
#' Uses check_columns() to check for:
#' - begin
#' - end
#' - participant
#' - utterance
#' - source
#'
#' @param data dataset to check
check_talkr <- function(data) {
  required_cols <- c("begin", "end", "participant", "utterance", "source")
  check_columns(data, required_cols)
}
