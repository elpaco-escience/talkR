#' Check the quality of a specific source
#'
#' Quality measure that is considered: the total duration of utterances in ms
#' must not be equal to the number of utterances.
#'
#' @param data dataset including the columns source, begin, and end
#' @param source the source that is verified
#'
#' @return 0 if the quality is good, 1 if the quality is bad
#' @export
check_quality <- function(data, source){
  check_columns(data, c("source", "begin", "end"))

  # extract only the source we want to check
  data <- data[data$source == source,]
  if(nrow(data) == 0){
    stop(paste("Source", source, "not found in dataset."))
  }

  # number of utterances
  nturns <- nrow(data)

  # duration of spoken utterances
  data$duration <- data$end - data$begin
  total_duration <- sum(data$duration, na.rm = TRUE)

  # if the total number of utterances is the same as the length of the spoken utterances
  # that means that they all last one millisecond on average
  # this is a sign of bad quality
  quality <- ifelse(total_duration == nturns, 0, 1)

  if(quality == 0){
    cat(paste("WARNING: Bad data quality for source", source, "\n"))
  }

  return(quality)
}



