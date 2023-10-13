#' Initialize a `talkr` dataset
#'
#' From a dataframe object, generate a talkr dataset.
#' This dataset contains columns that are used throughout the talkr
#' infrastructure to visualize conversations and language corpora.
#' Initializing a talkr dataset is the first step in the talkr workflow.
#'
#' @param data A dataframe object
#' @param begin The column name with the begin time of the utterance
#' @param end The column name with the end time of the utterance
#' @param participant The column name with the participant who produced the utterance
#' @param utterance The column name with the utterance itself
#' @param timeunit The unit of time for the begin and end columns
#'                (default: "ms"; options: "ms" for milliseconds, "s" for seconds, "m" for minutes).
#'
#' @return A dataframe object with columns needed for the talkr workflow
#' @export
init <- function(data,
                 begin = "begin",
                 end = "end",
                 participant = "participant",
                 utterance = "utterance",
                 timeunit = "ms"){
  data <- data |>
    dplyr::rename(begin = begin,
                  end = end,
                  participant = participant,
                  utterance = utterance)

  # time conversion
  if(timeunit == "ms"){
    conversion <- 1
  } else if(timeunit == "s"){
    conversion <- 1000
  } else if(timeunit == "m"){
    conversion <- 60 * 1000
  } else {
    stop("timeunit must be one of 'ms', 's', or 'm'")
  }
  data$begin <- data$begin * conversion
  data$end <- data$end * conversion

  return(data)
}
