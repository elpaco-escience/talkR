#' Initialize a `talkr` dataset
#'
#' From a dataframe object, generate a talkr dataset.
#' This dataset contains columns that are used throughout the talkr
#' infrastructure to visualize conversations and language corpora.
#' Initializing a talkr dataset is the first step in the talkr workflow.
#'
#' @param data A dataframe object
#' @param source The column name identifying the conversation source (e.g. a filename; is used as unique conversation ID)
#' @param begin The column name with the begin time of the utterance (in milliseconds)
#' @param end The column name with the end time of the utterance (in milliseconds)
#' @param participant The column name with the participant who produced the utterance
#' @param utterance The column name with the utterance itself
#'
#' @return A dataframe object with columns needed for the talkr workflow
#' @export
init <- function(data,
                 source = "source",
                 begin = "begin",
                 end = "end",
                 participant = "participant",
                 utterance = "utterance"){

  # verify that column names declared actually exist in the dataset
  names_required <- c(source, begin, end, participant, utterance)
  check_columns(data, names_required)

  data <- data |>
    dplyr::rename(source = all_of(source),
                  begin = all_of(begin),
                  end = all_of(end),
                  participant = all_of(participant),
                  utterance = all_of(utterance))

  return(data)
}
