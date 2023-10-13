# make_talkr_dataset(data,
#                    begin = "kolom1",
#                    end = "kolom2",
#                    participant = "x",
#                    utterance = "y")

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
#' @param participant The participant who produced the utterance
#' @param utterance The utterance itself
#'
#' @return A dataframe object with columns needed for the talkr workflow
#' @export
init <- function(data,
                 begin = "begin",
                 end = "end",
                 participant = "participant",
                 utterance = "utterance"){
  data <- data |>
    dplyr::rename(begin = begin,
                  end = end,
                  participant = participant,
                  utterance = utterance)
  return(data)
}
