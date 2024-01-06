#' Generate a token-specific dataframe
#'
#' From a dataframe with utterances, generate a dataframe that separates tokens
#' in utterances, and assesses their relative timing.
#' The returned data contains information about the original utterance (`uid`),
#' as well as the number of tokens in the utterance (`nwords`), and the relative
#' time of the token in the utterance (`relative_time`).
#'
#' The relative time is calculated with each token in an utterance having an equal
#' duration (the duration of the utterance divided by the number of words), and
#' the first token in the utterance beginning at the beginning of the utterance.
#'
#' The input column provided with the argument `utterancecol` is used to generate
#' the tokens. It is advised to provide a version of the utterance that has been
#' cleaned and stripped of special characters. Cleaning is not performed in this
#' function. Spaces are used to separate tokens.
#'
#' @param data a talkr dataset
#' @param utterancecol the name of the column containing the clean utterance (defaults to "utterance")
#'
#' @return a dataframe with details about each token in the utterance
#' @export
tokenize <- function(data, utterancecol = "utterance") {
  check_columns(data, utterancecol)
  check_talkr(data)

  # split utterances into tokens
  data <- data |>
    dplyr::mutate(utterance_list = stringr::str_split(.data[[utterancecol]], " ")) |>
    tidyr::unnest(cols = "utterance_list") |>
    dplyr::rename(token = "utterance_list") |>
    dplyr::filter(.data$token != "") |>
    dplyr::mutate(tokenorder = stats::ave(seq_along(.data$uid), .data$uid, FUN = seq_along))

  # count tokens per utterance
  count <- data |>
    dplyr::group_by(.data$uid) |>
    dplyr::summarise(nwords = dplyr::n())

  # merge timing data with token data and calculate timing
  data <- data |>
    dplyr::left_join(count, by = "uid") |>
    dplyr::mutate(time_per_token = (.data$end - .data$begin) / .data$nwords,
                  starttime = .data$begin + (0.5 * .data$time_per_token),
                  relative_time = round(.data$starttime + (.data$tokenorder - 1) * .data$time_per_token, 0),
                  order = dplyr::case_when(
                    .data$tokenorder == 1 & .data$nwords == 1 ~ "only",
                    .data$tokenorder == 1 ~ "first",
                    .data$tokenorder == .data$nwords ~ "last",
                    TRUE ~ "middle")) |>
    dplyr::select(.data$source, .data$uid, .data$participant, .data$nwords, .data$token, .data$order, .data$relative_time)

  return(data)
}
