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
#' function. Spaces are used to separate words.
#'
#' @param data a talkr dataset
#' @param utterancecol the name of the column containing the clean utterance (defaults to "utterance")
#'
#' @return
#' @export
tokenize <- function(data, utterancecol = "utterance") {
  check_columns(data, utterancecol)
  check_talkr(data)

  data <- data |>
    dplyr::mutate(utterance_list = stringr::str_split(.data[[utterancecol]], " ")) |>
    dplyr::mutate(nwords = purrr::map_int(utterance_list, length)) |>
    dplyr::mutate(time_per_token = (.data$end - .data$begin) / nwords) |>
    tidyr::unnest(cols = "utterance_list") |>
    dplyr::rename(token = "utterance_list") |>
    dplyr::mutate(tokenorder = stats::ave(seq_along(uid), uid, FUN = seq_along)) |>
    dplyr::mutate(relative_time = round(begin + (tokenorder - 1) * time_per_token, 0)) |>
    dplyr::select(source, uid, participant, nwords, token, relative_time)
  return(data)
}
