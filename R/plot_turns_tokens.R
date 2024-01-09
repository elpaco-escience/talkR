#' Plot conversation
#'
#' @param data a talkr dataset
#' @param source a sourcename to extract; default is NULL, then the first source will be picked
#'
#' @return plot object
#' @export
plot_turns_tokens <- function(data, sourcecount = 1){
  check_talkr(data)

  window_length = 60 * 1000 # one minute, in milliseconds. The duration of a single line.

  sourcecount <- max(sourcecount, length(unique(data$source)))
  source <- unique(data$source)[sourcecount]

  data <- data[data$source == source,]
  data <- data[data$end < window_length,]

  tokens <- data |>
    tokenize()

  p <- data |>
    ggplot2::ggplot(aes(x = end, y = participant)) +
    talkr::geom_turn(aes(
      begin = begin,
      end = end)) +
    talkr::geom_token(data = tokens,
                      aes(x = relative_time,
                          y = participant)) +
    talkr::theme_turnPlot() +
    ggplot2::xlab("time (ms)") +
    ggplot2::ggtitle(source)


  return(p)
}


