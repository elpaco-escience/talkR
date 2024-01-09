#' Plot conversation
#'
#' @param data a talkr dataset
#' @param sourcecount number of the source to plot (defaults to 1)
#'
#' @return plot object
#' @export
plot_turns_tokens <- function(data, sourcecount = 1){
  check_talkr(data)

  window_length = 60 * 1000 # one minute, in milliseconds. The duration of a single line.

  sourcecount <- min(sourcecount, length(unique(data$source)))
  source <- unique(data$source)[sourcecount]

  data <- data[data$source == source,]
  data <- data[data$end < window_length,]

  tokens <- data |>
    tokenize()

  tokens <- tokens[tokens$rank < 10,]

  p <- data |>
    ggplot2::ggplot(aes(x = end, y = participant)) +
    talkr::geom_turn(aes(
      begin = begin,
      end = end)) +
    talkr::geom_token(data = tokens,
                      aes(x = relative_time,
                          y = participant,
                          color = rank)) +
    talkr::theme_turnPlot() +
    ggplot2::xlab("time (ms)") +
    ggplot2::ggtitle(source)


  return(p)
}


