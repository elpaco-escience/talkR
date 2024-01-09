#' Plot a stretch of conversation
#'
#' Utterances are plotted as grey boxes, with frequently occurring tokens overlaid as colored dots.
#'
#' @param data a talkr dataset
#' @param time_start start time of the plot in seconds (defaults to 0)
#' @param time_end end time of the plot in seconds (defaults to 60)
#' @param maxrank maximum rank of tokens to plot (defaults to 10)
#' @param source name or number of the source to plot (defaults to 1, plotting the first source in the data)
#'
#' @return plot object
#' @export
plot_turns_tokens <- function(data,
                              time_start = 0,
                              time_end = 60,
                              maxrank = 10,
                              source = 1){
  check_talkr(data)

  if(is.numeric(source)){
    sourcenr <- min(source, length(unique(data$source)))
    source <- unique(data$source)[sourcenr]
  }
  data <- data[data$source == source,]

  tokens <- data |>
    tokenize()
  tokens <- tokens[tokens$rank < maxrank,]

  time_start <- time_start * 1000
  time_end <- time_end * 1000
  data <- data[data$begin > time_start & data$end < time_end,]

  uids_included <- unique(data$uid)
  tokens <- tokens[tokens$uid %in% uids_included,]

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


