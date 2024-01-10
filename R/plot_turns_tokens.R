#' Plot a stretch of conversation
#'
#' Utterances are plotted as grey boxes, with frequently occurring tokens overlaid as colored dots.
#'
#' @param data a talkr dataset
#' @param begin start time of the plot in seconds (defaults to 0)
#' @param duration duration of the plot in seconds (defaults to 60)
#' @param maxrank maximum rank of tokens to plot (defaults to 10)
#' @param source name or number of the source to plot (defaults to 1, plotting the first source in the data)
#'
#' @return plot object
#' @export
plot_turns_tokens <- function(data,
                              begin = 0,
                              duration = 60,
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

  time_start <- begin * 1000
  time_end <- time_start + duration * 1000
  data <- data[data$begin > time_start & data$end < time_end,]

  uids_included <- unique(data$uid)
  tokens <- tokens[tokens$uid %in% uids_included,]

  p <- data |>
    ggplot2::ggplot(aes(
      x = .data$end,
      y = .data$participant)) +
    talkr::geom_turn(aes(
      begin = .data$begin,
      end = .data$end)) +
    talkr::geom_token(data = tokens,
                      aes(x = .data$relative_time,
                          y = .data$participant,
                          color = .data$rank)) +
    talkr::theme_turnPlot() +
    ggplot2::xlab("time (ms)") +
    ggplot2::ggtitle(source)


  return(p)
}


