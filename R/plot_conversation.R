plot_conversation <- function(data){
  check_talkr(data)

  window_length = 60 * 1000 # one minute, in milliseconds. The duration of a single line.
  timing_length = 1000 # one second, in milliseconds. The timing indicated on the x-axis.

  data <- data |>
    dplyr::mutate(participant_int = as.integer(as.factor(.data$participant)),
                  scope = floor(.data$begin/window_length),
                  begin = (.data$begin - (.data$scope*window_length)) / timing_length,
                  end = (.data$end - (.data$scope*window_length)) / timing_length)

  p <- data |>
    ggplot2::ggplot(
      ggplot2::aes(y=.data$participant_int)) +
    ggthemes::theme_tufte() +
    ggplot2::theme(strip.text = element_text(hjust=0,color="grey50")) +
    ggplot2::ylab("") +
    ggplot2::xlab("time (s)") +
    ggplot2::scale_y_continuous(breaks=c(1:max(data$participant_int))) +
    ggplot2::theme(axis.ticks.y = element_blank()) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin=.data$begin,
                   xmax=.data$end,
                   ymin=.data$participant_int-0.4,
                   ymax=.data$participant_int+0.4),
              fill="grey90") +
    ggplot2::facet_wrap(~ .data$scope, ncol=1)

  return(p)
}

