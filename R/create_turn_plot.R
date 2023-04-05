#' create turn plot as originally designed by mark
#'
#' @param
#'
#' @export

create_turn_plot <- function() {
  test_dir <- read.csv(testthat::test_path("testdata", "sample_test.csv"))
  extract_length <- 600000 # 10 min
  window_size <- 60000 # 1 min
  window_breaks <- as.integer(c(0:round(extract_length/window_size)) * window_size)

  plot.base <- test_dir[1:2000, ] |>
    ggplot2::ggplot(ggplot2::aes(y = participant_int)) +
    ggthemes::theme_tufte() + ggplot2::theme(legend.position = "none",
                                             strip.text = ggplot2::element_blank(),
                                             axis.text.y = ggplot2::element_blank(),
                                             axis.ticks.y = ggplot2::element_blank(),
                                             plot.title.position = "plot") +
    ggplot2::ylab("") + ggplot2::xlab("time (s)") +
    viridis::scale_fill_viridis(option = "plasma", direction = 1, begin = 0.2, end = 0.8) +
    ggplot2::scale_y_reverse(breaks = seq(1, max(test_dir$line, 1)),
                             labels = seq(1, max(test_dir$line, 1))) +
    ggplot2::scale_x_continuous(limits = c(0, window_size),
                                breaks = seq(0, window_size, 10000),
                                label = seq(0, window_size / 1000, 10),
                                oob = scales::oob_keep)

  plot.base +
    ggplot2::theme(axis.text.y = ggplot2::element_text()) +
    ggplot2::ggtitle("Ten minutes of conversation",
                     subtitle = "Points are interjections, time moves left-right and top-bottom") +
    ggplot2::geom_rect(ggplot2::aes(xmin = begin0, xmax = end0,
                                    ymin = line - 0.5 + participant_int / 3 - 0.2,
                                    ymax = line - 0.5 + participant_int / 3 + 0.2),
                       linewidth = 0, colour = NA, fill = "lightgrey") +
    ggplot2::geom_point(data = plot.base$data |> dplyr::filter(nwords == 1 & transitions == 1),
                        ggplot2::aes(x = begin0 + 200,
                                     fill = rank,
                                     y = line - 0.5 + participant_int / 3),
                        colour = "white",
                        size = 2,
                        shape = 21,
                        stroke = 1)


}



#' create turn plot for testing purposes
#'
#' @param
#'
#' @export

create_turn_plot_modified <- function() {
  test_dir <- read.csv(testthat::test_path("testdata", "sample_test.csv"))
  extract_length <- 600000 # 10 min
  window_size <- 60000 # 1 min
  window_breaks <- as.integer(c(0:round(extract_length/window_size)) * window_size)

  plot.base <- test_dir[1:2000, ] |>
    ggplot2::ggplot(ggplot2::aes(y = participant_int)) +
    # we got our theme here
    talkr::theme_turnPlot() +
    ggplot2::ylab("") + ggplot2::xlab("time (s)") +
    viridis::scale_fill_viridis(option = "plasma", direction = 1, begin = 0.2, end = 0.8) +
    ggplot2::scale_y_reverse(breaks = seq(1, max(test_dir$line, 1)),
                             labels = seq(1, max(test_dir$line, 1))) +
    ggplot2::scale_x_continuous(limits = c(0, window_size),
                                breaks = seq(0, window_size, 10000),
                                label = seq(0, window_size / 1000, 10),
                                oob = scales::oob_keep)

  plot.base +
    ggplot2::theme(axis.text.y = ggplot2::element_text()) +
    ggplot2::ggtitle("Ten minutes of conversation",
                     subtitle = "Points are interjections, time moves left-right and top-bottom") +
    ggplot2::geom_rect(ggplot2::aes(xmin = begin0, xmax = end0,
                                    ymin = line - 0.5 + participant_int / 3 - 0.2,
                                    ymax = line - 0.5 + participant_int / 3 + 0.2),
                       linewidth = 0, colour = NA, fill = "lightgrey") +
    ggplot2::geom_point(data = plot.base$data |> dplyr::filter(nwords == 1 & transitions == 1),
                        ggplot2::aes(x = begin0 + 200,
                                     fill = rank,
                                     y = line - 0.5 + participant_int / 3),
                        colour = "white",
                        size = 2,
                        shape = 21,
                        stroke = 1)


}

