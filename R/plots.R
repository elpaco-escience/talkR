#' Check source quality by plotting timing data
#'
#' @param data talkr data frame
#' @param source source to be checked (default is "all", no source is selected)
#' @param saveplot save plot to file (default is FALSE)
#'
#' @return list of recorded plots
#' @export
plot_quality <- function(data, source = "all", saveplot = FALSE){
  check_talkr(data)

  if(source != "all"){
    data <- data[data$source == source,]
  }

  if(nrow(data) == 0){
    stop("No data for source ", source)
  }

  timing <- calculate_timing(data)
  nturns <- nrow(timing)
  ntransitions <- sum(!is.na(timing$transition_time))

  plot_duration <- plot_density(timing,
                                colname = "turn_duration",
                                title = paste(nturns, "turns"),
                                xlab = "turn duration (ms)",
                                ylab = "density")

  plot_transition <- plot_density(timing,
                                  colname = "transition_time",
                                  title = paste(ntransitions, "transitions"),
                                  xlab = "transition (ms)",
                                  ylab = "")

  plot_duration_transition <- plot_scatter(timing,
                                           colname_x = "transition_time",
                                           colname_y = "turn_duration",
                                           title = "",
                                           xlab = "transition (ms)",
                                           ylab = "turn duration (ms)")

  panel <- cowplot::plot_grid(
    plot_duration,
    plot_transition,
    plot_duration_transition,
    labels = c("", "", source),
    hjust = 0,
    nrow=1
  )

  print(panel)

  if(saveplot) {
    sourceclean <- gsub("[./]", "", source)
    filename <- paste0('quality-panel-',sourceclean,'.png')
    cowplot::save_plot(filename = filename,
                       plot = panel)
    message("Saved plot to ", paste0(getwd(), "/", filename))
  }

}


#' Make a density plot of a specific column
#'
#' @param data data frame containing the column
#' @param colname column name for which the density should be plotted
#' @param title plot title
#' @param xlab x-axis label
#' @param ylab y-axis label
#'
#' @return recorded plot
#' @export
plot_density <- function(data, colname, title = "Density", xlab = "value", ylab = "density"){
  check_columns(data, colname)

  p <- data |>
    ggplot2::ggplot(ggplot2::aes(.data[[colname]])) +
    ggthemes::theme_tufte() +
    ggplot2::ggtitle(title) +
    ggplot2::geom_density(na.rm=T) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  return(p)
}

#' Make a scatter plot of two columns
#'
#' @param data data frame containing the columns
#' @param colname_x name of column plotted on x-axis
#' @param colname_y name of column plotted on y-axis
#' @param title plot title
#' @param xlab x-axis label
#' @param ylab y-axis label
#'
#' @return recorded plot
#' @export
plot_scatter <- function(data, colname_x, colname_y, title = "Scatter", xlab = "x", ylab = "y"){
  check_columns(data, c(colname_x, colname_y))

  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[colname_x]],
                                 y = .data[[colname_y]])) +
    ggthemes::theme_tufte() +
    ggplot2::ggtitle(title) +
    ggplot2::geom_point(alpha=0.1,na.rm=T) +
    ggplot2::geom_vline(xintercept = 0,colour="#cccccc") +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  return(p)
}
