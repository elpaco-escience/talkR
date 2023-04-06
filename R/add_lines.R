#' Add information for line-by-line visualization
#'
#' This function adds columns to the dataset that adds a line ID, and a `begin0` and
#' `end0` column for timestamps relative to the beginning of the line, so data can
#' be visualized line-by-line.
#'
#' @param data dataset to divide into lines
#' @param extract_length total length of extract in ms
#' @param window_size length of line in ms
#'
#' @return data set with added columns `line`, `begin0`, `end0`
#' @export
#'
add_lines <- function(data,extract_length=600000,window_size=60000) {

  window_breaks <- as.integer(c(0:round(extract_length/window_size)) * window_size)

  data <- data |>
    dplyr::mutate(end = end - min(begin), # reset timestamps to start from 0
           begin = begin - min(begin),
           line = cut(begin,window_breaks,right=F,labels=F)) |>
    tidyr::drop_na(line) |>
    dplyr::group_by(line) |>
    dplyr::mutate(begin0 = begin - min(begin), # reset timestamps to 0 for each new line
           end0 = end - min(begin)) |>
    dplyr::ungroup()

  return(data)
}
