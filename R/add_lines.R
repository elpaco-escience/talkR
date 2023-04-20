#' Add information for line-by-line visualization
#'
#' This function adds columns to the dataset that adds a line ID, and a `begin_line` and
#' `end_line` column for timestamps relative to the beginning of the line, so data can
#' be visualized line-by-line.
#'
#' @param data dataset to divide into lines
#' @param line_duration length of line in ms
#'
#' @return data set with added columns `line`, `begin_line`, `end_line`
#' @export
#'
add_lines <- function(data,line_duration=60000) {
  
  # total length of the selected data
  extract_begin <- min(data$begin,na.rm=T)
  extract_end <- end(data$end,na.rm=T) 
  extract_length <- extract_end - extract_begin

  # define line breaks as a range of multiples of line_duration that starts from extract_begin
  line_breaks <- as.integer(c(extract_begin:round(extract_length/line_duration)) * line_duration)

  # add line numbers
  data <- data |>
    dplyr::mutate(line = cut(begin,line_breaks,right=F,labels=F)) |>
    tidyr::drop_na(line) |> # TODO: do we need this and why?
    #group by line and reset timestamps to start at 0 for each new line
    dplyr::group_by(line) |>
    dplyr::mutate(begin_line = begin - min(begin),
           end_line = end - min(begin)) |>
    dplyr::ungroup()

  return(data)
}
