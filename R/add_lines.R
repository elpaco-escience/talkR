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

  # define line breaks as a range of multiples of line_duration that starts from extract_begin
  line_breaks <- seq(from = min(data$begin,na.rm=T),
                     to = max(data$end,na.rm=T) ,
                     by = line_duration)

  # add line numbers
  data <- data |>
    dplyr::mutate(line_id = cut(begin,line_breaks,right=F,labels=F)) |>
    #group by line and reset timestamps to start at 0 for each new line
    dplyr::group_by(line_id) |>
    dplyr::mutate(line_begin = begin - min(begin),
           line_end = end - min(begin)) |>
    dplyr::ungroup()

  # add participant combined with line ID
  data <- data |>
    tidyr::unite("line_participant",c("line_id","participant"),sep="_")

  return(data)
}

