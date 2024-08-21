#' Add information for line-by-line visualization
#'
#' This function adds columns to the dataset that adds a line ID, and changes columns
#' with timestamps relative to the beginning of the line, so data can
#' be visualized line-by-line.
#' The participant column is also adjusted to create a Y-coordinate for each speaker.
#' The line duration is set to 60 seconds by default.
#'
#' This transformation can be done for multiple columns with time-stamped data.
#' Use the `time_columns` argument to supply the names of one or more columns
#' that should be transformed.
#'
#' @param data dataset to divide into lines
#' @param time_columns columns with timestamps that need to be adjusted to line-relative time
#' @param line_duration length of line (in ms)
#'
#' @return data set with added columns: `line_id`, `line_participant`, and
#'        `line_column` for every column in `time_columns`
#' @export
#'
add_lines <- function(data, time_columns = c("begin", "end"), line_duration=60000) {

  # define line breaks as a range of multiples of line_duration that starts from extract_begin
  all_times <- data[time_columns]
  line_breaks <- seq(from = min(all_times, na.rm = T),
                     to = max(all_times,na.rm = T) ,
                     by = line_duration)
  line_breaks <- c(line_breaks,max(all_times, na.rm = T) + 1)

  # identify the column with the lowest minimum value
  min_col <- which.min(sapply(data[time_columns], min, na.rm = T))
  min_col <- time_columns[min_col]

  # add line numbers, and reset timestamps to start at 0 for each new line
  data$line_id = cut(data[[min_col]],line_breaks,right=F,labels=F)

  for(col in time_columns){
    new_col <- paste0("line_", col)
    data <- data |>
      dplyr::group_by(.data$line_id) |>
      dplyr::mutate({{new_col}} := .data[[col]] - (.data$line_id-1)*line_duration) |>
      dplyr::ungroup()
  }

  # create Y coordinates for speakers
  data$participant_int <- as.integer(as.factor(data$participant))
  n_participants <- length(unique(data$participant_int)) + 1
  data$line_participant <- (data$participant_int/n_participants) + data$line_id - 0.5

  return(data)
}
