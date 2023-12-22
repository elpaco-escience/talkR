#' Initialize a `talkr` dataset
#'
#' From a dataframe object, generate a talkr dataset.
#' This dataset contains columns that are used throughout the talkr
#' infrastructure to visualize conversations and language corpora.
#' Initializing a talkr dataset is the first step in the talkr workflow.
#'
#' @param data A dataframe object
#' @param source The column name identifying the conversation source (e.g. a filename; is used as unique conversation ID)
#' @param begin The column name with the begin time of the utterance (in milliseconds)
#' @param end The column name with the end time of the utterance (in milliseconds)
#' @param participant The column name with the participant who produced the utterance
#' @param utterance The column name with the utterance itself
#' @param format_timestamps The format of the timestamps in the begin and end columns. Default is "ms", which expects milliseconds. `\%H:\%M:\%OS` will format eg. 00:00:00.010 to milliseconds (10). See `?strptime` for more format examples.
#'
#' @return A dataframe object with columns needed for the talkr workflow
#' @export
init <- function(data,
                 source = "source",
                 begin = "begin",
                 end = "end",
                 participant = "participant",
                 utterance = "utterance",
                 format_timestamps = "ms"){

  # verify that column names declared actually exist in the dataset
  names_required <- c(source, begin, end, participant, utterance)
  check_columns(data, names_required)

  data <- data |>
    dplyr::rename(source = tidyselect::all_of(source),
                  begin = tidyselect::all_of(begin),
                  end = tidyselect::all_of(end),
                  participant = tidyselect::all_of(participant),
                  utterance = tidyselect::all_of(utterance))

  # convert timestamps if necessary
  if (format_timestamps != "ms"){
    data$begin <- timestamp_to_milliseconds(data$begin, format = format_timestamps)
    data$end <- timestamp_to_milliseconds(data$end, format = format_timestamps)
  } else {
    data$begin <- as.numeric(data$begin)
    data$end <- as.numeric(data$end)
  }

  # generate UIDs
  if("uid" %in% names(data)){
    warning("Column 'uid' already exists in the dataset. This column will be renamed to `original_uid`.")
    data$original_uid <- data$uid
  }
  data$uid <- generate_uid(data$source, data$begin)

  return(data)
}


timestamp_to_milliseconds <- function(timestamp, format = "%H:%M:%OS"){
  timestamp <- strptime(timestamp, format = format)
  seconds <- as.numeric(strftime(timestamp, format = "%OS3"))
  minutes <- as.numeric(strftime(timestamp, format = "%M"))
  hours <- as.numeric(strftime(timestamp, format = "%H"))
  time_in_ms <- (seconds + minutes*60 + hours*60*60) * 1000
  return(time_in_ms)
}


generate_uid <- function(source, begin){
  sourcecount <- stats::ave(seq_along(source), source, FUN = seq_along)
  sourcecount <- sprintf("%04d", sourcecount)
  sourceclean <- gsub("[./]", "", source)
  uid <- paste(sourceclean, sourcecount, begin, sep = "-")
  return(uid)
}
