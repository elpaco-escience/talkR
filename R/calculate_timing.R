#' Calculate conversation properties
#'
#' A dataframe is generated with conversation properties related to timing.
#' This data is made for quality control purposes only, and does not contain
#' sophisticated transition calculation methods. For this, we refer to the
#' python package `scikit-talk`.
#'
#'
#' @param data talkr data frame
#'
#' @return data frame containing the UIDs and calculated columns turn_duration, transition_time
#' @export
calculate_timing <- function(data) {
  check_talkr(data)

  # calculate time difference with previous turn by other
  data$transition_time <- transition_time(data)

  # calculate duration of turn
  data$turn_duration <- data$end - data$begin

  # generate data frame to return
  return(data.frame(uid = data$uid,
                    turn_duration = data$turn_duration,
                    transition_time = data$transition_time))
}


prior_by <- function(data) {
  df <- data.frame(source = data$source, participant = data$participant)
  df$prior_source <- dplyr::lag(df$source, 1)
  df$prior_participant <- dplyr::lag(df$participant, 1)

  return(ifelse(df$prior_source == df$source &
                  df$prior_participant == df$participant,
                "self", "other")
         )
}

transition_time <- function(data) {
  data$prior_by <- prior_by(data)
  data$prior_source <- dplyr::lag(data$source, 1)
  data$transition_time <- data$begin - dplyr::lag(data$end, 1)

  return(ifelse(data$prior_source == data$source &
                  data$prior_by == "other",
                data$transition_time, NA))
}
