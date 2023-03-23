#' Summarize the data for a specific language
#'
#' @param data dataset
#' @param lang language
#'
#' @export
summarize_language_data <- function(data, lang){
  data |>
    dplyr::filter(language == lang) |>
    dplyr::group_by(source) |>
    dplyr::mutate(translation = ifelse(is.na(translation),0,1)) |>
    dplyr::summarize(start=min.na(begin),finish=max.na(end),
                     turns=dplyr::n_distinct(uid),
                     translated=round(sum(translation)/turns,2),
                     words=sum(nwords,na.rm=T),
                     people=dplyr::n_distinct(participant),
                     talktime = sum(duration),
                     totaltime = finish - start,
                     talkprop = round(talktime / totaltime,1),
                     minutes = round((totaltime/1000 / 60),1),
                     hours = round((totaltime/1000) / 3600,2))
}


#' Summarize source data
#'
#' @param data dataset
#' @param lang language
#'
#' @export
summarize_source_data <- function(data, lang){
  data |>
    summarize_language_data(lang=lang) |> #TODO this uses another function?
    dplyr::summarize(turns = sum(turns),
                     translated=round(mean.na(translated),2),
                     words = sum(words),
                     turnduration=round(mean.na(sum(talktime)/turns)),
                     talkprop = round(mean.na(talkprop),2),
                     people = dplyr::n_distinct(data$participant),
                     hours = round(sum(hours),2),
                     turns_per_h = round(turns/hours)) |>
    dplyr::arrange(desc(hours))
}
