#' Report summaries on data
#'
#' @param data dataset
#' @param lang language
#' @param allsources print all sources
#'
#' @export
report_summaries <- function(data, lang, allsources){
  bysource <- summarize_language_data(data, lang)
  bylanguage <- summarize_source_data(data, lang)

  nhours <- round(bylanguage$hours,1)
  nature <- data |>
    dplyr::group_by(nature) |>
    dplyr::summarise(n=dplyr::n())

  # To command line
  cat("\n")
  cat("\n")
  nhours <- round(bylanguage$hours,1)
  cat("### ",nhours,"hours")
  print(knitr::kable(bylanguage,label=lang))

  cat("\n")
  cat("\n")
  cat("### nature")
  print(knitr::kable(nature))

  cat("\n")
  nsources <- length(unique(bysource$source))
  cat("### ",nsources,"sources")

  if(allsources) {
    print(knitr::kable(bysource |>
                         dplyr::select(-"start",
                                       -"finish",
                                       -"talktime",
                                       -"totaltime")))
  } else {
    if(nsources > 10) {
      cat("\n")
      cat("Showing only the first 10 sources; use `allsources=T` to show all")
    }
    print(knitr::kable(bysource |>
                         dplyr::select(-"start",
                                       -"finish",
                                       -"talktime",
                                       -"totaltime") |>
                         dplyr::slice(1:10)))
  }
}


summarize_language_data <- function(data, lang){
  if(!"translation" %in% colnames(data)){
    data$translation <- NA
  }

  data |>
    dplyr::filter(.data$language == lang) |>
    dplyr::group_by(.data$source) |>
    dplyr::mutate(translation = ifelse(is.na(.data$translation),0,1)) |>
    dplyr::summarize(start=min.na(.data$begin),finish=max.na(.data$end),
                     turns=dplyr::n_distinct(.data$uid),
                     translated=round(sum(.data$translation)/.data$turns,2),
                     words=sum(.data$nwords,na.rm=T),
                     people=dplyr::n_distinct(.data$participant),
                     talktime = sum(.data$duration),
                     totaltime = .data$finish - .data$start,
                     talkprop = round(.data$talktime / .data$totaltime,1),
                     minutes = round((.data$totaltime/1000 / 60),1),
                     hours = round((.data$totaltime/1000) / 3600,2))
}


summarize_source_data <- function(data, lang){
  data |>
    summarize_language_data(lang=lang) |> #TODO this uses another function?
    dplyr::summarize(turns = sum(.data$turns),
                     translated=round(mean.na(.data$translated),2),
                     words = sum(.data$words),
                     turnduration=round(mean.na(sum(.data$talktime)/.data$turns)),
                     talkprop = round(mean.na(.data$talkprop),2),
                     people = dplyr::n_distinct(data$participant),
                     hours = round(sum(.data$hours),2),
                     turns_per_h = round(.data$turns/.data$hours)) |>
    dplyr::arrange(desc(.data$hours))
}
