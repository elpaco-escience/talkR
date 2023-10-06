#' Report summaries on data
#'
#' @param data dataset
#' @param lang language
#' @param allsources print all sources
#'
#' @export
report_summaries <- function(data, lang = NA, allsources = FALSE){
  # select the requested language
  if(!is.na(lang)){
    data <- data |>
      dplyr::filter(.data$language==lang)
  }
  # ensure a translation column is present
  if(!"translation" %in% colnames(data)){
    data$translation <- NA
  }

  bylanguage <- summarize_stats(data)
  bysource <- summarize_bysource(data)

  ## Overall summary of the utterances
  nhours <- round(bylanguage$hours,1)
  language_header <- paste(nhours,"hours")
  print_summary(header = language_header, table = bylanguage)

  ## Summarize nature of utterances
  nature <- summarize_nature(data)
  print_summary(header = "nature", table = nature)

  ## Summarize by source
  nsources <- length(unique(bysource$source))
  source_header <- paste(nsources,"sources")

  bysource <- bysource |>
    dplyr::select(-"start",
                  -"finish",
                  -"talktime",
                  -"totaltime")
  if(!allsources) {
    bysource <- bysource |>
      dplyr::slice(1:10)
    cat("\n")
    cat("Showing only the first 10 sources; use `allsources=T` to show all")
    cat("\n")
  }

  print_summary(header = source_header, table = bysource)
}

summarize_bysource <- function(data){
  summary <- data |>
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


summarize_stats <- function(data){
  summary <- data |>
    summarize_bysource() |>
    dplyr::summarize(turns = sum(.data$turns),
                     translated=round(mean.na(.data$translated),2),
                     words = sum(.data$words),
                     turnduration=round(mean.na(sum(.data$talktime)/.data$turns)),
                     talkprop = round(mean.na(.data$talkprop),2),
                     people = dplyr::n_distinct(data$participant),
                     hours = round(sum(.data$hours),2),
                     turns_per_h = round(.data$turns/.data$hours)) |>
    dplyr::arrange(desc(.data$hours))
  return(summary)
}


summarize_nature <- function(data){
  summary <- data |>
    dplyr::group_by(nature) |>
    dplyr::summarise(n=dplyr::n())
  return(summary)
}

print_summary <- function(header, table){
  cat("\n")
  cat(paste("###", header))
  print(knitr::kable(table))
  cat("\n")
}
