#' convplot
#'
#' Note: slightly more up to date forks in convplot-dev repository
#' https://github.com/mdingemanse/convoplot-dev
#'
#' @param data dataset
#' @param uids        set of uids to plot (optional; if omitted, n uids are sampled)
#' @param lang        language from which to sample uids (if not supplied)
#' @param n_uid           number of uids to sample
#' @param window      time window in ms (optional; if supplied, window will be split into before and after)
#' @param before      stretch to include before selected turn (default: 10000ms, unless `window` is supplied)
#' @param after       stretch to include after selected turn (default: 0, unless `window` is supplied)
#' @param printuids   print the sampled uids
#' @param verbose   print language and information about selected uids
#' @param dyads     if TRUE, select only dyadic interactions for display
#' @param content   if TRUE, render annotation content (EXPERIMENTAL)
#' @param highlight if TRUE, highlight the uid in focus in the plot
#' @param center    if TRUE, center the plot around the uid in focus
#' @param datamode  if TRUE, outputs dataframe instead of plot, for more advanced plotting
#' @param alldata   if TRUE, output all data, not just the selected dyads
#' @param debug     if TRUE, print the selected data and some other diagnostics
#'
#' @export
#'
#' @import ggplot2
#' @import viridis
convplot <- function(data, uids=NULL,lang=NULL,n_uid=10,
                     window=NULL,before=10000,after=10000,
                     printuids=T,verbose=T,
                     dyads=F,content=F,highlight=F,center=F,
                     datamode=F,alldata=F,debug=F) {

  if(!is.null(window)) {
    before <- window / 2
    after <- window / 2
  }

  #TODO the arguments n_uid and uids need to be one; they overwrite each other
  #TODO should lang ever be NULL?
  if(is.null(uids)){
    if(verbose){
        print(paste("No uids given, sampling", n_uid, "random ones from", lang))
      }
    uids <- get_uids(data, lang, n_uid)
  } else{
    n_uid <- length(uids)
  }


  # print uids when asked
  if(printuids) { dput(sort(uids)) } #TODO why not print?

  # get uid metadata and filter uids that fall in the same window
  theseuids <- get_uid_metadata(data, uids, before, after)

  # create slim df
  extracts <- data[data$source %in% theseuids$source,]
  extracts <- extracts |>
    dplyr::arrange(source,begin) |>
    dplyr::group_by(source) |>
    dplyr::mutate(focus = ifelse(uid %in% uids,"focus",NA),
           scope = NA)

  # set scope (= the uid for which the other turns form the sequential context)
  for (thisuid in theseuids$uid) {
    extracts$scope <- ifelse(extracts$source %in% theseuids[theseuids$uid == thisuid,]$source &
                               extracts$begin >= theseuids[theseuids$uid == thisuid,]$begin - before &
                               extracts$end < theseuids[theseuids$uid == thisuid,]$end + after,thisuid,extracts$scope)
  }

  # drop turns outside scope, add useful metadata, compute relative times for each scope
  extracts <- extracts |>
    tidyr::drop_na(scope) |>
    dplyr::group_by(scope) |>
    dplyr::mutate(participant_int = as.integer(as.factor(participant))) |>
    dplyr::mutate(begin0 = begin - min(begin),
           end0 = end - min(begin),
           participation = ifelse(dplyr::n_distinct(participant) < 3,"dyadic","multiparty"))
  nconv <- length(unique(extracts$scope))

  extracts.dyadic <- extracts |> dplyr::filter(participation == "dyadic")
  ndyads <- length(unique(extracts.dyadic$scope))

  if(verbose) {
    print(paste('seeing',ndyads,'dyads in ',n_uid,'non-overlapping extracts'))
  }

  if (debug) {

    #print(dyads)
    dput(uids)
    return(extracts)
  }

  if (datamode) {

    if(alldata) { return(extracts) }

    return(extracts.dyadic)

  } else {

    if(dyads) { extracts <- extracts.dyadic }

    p <- extracts |>
      dplyr::mutate(striplength = dplyr::case_when(duration < 300 ~ 3,
                                     duration >= 300 ~ round(duration/90)),
             uttshort = ifelse(nchar <= striplength | nchar <= 4,
                               utterance,
                               paste0(stringx::strtrim(utterance,striplength),'~'))) |>
      ggplot(aes(y=participant_int)) +
      ggthemes::theme_tufte() + theme(legend.position = "none",
                            strip.placement = "outside",
                            strip.text = element_text(hjust=0,color="grey50")) +
      ylab("") + xlab("time (ms)") +
      scale_y_continuous(breaks=c(1:max(extracts$participant_int)),
                         labels=rev(LETTERS[1:max(extracts$participant_int)])) +
      theme(axis.ticks.y = element_blank()) +
      geom_rect(aes(xmin=begin0,xmax=end0,ymin=participant_int-0.4,ymax=participant_int+0.4),
                #linewidth=1,
                fill="grey90",color="white")

    if(highlight) {
      p <- p + geom_rect(data=extracts |> filter(focus == "focus"),
                         aes(xmin=begin0,xmax=end0,
                             ymin=participant_int-0.4,ymax=participant_int+0.4),
                         #linewidth=1,
                         fill="red",color="white")
    }
    if(content) {
      p <- p + geom_text(aes(label=uttshort,x=begin0+60),
                         color="black",hjust=0,size=3)
    }

    p <- p + facet_wrap(~ scope, ncol=1)

    return(p)

  }
}


get_uids <- function(data, lang=NULL, n_uids=10){
  if(!is.null(lang)){
    data <- dplyr::filter(data, language==lang)
  }
  uids <- sample(unique(data$uid),n_uids)
  return(uids)
}

get_uid_metadata <- function(data, uids, before, after){
  uid_data <- data[data$uid %in% uids, names(data) %in% c("uid","source","begin","end")]
  uid_data <- uid_data |>
    dplyr::arrange(source, begin) |>
    dplyr::group_by(source) |>
    dplyr::mutate(distance = begin - dplyr::lag(begin)) |> #TODO what lag function is meant here?
    dplyr::filter(is.na(distance) | distance > before + after)
  return(uid_data)
}
