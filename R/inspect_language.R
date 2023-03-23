#' Inspect language
#'
#' @param data_conv conversation dataset
#' @param data_tokens tokens dataset
#' @param lang language
#' @param saveplot should the plot be saved
#' @param allsources all sources
#'
#' @export
inspect_language <- function(data_conv,
                             data_tokens,
                             lang,
                             saveplot=FALSE,
                             allsources=FALSE) {
  # conversation data
  dp <- data_conv |>
    dplyr::filter(language == lang)

  nturns <- sum(!is.na(dp$FTO)) # QUESTION: does this make sense?

  pA <- plot_transitions(data=dp, nturns=nturns)
  pB <- plot_FTO(data=dp)
  pC <- plot_turn_duration(data=dp)
  pD <- plot_top_turn_types(data=dp)

  # token data
  dt <- data_tokens |>
    dplyr::filter(language==lang)
  nwords <- dt$total[1]

  pE <- plot_token_rank(data=dt, nwords)

  # combine the plots
  top_row <- cowplot::plot_grid(pA,pB,pC,labels=c("A","B","C"),rel_widths = c(1,1,1),nrow=1)
  bottom_row <- cowplot::plot_grid(pD,pE,labels=c("D","E"),rel_widths = c(1,1),nrow=1)
  panel <- cowplot::plot_grid(top_row,bottom_row,ncol=1)
  print(panel)

  if(saveplot) {
    filename <- paste0('qc-panel-',lang,'.png')
    ggplot2::ggsave(filename,bg="white",width=2400,height=1200,units="px")
  }

  report_summaries(data_conv, lang)

}











#' Summarize all data
#'
#' @param data dataset
#' @param lang language
#'
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
summarize_all_data <- function(data, lang) {
  data <- dplyr::filter(data, language==lang)

  if(max.na(data$participants) > 1) {

    uids <- sample(data[data$participants=="2",]$uid,7)

    if (sum(is.na(uids)) == length(uids)) {
      cat("\n","Random sample didn't catch dyads; perhaps check if moving window averages are present.")
      pconv <- convplot(data, lang=lang,before=10000,after=0,verbose=F,printuids=F,datamode=T,dyads=T)

    } else {

      pconv <- convplot(data, uids,before=10000,after=0,verbose=F,printuids=F,datamode=T,dyads=T)

    }

    pconv <- plot_conversation(pconv)

    print(pconv)
    cat("\n")

    # } else {
    #   cat("Sample did not yield enough conversations with >1 participants in this language.")
    #   cat("\n")
  }

  # cat("\n")
  # nsources <- length(unique(bysource$source))
  # cat("### ",nsources,"sources")
  #
  # if(allsources) {
  #   print(kable(bysource |> select(-start,-finish,-talktime,-totaltime)))
  # } else {
  #   if(nsources > 10) {
  #     cat("\n")
  #     cat("Showing only the first 10 sources; use `allsources=T` to show all")
  #   }
  #   print(kable(bysource |> select(-start,-finish,-talktime,-totaltime) |> slice(1:10)))
  # }
  #
  #
  # cat("\n")
}

