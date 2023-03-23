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

  # sample conversation
  data_convplot <- prepare_convplot(data_conv, lang)
  pconv <- plot_conversation(data_convplot)
  print(pconv)

  # print summary stats
  report_summaries(data_conv, lang, allsources)
}





