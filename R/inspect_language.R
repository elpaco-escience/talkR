#' Inspect language
#'
#' @param data_conv conversation dataset
#' @param data_tokens tokens dataset
#' @param lang language
#' @param returnplots boolean indicating whether plots should be made
#' @param saveplot should the plot be saved
#' @param allsources all sources
#'
#' @export
inspect_language <- function(data_conv,
                             data_tokens = NULL,
                             lang = NULL,
                             returnplots = FALSE,
                             saveplot=FALSE,
                             allsources=FALSE) {
  # conversation data
  dp <- data_conv |>
    dplyr::filter(.data$language == lang)

  nturns <- sum(!is.na(dp$FTO)) # QUESTION: does this make sense?

  pA <- plot_transitions(data=dp, nturns=nturns)
  pB <- plot_FTO(data=dp)
  pC <- plot_turn_duration(data=dp)
  pD <- plot_top_turn_types(data=dp)



  if(!is.null(data_tokens)){
    # token data
    dt <- data_tokens |>
      dplyr::filter(.data$language==lang)
    nwords <- dt$total[1]
    pE <- plot_token_rank(data=dt, nwords)
  }


  # generate plots
  if(returnplots){
    if(!is.null(data_tokens)){
      generate_plots(saveplot, pA, pB, pC, pD, pE)
    } else {
    generate_plots(saveplot, pA, pB, pC, pD, pE = NULL)
    }
  }

  # sample conversation
  data_convplot <- prepare_convplot(data_conv, lang)

  if(returnplots){
    pconv <- plot_conversation(data_convplot)
    print(pconv)

  }

  # print summary stats
  report_stats(data_conv)
}


generate_plots <- function(saveplot = FALSE, pA, pB, pC, pD, pE){
  # combine the plots

    if(is.null(pE)){
    top_row <- cowplot::plot_grid(pA,pB,labels=c("A","B"),rel_widths = c(1,1),nrow=1)
    bottom_row <- cowplot::plot_grid(pC,pD,labels=c("C", "D"),rel_widths = c(1,1),nrow=1)
  } else{
    top_row <- cowplot::plot_grid(pA,pB,pC,labels=c("A","B","C"),rel_widths = c(1,1,1),nrow=1)
    bottom_row <- cowplot::plot_grid(pD,pE,labels=c("D","E"),rel_widths = c(1,1),nrow=1)
  }

  panel <- cowplot::plot_grid(top_row,bottom_row,ncol=1)
  print(panel)

  if(saveplot) {
    filename <- paste0('qc-panel-',lang,'.png')
    ggplot2::ggsave(filename,bg="white",width=2400,height=1200,units="px")
  }
}




