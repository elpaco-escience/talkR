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
}





  #
  #
  # cat("\n")
  # cat("\n")
  # nhours <- round(bylanguage$hours,1)
  # cat("### ",nhours,"hours")
  # print(kable(bylanguage,label=lang))
  #
  # cat("\n")
  # cat("\n")
  # nature <- dp |> group_by(nature) |> summarise(n=n())
  # cat("### nature")
  # print(kable(nature))
  #
  # cat("\n")
  # cat("### samples")
  # cat("\n")
  #


#}









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
  if(max.na(data$participants) > 1) {

    uids <- sample(data[data$participants=="2",]$uid,7)

    if (sum(is.na(uids)) == length(uids)) {
      cat("\n","Random sample didn't catch dyads; perhaps check if moving window averages are present.")
      pconv <- convplot(data, lang=lang,before=10000,after=0,verbose=F,printuids=F,datamode=T,dyads=T)

    } else {

      pconv <- convplot(data, uids,before=10000,after=0,verbose=F,printuids=F,datamode=T,dyads=T)

    }

    pconv <- pconv |>
      mutate(striplength = case_when(duration < 300 ~ 3,
                                     duration >= 300 ~ round(duration/90)),
             uttshort = ifelse(nchar <= striplength | nchar <= 4,
                               utterance,
                               paste0(stringx::strtrim(utterance,striplength),'~'))) |>
      ggplot(aes(y=participant_int)) +
      theme_tufte() + ylab("") + xlab("time (ms)") +
      theme(axis.ticks.y = element_blank(),
            strip.placement = "outside",
            strip.text.x = element_text(hjust = 0)) +
      scale_fill_viridis(option="plasma",direction=1) +
      scale_y_continuous(breaks=c(1:2),
                         labels=rev(LETTERS[1:2])) +
      geom_rect(aes(xmin=begin0,xmax=end0,ymin=participant_int-0.4,ymax=participant_int+0.4),
                linewidth=1,fill="grey90",color="white") +
      geom_text(aes(label=uttshort,x=begin0+60),
                color="black",hjust=0,size=3,na.rm=T) +
      facet_wrap(~ scope, ncol=1)

    print(pconv)
    cat("\n")

    # } else {
    #   cat("Sample did not yield enough conversations with >1 participants in this language.")
    #   cat("\n")
  }

  cat("\n")
  nsources <- length(unique(bysource$source))
  cat("### ",nsources,"sources")

  if(allsources) {
    print(kable(bysource |> select(-start,-finish,-talktime,-totaltime)))
  } else {
    if(nsources > 10) {
      cat("\n")
      cat("Showing only the first 10 sources; use `allsources=T` to show all")
    }
    print(kable(bysource |> select(-start,-finish,-talktime,-totaltime) |> slice(1:10)))
  }


  cat("\n")
}

