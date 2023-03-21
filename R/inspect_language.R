#' Inspect language
#'
#' @param data dataset
#' @param lang language
#' @param saveplot should the plot be saved
#' @param allsources all sources
#'
#' @export
inspect_language <- function(data,
                             lang=NULL,
                             saveplot=FALSE,
                             allsources=FALSE) {
  d <- data

  dp <- d |>
    dplyr::filter(language == lang)

  nturns <- sum(!is.na(dp$FTO)) # QUESTION: does this make sense?

  pA <- plot_transitions(data=dp, nturns=nturns)
  pB <- plot_FTO(data=dp)
  pC <- plot_turn_duration(data=dp)
  pD <- plot_top_turn_types(data=dp)



  return(list(pA, pB, pC, pD))
}


  # pD <- dp |>
  #   arrange(desc(n)) |>
  #   group_by(rank) |>
  #   slice(1) |>
  #   ggplot(aes(rank,n)) +
  #   theme_tufte() + theme(legend.position="none") +
  #   ggtitle(paste0('Top turn types (of ',turntypes,')')) +
  #   scale_x_log10() +
  #   scale_y_log10() +
  #   geom_line(na.rm=T,alpha=0.5,linewidth=1) +
  #   geom_point(alpha=0.5,na.rm=T,size=1.2) +
  #   geom_text_repel(data = . |> ungroup() |> slice(1:10),
  #                   aes(label=utterance_stripped),
  #                   segment.alpha=0.2,
  #                   direction="y",nudge_y = -0.2,size=3,
  #                   max.overlaps=Inf)
  #
  # dt <- d.tokens |> filter(language==lang)
  # nwords <- dt$total[1]
  #
  # pE <- dt |>
  #   ggplot(aes(rank,n)) +
  #   theme_tufte() + theme(legend.position="none") +
  #   ggtitle(paste0('Token rank & frequency (',nwords,' words)')) +
  #   scale_x_log10() +
  #   scale_y_log10() +
  #   geom_line(na.rm=T,alpha=0.5,size=1) +
  #   geom_text_repel(data = . |> ungroup() |> slice(1:10),
  #                   aes(label=word),
  #                   segment.alpha=0.2,
  #                   direction="y",nudge_y = -0.2,size=3,
  #                   max.overlaps=Inf)
  #
  #
  # top_row <- plot_grid(pA,pB,pC,labels=c("A","B","C"),rel_widths = c(1,1,1),nrow=1)
  # bottom_row <- plot_grid(pD,pE,labels=c("D","E"),rel_widths = c(1,1),nrow=1)
  # panel <- plot_grid(top_row,bottom_row,ncol=1)
  # print(panel)
  # cat("\n")
  #
  # if(saveplot) {
  #   filename <- paste0('qc-panel-',lang,'.png')
  #   ggsave(filename,bg="white",width=2400,height=1200,units="px")
  # }
  #
  # bysource <- dp |> group_by(source) |>
  #   mutate(translation = ifelse(is.na(translation),0,1)) |>
  #   summarize(start=min.na(begin),finish=max.na(end),
  #             turns=n_distinct(uid),
  #             translated=round(sum(translation)/turns,2),
  #             words=sum(nwords,na.rm=T),
  #             people=n_distinct(participant),
  #             talktime = sum(duration),
  #             totaltime = finish - start,
  #             talkprop = round(talktime / totaltime,1),
  #             minutes = round((totaltime/1000 / 60),1),
  #             hours = round((totaltime/1000) / 3600,2))
  #
  # bylanguage <- bysource |>
  #   summarize(turns = sum(turns),
  #             translated=round(mean.na(translated),2),
  #             words = sum(words),
  #             turnduration=round(mean.na(sum(talktime)/turns)),
  #             talkprop = round(mean.na(talkprop),2),
  #             people = n_distinct(dp$participant),
  #             hours = round(sum(hours),2),
  #             turns_per_h = round(turns/hours)) |>
  #   arrange(desc(hours))
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
  # if(max.na(dp$participants) > 1) {
  #
  #   uids <- sample(dp[dp$participants=="2",]$uid,7)
  #
  #   if (sum(is.na(uids)) == length(uids)) {
  #     cat("\n","Random sample didn't catch dyads; perhaps check if moving window averages are present.")
  #     pconv <- convplot(lang=lang,before=10000,after=0,verbose=F,printuids=F,datamode=T,dyads=T)
  #
  #   } else {
  #
  #     pconv <- convplot(uids,before=10000,after=0,verbose=F,printuids=F,datamode=T,dyads=T)
  #
  #   }
  #
  #   pconv <- pconv |>
  #     mutate(striplength = case_when(duration < 300 ~ 3,
  #                                    duration >= 300 ~ round(duration/90)),
  #            uttshort = ifelse(nchar <= striplength | nchar <= 4,
  #                              utterance,
  #                              paste0(stringx::strtrim(utterance,striplength),'~'))) |>
  #     ggplot(aes(y=participant_int)) +
  #     theme_tufte() + ylab("") + xlab("time (ms)") +
  #     theme(axis.ticks.y = element_blank(),
  #           strip.placement = "outside",
  #           strip.text.x = element_text(hjust = 0)) +
  #     scale_fill_viridis(option="plasma",direction=1) +
  #     scale_y_continuous(breaks=c(1:2),
  #                        labels=rev(LETTERS[1:2])) +
  #     geom_rect(aes(xmin=begin0,xmax=end0,ymin=participant_int-0.4,ymax=participant_int+0.4),
  #               linewidth=1,fill="grey90",color="white") +
  #     geom_text(aes(label=uttshort,x=begin0+60),
  #               color="black",hjust=0,size=3,na.rm=T) +
  #     facet_wrap(~ scope, ncol=1)
  #
  #   print(pconv)
  #   cat("\n")
  #
  #   # } else {
  #   #   cat("Sample did not yield enough conversations with >1 participants in this language.")
  #   #   cat("\n")
  # }
  #
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

#}


plot_transitions <- function(data, nturns){
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(FTO)) +
    ggthemes::theme_tufte() +
    ggplot2::ggtitle(paste0(nturns,' transitions')) +
    ggplot2::geom_density(na.rm=T) +
    #  xlim(-5000,5000) +
    ggplot2::geom_vline(xintercept = 0,colour="#cccccc")

  return(p)
}


plot_FTO <- function(data){
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(FTO,duration)) +
    ggthemes::theme_tufte() +
    ggplot2::ggtitle('FTO by turn duration') +
    ggplot2::geom_point(alpha=0.1,na.rm=T) +
    #  ylim(0,20000) +
    #  xlim(-20000,20000) +
    ggplot2::geom_vline(xintercept = 0,colour="#cccccc")

  return(p)
}

plot_turn_duration <- function(data){
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(duration)) +
    ggthemes::theme_tufte() +
    ggplot2::ggtitle('Turn duration') +
    ggplot2::geom_density(na.rm=T)
  return(p)
}

plot_top_turn_types <- function(data){
  turntypes <- dplyr::n_distinct(dp$utterance_stripped)
  p <- data |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::group_by(rank) |>
    dplyr::slice(1) |>
    ggplot2::ggplot(ggplot2::aes(rank,n)) +
    ggthemes::theme_tufte() + ggplot2::theme(legend.position="none") +
    ggplot2::ggtitle(paste0('Top turn types (of ',turntypes,')')) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::geom_line(na.rm=T,alpha=0.5,) + #linewidth=1
    ggplot2::geom_point(alpha=0.5,na.rm=T,size=1.2) +
    ggrepel::geom_text_repel(data = . %>% dplyr::ungroup() %>% dplyr::slice(1:10),
                             ggplot2::aes(label=utterance_stripped),
                             segment.alpha=0.2,
                             direction="y",nudge_y = -0.2,size=3,
                             max.overlaps=Inf)
  return(p)
}



