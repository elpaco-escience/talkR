#' Inspect corpus
#'
#' A function that gives us a quick and rich impression of a language or
#' corpus.
#'
#' @param d dataset
#' @param d.tokens tokens dataset
#' @param lang language
#' @param saveplot should the plot be saved
#' @param allsources should all sources be shown
#'
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @import viridis
#' @import tidyr
#' @import ggrepel
#' @import knitr
inspect_corpus <- function(d, d.tokens, lang=NULL,saveplot=F,allsources=F) {

  dp <- d |> dplyr::filter(language == lang)
  ntransitions <- dp |> drop_na(FTO) |> ungroup() |> summarize(n=n()) |> as.integer()

  if(ntransitions > 1) {

    pA <- dp |>
      ggplot(aes(FTO)) +
      theme_tufte() +
      ggtitle(paste0('Transitions (n=',ntransitions,')')) +
      geom_density(na.rm=T,linewidth=1) +
      #  xlim(-5000,5000) +
      geom_vline(xintercept = 0,colour="#cccccc")

  } else {
    pA <- dp |>
      ggplot(aes(FTO)) +
      theme_tufte() +
      ggtitle(paste0('Transitions (n=',ntransitions,')'),
              subtitle="Not enough transitions to show density plot") +
      #geom_density(na.rm=T,size=1) +
      #  xlim(-5000,5000) +
      geom_vline(xintercept = 0,colour="#cccccc")

  }

  nannotations <- dp |> summarize(n=n()) |> as.integer()

  pB <- dp |>
    ggplot(aes(FTO,duration)) +
    theme_tufte() +
    ggtitle(paste0('...by duration')) +
    geom_point(alpha=0.1,na.rm=T) +
    geom_vline(xintercept = 0,colour="#cccccc")

  dt <- d.tokens |> dplyr::filter(language==lang)
  nwords <- dt$total[1]

  pC <- dt |>
    ggplot(aes(rank,n)) +
    theme_tufte() + theme(legend.position="none") +
    ggtitle(paste0('Top 10 tokenised words (n=',nwords,')')) +
    scale_x_log10() +
    scale_y_log10() +
    geom_line(na.rm=T,alpha=0.5,linewidth=1) +
    geom_text_repel(data = . %>% ungroup() %>% slice(1:10),
                    aes(label=word),
                    segment.alpha=0.2,
                    direction="y",nudge_y = -0.2,size=3,
                    max.overlaps=Inf)

  panel <- cowplot::plot_grid(pA,pB,pC,labels=c("A","B","C"),rel_widths = c(1,1,2),nrow=1)
  print(panel)
  cat("\n")

  if(saveplot) {
    filename <- paste0('qc-lrec-panel-',lang,'.png')
    ggsave(filename,bg="white",width=2400,height=1200,units="px")
  }

  bysource <- dp |> group_by(source) |>
    mutate(translation = ifelse(is.na(translation),0,1)) |>
    summarize(start=min.na(begin),finish=max.na(end),
              turns=n_distinct(uid),
              translated=round(sum(translation)/turns,2),
              words=sum(nwords,na.rm=T),
              people=n_distinct(participant),
              talktime = sum(duration,na.rm=T),
              totaltime = finish - start,
              notiming = sum(is.na(duration)),
              useless = ifelse(notiming==turns,1,0),
              talkprop = round(talktime / totaltime,1),
              minutes = round((totaltime/1000 / 60),1),
              hours = round((totaltime/1000) / 3600,2)) |>
    mutate(hours = ifelse(hours > 0,hours,0),
           totaltime = ifelse(totaltime > 0,totaltime,0))

  useless_sources <- bysource[bysource$useless == 1,]$source

  bylanguage <- bysource |>
    summarize(turns = sum(turns),
              translated=round(mean.na(translated),2),
              words = sum(words),
              mean.duration=round(mean.na(sum(talktime)/turns)),
              talkprop = round(mean.na(talkprop),2),
              people = n_distinct(dp$participant),
              hours = round(sum(hours),2),
              turns_per_h = round(turns/hours)) |>
    arrange(desc(hours))


  cat("\n")
  cat("\n")
  nhours <- round(bylanguage$hours,1)
  cat("### ",nhours,"hours")
  print(kable(bylanguage,label=lang))

  cat("\n")
  cat("\n")
  nature <- dp |> group_by(nature) |> summarise(n=n())
  cat("### annotation types")
  print(kable(nature))

  cat("\n")
  cat("### samples")
  cat("\n")

  if(max.na(dp$participants) > 1) {

    dp <- dp |> dplyr::filter(source %notin% useless_sources)
    uids <- NA #sample(dp[dp$participants=="2",]$uid,3) # removed this because of error

    if (sum(is.na(uids)) == length(uids)) {
      cat("\n","Random sample didn't catch dyads; perhaps check if moving window averages are present.")
      pconv <- convplot(data=dp, lang=lang,before=10000,after=0,verbose=F,printuids=F,datamode=T,dyads=T)

    } else {

      pconv <- convplot(data=dp, uids,before=10000,after=0,verbose=F,printuids=F,datamode=T,dyads=T)

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
    print(kable(bysource |> select(-start,-finish,-talktime,-totaltime,-useless)))
  } else {
    if(nsources > 10) {
      cat("\n")
      cat("Showing only the first 10 sources; use `allsources=T` to show all")
    }
    print(kable(bysource |> select(-start,-finish,-talktime,-totaltime,-useless) |> slice(1:10)))
  }


  cat("\n")

}

