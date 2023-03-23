# ElPaCo conversation & visualization functions




# helper functions


# Inspect corpus ----------------------------------------------------------

# A function that gives us a quick and rich impression of a language or
# corpus.





# Inspect corpus ----------------------------------------------------------

# A function that gives us a quick and rich impression of a language or
# corpus.

inspect_corpus <- function(lang=NULL,saveplot=F,allsources=F) {

  dp <- d |> filter(language == lang)
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

  dt <- d.tokens |> filter(language==lang)
  nwords <- dt$total[1]

  pC <- dt |>
    ggplot(aes(rank,n)) +
    theme_tufte() + theme(legend.position="none") +
    ggtitle(paste0('Top 10 tokenised words (n=',nwords,')')) +
    scale_x_log10() +
    scale_y_log10() +
    geom_line(na.rm=T,alpha=0.5,linewidth=1) +
    geom_text_repel(data = . |> ungroup() |> slice(1:10),
                    aes(label=word),
                    segment.alpha=0.2,
                    direction="y",nudge_y = -0.2,size=3,
                    max.overlaps=Inf)

  panel <- plot_grid(pA,pB,pC,labels=c("A","B","C"),rel_widths = c(1,1,2),nrow=1)
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

    dp <- dp |> filter(source %notin% useless_sources)
    uids <- sample(dp[dp$participants=="2",]$uid,3)

    if (sum(is.na(uids)) == length(uids)) {
      cat("\n","Random sample didn't catch dyads; perhaps check if moving window averages are present.")
      pconv <- convplot(lang=lang,before=10000,after=0,verbose=F,printuids=F,datamode=T,dyads=T)

    } else {

      pconv <- convplot(uids,before=10000,after=0,verbose=F,printuids=F,datamode=T,dyads=T)

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


# # convplot ----------------------------------------------------------------
#
# # Note: slightly more up to date forks in convplot-dev repository
# # https://github.com/mdingemanse/convoplot-dev
#
# # options:
#
# # uids        set of uids to plot (optional; if omitted, n uids are sampled)
# # lang        language from which to sample uids (if not supplied)
# # n           number of uids to sample
# # window      time window in ms (optional; if supplied, window will be split into before and after)
# # before      stretch to include before selected turn (default: 10000ms, unless `window` is supplied)
# # after       stretch to include after selected turn (default: 0, unless `window` is supplied)
#
# # printuids=T print the sampled uids
# # verbose=T   print language and information about selected uids
#
# # dyads=F     if TRUE, select only dyadic interactions for display
# # content=F   if TRUE, render annotation content (EXPERIMENTAL)
# # highlight=F if TRUE, highlight the uid in focus in the plot
# # center=F    if TRUE, center the plot around the uid in focus
#
# # datamode=F  if TRUE, outputs dataframe instead of plot, for more advanced plotting
# # alldata=F   if TRUE, output all data, not just the selected dyads
# # debug=F     if TRUE, print the selected data and some other diagnostics
# convplot <- function(uids=NULL,lang=NULL,n=10,
#                      window=NULL,before=10000,after=10000,
#                      printuids=T,verbose=T,
#                      dyads=F,content=F,highlight=F,center=F,
#                      datamode=F,alldata=F,debug=F) {
#
#   if(!is.null(window)) {
#     before <- window / 2
#     after <- window / 2
#   }
#
#   if(!is.null(uids)) {
#     n <- length(uids)
#   } else {
#     if(verbose) {
#       print(paste('No uids given, sampling',n,'random ones'))
#     }
#     if(!is.null(lang)) {
#       if(verbose) {
#         print(paste('...from',lang))
#       }
#       d.lg <- d |> filter(language %in% lang)
#       uids <- sample(unique(d.lg$uid),n)
#
#     } else {
#       uids <- sample(unique(d$uid),n)
#     }
#
#   }
#
#   # print uids when asked
#   if(printuids) { dput(sort(uids)) }
#
#   # get uid metadata and filter uids that fall in the same window
#   theseuids <- finduid(uids) |> arrange(source,begin)
#   theseuids |> group_by(source) |>
#     mutate(distance = begin - lag(begin)) |>
#     filter(is.na(distance) | distance > before + after)
#
#   # create slim df
#   extracts <- d[d$source %in% theseuids$source,]
#   extracts <- extracts |>
#     arrange(source,begin) |>
#     group_by(source) |>
#     mutate(focus = ifelse(uid %in% uids,"focus",NA),
#            scope = NA)
#
#   # set scope (= the uid for which the other turns form the sequential context)
#   for (thisuid in theseuids$uid) {
#     extracts$scope <- ifelse(extracts$source %in% theseuids[theseuids$uid == thisuid,]$source &
#                                extracts$begin >= theseuids[theseuids$uid == thisuid,]$begin - before &
#                                extracts$end < theseuids[theseuids$uid == thisuid,]$end + after,thisuid,extracts$scope)
#   }
#
#   # drop turns outside scope, add useful metadata, compute relative times for each scope
#   extracts <- extracts |>
#     drop_na(scope) |>
#     group_by(scope) |>
#     mutate(participant_int = as.integer(as.factor(participant))) |>
#     mutate(begin0 = begin - min(begin),
#            end0 = end - min(begin),
#            participation = ifelse(n_distinct(participant) < 3,"dyadic","multiparty"))
#   nconv <- length(unique(extracts$scope))
#
#   extracts.dyadic <- extracts |> filter(participation == "dyadic")
#   ndyads <- length(unique(extracts.dyadic$scope))
#
#   if(verbose) {
#     print(paste('seeing',ndyads,'dyads in ',n,'non-overlapping extracts'))
#   }
#
#   if (debug) {
#
#     #print(dyads)
#     dput(uids)
#     return(extracts)
#   }
#
#   if (datamode) {
#
#     if(alldata) { return(extracts) }
#
#     return(extracts.dyadic)
#
#   } else {
#
#     if(dyads) { extracts <- extracts.dyadic }
#
#     p <- extracts |>
#       mutate(striplength = case_when(duration < 300 ~ 3,
#                                      duration >= 300 ~ round(duration/90)),
#              uttshort = ifelse(nchar <= striplength | nchar <= 4,
#                                utterance,
#                                paste0(stringx::strtrim(utterance,striplength),'~'))) |>
#       ggplot(aes(y=participant_int)) +
#       theme_tufte() + theme(legend.position = "none",
#                             strip.placement = "outside",
#                             strip.text = element_text(hjust=0,color="grey50")) +
#       ylab("") + xlab("time (ms)") +
#       scale_y_continuous(breaks=c(1:max(extracts$participant_int)),
#                          labels=rev(LETTERS[1:max(extracts$participant_int)])) +
#       theme(axis.ticks.y = element_blank()) +
#       geom_rect(aes(xmin=begin0,xmax=end0,ymin=participant_int-0.4,ymax=participant_int+0.4),
#                 linewidth=1,fill="grey90",color="white")
#
#     if(highlight) {
#       p <- p + geom_rect(data=extracts |> filter(focus == "focus"),
#                          aes(xmin=begin0,xmax=end0,
#                              ymin=participant_int-0.4,ymax=participant_int+0.4),
#                          linewidth=1,fill="red",color="white")
#     }
#     if(content) {
#       p <- p + geom_text(aes(label=uttshort,x=begin0+60),
#                          color="black",hjust=0,size=3)
#     }
#
#     p <- p + facet_wrap(~ scope, ncol=1)
#
#     return(p)
#
#   }
# }
