plot_token_rank <- function(data, nwords){
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(rank,n)) +
    ggthemes::theme_tufte() + ggplot2::theme(legend.position="none") +
    ggplot2::ggtitle(paste0('Token rank & frequency (',nwords,' words)')) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::geom_line(na.rm=T,alpha=0.5,size=1) +
    ggrepel::geom_text_repel(data=group_and_slice(data),
                             ggplot2::aes(label=word),
                             segment.alpha=0.2,
                             direction="y",nudge_y = -0.2,size=3,
                             max.overlaps=Inf)
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
  turntypes <- dplyr::n_distinct(data$utterance_stripped)

  data <- process_for_plot(data)

  p <- data |>
    ggplot2::ggplot(ggplot2::aes(rank,n)) +
    ggthemes::theme_tufte() + ggplot2::theme(legend.position="none") +
    ggplot2::ggtitle(paste0('Top turn types (of ',turntypes,')')) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::geom_line(na.rm=T,alpha=0.5,) + #linewidth=1
    ggplot2::geom_point(alpha=0.5,na.rm=T,size=1.2) +
    ggrepel::geom_text_repel(data=group_and_slice(data),
                             ggplot2::aes(label=utterance_stripped),
                             segment.alpha=0.2,
                             direction="y",nudge_y = -0.2,size=3,
                             max.overlaps=Inf)
  return(p)
}

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

# plotting helpers
process_for_plot <- function(data){
  data <- data |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::group_by(rank) |>
    dplyr::slice(1)
  return(data)
}

group_and_slice <- function(data) {
  data <- process_for_plot(data)
  data <- data |>
    dplyr::ungroup() |> dplyr::slice(1:10)
  return(data)
}
