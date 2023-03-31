#' Conversation data from the IFA Dialog Video (IFADV) corpus
#'
#' This dataset is a processed version of the annotated transcript DVA13U in
#' the IFADV corpus.
#' The original corpus is licensed under the GNU Public License (v2).
#'
#' @format A data frame with 717 rows and 30 variables:
#' \describe{
#'   \item{begin}{timestamp in milliseconds}
#'   \item{end}{timestamp in milliseconds}
#'   \item{participant}{a string identifying the speaker}
#'   \item{utterance}{the spoken text}
#'   \item{source}{file of origin}
#'   \item{utterance_raw}{the raw spoken text}
#'   \item{language}{a string specifying the language}
#'   \item{corpus}{a string specifying the corpus, "dutch2"}
#'   \item{duration}{length of the utterance in milliseconds}
#'   \item{uid}{a string uniquely identifying the utterance}
#'   \item{nature}{the nature of the utterance ("talk" or "laugh")}
#'   \item{utterance_stripped}{the spoken text stripped of special characters}
#'   \item{nwords}{number of words in the utterance}
#'   \item{nchar}{number of characters in the utterance}
#'   \item{n}{}
#'   \item{rank}{}
#'   \item{freq}{}
#'   \item{overlap}{}
#'   \item{priorby}{a string specifying the speaker of the previous utterance ("self", "other", or "self_during_other" in case of overlap)}
#'   \item{FTO}{}
#'   \item{overlapped}{an indicator of overlap with a preceding or following utterance ("overlap")}
#'   \item{talk_all}{}
#'   \item{talk_rel}{}
#'   \item{load}{}
#'   \item{transitions}{}
#'   \item{turns_all}{}
#'   \item{turns_rel}{}
#'   \item{participants}{}
#'   \item{rank_perc}{}
#'   \item{freq_perc}{}
#' }
#' @source {The IFADV corpus: A free dialog video corpus. R.J.J.H. van Son, Wieneke Wesseling, Eric Sanders, Henk van den Heuvel} \url{https://www.fon.hum.uva.nl/IFA-SpokenLanguageCorpora/IFADVcorpus/Documents/LREC2008RvSetal.pdf}
#' @source {IFA Dialog Video corpus} \url{https://www.fon.hum.uva.nl/IFA-SpokenLanguageCorpora/IFADVcorpus/}
"ifadv"
