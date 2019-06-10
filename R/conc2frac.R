#' Convert signals into concentration by using given model
#'
#' @param feed     numeric vector with feed concentrations
#' @param strip    numeric vector with strip concentrations
#' @param time     numeric vector with time of aliquots sampling
#'
#' @return Data frame with the transport proccess information
#' @export
#'

conc2frac <- function(feed, strip, time = NULL){
  name <- deparse(substitute(feed))

  if (grepl('ConcFeed', name, ignore.case = TRUE)) {
    name <- gsub('ConcFeed', 'Transport', name, ignore.case = TRUE)
  } else {
    name <- paste0('Transport', name)
  }

  if (missing(time)) time <- 1:length(feed)

  strip <- strip / feed[1]
  feed  <- feed / feed[1]

  DF <- data.frame(Time = rep(time, 2), Phase = rep(c('Feed', 'Strip'), each = length(time)),
                   Fraction = c(feed, strip))

  return(DF)
}
