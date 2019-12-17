#' Transforms feed and strip concentrations into fractions
#'
#' The change in concentration of species in the feed and strip phases as a
#' function of time are the main magnitudes being measured in processes
#' involving transport across membranes. This information is often provided as
#' the fractions still remaining or already transported in the feed and to the
#' strip phase, respectively. The function is used to transform the data from
#' concentrations to fractions.
#'
#' Usually this function will be used after \code{\link{signal2conc}} usage.
#'
#' @param feed          numeric vector with (concentrations in the feed phase
#' @param strip         numeric vector with concentrations in the strip phase
#' @param time          numeric vector with time of aliquots sampling
#'                      (optional, if not provided regular time intervals is
#'                      assumed)
#' @param correct.strip logical. If \code{FALSE} (default) the fraction
#'                      transported to the strip phase is calculated normally.
#'                      If set to \code{TRUE} the concentration in the strip
#'                      phase is substracted to all concentrations in the same
#'                      phase. This is useful when the blank signal is
#'                      significative or there is a background concentration
#'                      that is not desired to be considerated but corrected.
#' @example
#' #Provide a dataset!!!
#' @return Data frame with the transport proccess information
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export
#'

conc2frac <- function(feed, strip, time = NULL, correct.strip = FALSE){

  if (missing(time)) time <- 1:length(feed)

  strip <- strip / feed[1]
  feed  <- feed / feed[1]

  if (correct.strip) {
    strip <- strip - strip[1]
  }

  DF <- data.frame(Time = rep(time, 2), Phase = rep(c('Feed', 'Strip'),
                                                    each = length(time)),
                   Fraction = c(feed, strip))

  return(DF)
}
