#' Converts signals into concentration by using given model.
#'
#' After a calibration model is established (either by using
#' \code{\link{calibCurve}} or \code{\link{calibPlane}}), the function
#' interpolates the signals of samples to get the associated concentrations.
#'
#' @param signal   numeric vector of signals to be interpolated.
#' @param model    regression model for the calibration. Must be obtained
#'                 using \code{\link{calibCurve}} or \code{\link{calibPlane}}.
#' @param dilution numeric vector of dilution factors applied to samples
#'                 before measurement
#' @param planar   logical, defalut to \code{FALSE}. It must be set to TRUE if
#'                 more than one explicatory variable is used. A planar
#'                 calibration model must be provided to \code{model}.
#' @param Conc.S   numeric vector of the concentrations of the interferent
#'                 specie to be considered when a planar calibration model is
#'                 provided to \code{model}. It is taken into account if
#'                 \code{planar} is set to TRUE.
#'
#' @return Numeric vector of concentrations
#' @importFrom cmna quadratic2
#' @examples
#'   #calibData <- #CREATE DATASET!!!!
#'   #calibPlane(plane = calibData)
#'
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export
#'

signal2conc <- function(signal, model, dilution = NULL,
                        planar = FALSE, Conc.S = NULL){
  name <- deparse(substitute(signal))

  if (grepl('Signal', name, ignore.case = TRUE) || grepl('Abs', name, ignore.case = TRUE)) {
    name <- gsub('Signal', 'Conc', name, ignore.case = TRUE)
    name <- gsub('Abs', 'Conc', name, ignore.case = TRUE)
  } else {
    name <- paste0('Conc', name)
  }

  if (planar) {
    conc <- predict(model$inter, newdata = data.frame(Signal = signal, Conc.S = Conc.S))
  } else {
    if (length(model$coefficients) == 2) {
      conc <- (signal - model$coefficients[1]) / model$coefficients[2]
    }
    if (length(model$coefficients) == 3) {
      conc <- cmna::quadratic2(b2 = model$coefficients[[3]], b1 = model$coefficients[[2]],
                               b0 = model$coefficients[[1]] - signal)
      conc <- conc[(length(conc) / 2 + 1):length(conc)]
    }
  }

  if (!missing(dilution)) conc <- conc * dilution

  return(conc)
}
