#' Convert signals into concentration by using given model
#'
#' @param signal   numeric vector of signals to be interpolated
#' @param model    regression model for the calibration curve
#' @param dilution numeric vector of dilution factors applied to samples
#' @param planar   logical
#' @param Conc.S   ...only used if \code{planar} is set to
#'
#' @return Numeric vector of concentrations
#' @importFrom cmna quadratic2
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
      conc <- conc[(length(conc)/2 + 1):length(conc)]
    }
  }

  if (!missing(dilution)) conc <- conc * dilution

  return(conc)
}
