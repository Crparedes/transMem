#' Interpolates seccondary species concentration at missed values
#'
#' If the seccondary species concentration is determinied in just a fraction
#' of the aliquots and for some reason the concentration in all the aliquots
#' is required or desired, the function fits a polynomial trend line to the
#' existing data and interpolates the concentration in missing aliquots.
#'
#' @param metalConc Seccondary metal concentrations
#' @param time      Times at which given concentrations were determined
#' @param compTime  Times at which the seccondary metal must be interpolated
#' @param order     Order of the polinomia to be fitted to data (1 or 2)
#' @return Vector of interpolated concentrations
#' @export
#'

fixSecondary <- function(metalConc, time, compTime, order = 1) {
  model <- calibCurve(curve = data.frame(Conc = time,
                                         Signal = metalConc),
                      intercept = TRUE, order = order, plot = FALSE)

  fConc <- predict(model, newdata = data.frame(Conc = compTime))
  return(fConc)
}
