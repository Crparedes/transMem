#' Interpolates seccondary metal concentration at missed values
#'
#' If the seccondary metal concentration is determinied in just a fraction
#' of the aliquots and for some reason the concentration in all the aliquots
#' is required (i.e. when this metal interferes the principal metal
#' quantification and a planar regression is necessary), the function fits
#' a polynomial trend line to the existing data and interpolates the
#' concentration in missing aliquots considering the times at which each
#' aliquot was taken.
#'
#' @param metalConc seccondary metal concentrations
#' @param time      times at which given concentrations were determined
#' @param compTime  times at which the seccondary metal must be interpolated
#' @param order     order of the polinomia to be fitted to data (1 or 2)
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
