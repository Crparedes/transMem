#' Interpolates seccondary metal concentration at missed values
#'
#' @param metalConc seccondary metal concentrations
#' @param time      times at which given concentrations were determined
#' @param compTime  numeric vector with the times at which the seccondary metal must be interpolated
#' @param order     order of the polinomia to be fitted to data (1 or 2)
#' @return Vector of interpolated concentrations
#' @export
#'

fixSecondary <- function(metalConc, time, compTime, order = 1) {
  model <- calibCurve(curve = data.frame(Conc = time,
                                         Signal = metalConc),
                      autoload = FALSE, intercept = FALSE, order = order, plot = FALSE)

  fConc <- predict(model, newdata = data.frame(Conc = compTime))
  return(fConc)
}
