#' Calculates regression curve for AAS
#'
#' @param curve     data frame of numeric vectors named 'Conc' and 'Signal' containing the
#'                  concentrations and the signals, respectively
#' @param order     regression curve order, 1 for linear (default), 2 for quadratic.
#'                  More options will be aviliable soon
#' @param badpoint  numeric vector with the points to be ignored in the regresión
#' @param intercept logical. if \code{TRUE} (default) the intercept is calculated normally
#'                  insthead of being forced to 0
#' @param plot      Logical. If \code{TRUE} (default) the calibration data is plotted
#' @return Model of the calibration curve
#' @export
#'

calibCurve <- function(curve, order = 1, badpoint = NULL,
                       intercept = TRUE, plot = TRUE){
  name <- deparse(substitute(curve))

  dfcheck(df = curve, param = c('Signal', 'Conc'), fun = 'calibCurvebCurve')

  if (!missing(badpoint)) curveN <- curve[- badpoint, ] else curveN <- curve

  if (intercept) {
    if (order == 1) model <- lm(Signal ~ Conc, data = curveN)

    if (order == 2) model <- lm(Signal ~ Conc + I(Conc ^ 2), data = curveN)

    if (order == 3) model <- lm(Signal ~ Conc + I(Conc ^ 2) + I(Conc ^ 3), data = curveN)
  } else {
    if (order == 1) model <- lm(Signal ~ 0 + Conc, data = curveN)

    if (order == 2) model <- lm(Signal ~ 0 + Conc + I(Conc ^ 2), data = curveN)

    if (order == 3) model <- lm(Signal ~ 0 + Conc + I(Conc ^ 2) + I(Conc ^ 3), data = curveN)
  }
  if (plot) plotCCurve(curve = curve, model = model, badpoint = badpoint)

  return(model)

}
