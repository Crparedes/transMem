#' Calculates regression curve for external standard calibration.
#'
#' The function calculates the regression curve of external standard
#' calibration to later use it to convert signals into concentration
#' values. It also plots the obtained calibration curve if desired.
#'
#' A simple linear method (i.e \code{lm()}) is applied to obtain the
#' regression curve. Model assumptions (e.g normal distribution of
#' residuals) must be verified by the user.
#'
#' @param curve     Data frame of numeric vectors named 'Conc' and 'Signal'
#'                  containing the concentrations and the signals,
#'                  respectively.
#' @param order     Regression curve order, 1 for linear (default) and 2 for
#'                  quadratic. More options will be aviliable soon.
#' @param badpoint  Numeric vector with the points to be ignored in the
#'                  regresión. This allows the easy elimination of ouliers
#'                  whithout losing the stored measurement information.
#' @param intercept Logical. If \code{TRUE}, the default, the intercept is
#'                  calculated normally insthead of being forced to 0.
#' @param plot      Logical. If \code{TRUE}, the default, the calibration data
#'                  is plotted.
#' @return Model of the calibration curve.
#' @examples
#'   calibData <- data.frame(Conc = c(0.00, 0.05, 0.25, 0.77, 1.00),
#'                           Signal = c(0.000, 0.031, 0.160, 0.476, 0.611))
#'   calibCurve(curve = calibData, order = 1)
#'   calibCurve(curve = calibData, order = 2)
#' @seealso \code{\link{calibPlane}} when using more than one explanatory
#'   variable.
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export

calibCurve <- function(curve, order = 1, badpoint = NULL,
                       intercept = TRUE, plot = TRUE){

  dfcheck(df = curve, param = c('Signal', 'Conc'), fun = 'calibCurve')
  if (!missing(badpoint)) curveN <- curve[- badpoint, ] else curveN <- curve

  if (intercept) {
    if (order == 1) model <- lm(Signal ~ Conc, data = curveN)
    if (order == 2) model <- lm(Signal ~ Conc + I(Conc ^ 2), data = curveN)
    if (order == 3) model <- lm(Signal ~ Conc + I(Conc ^ 2) +
                                  I(Conc ^ 3), data = curveN)
  } else {
    if (order == 1) model <- lm(Signal ~ 0 + Conc, data = curveN)
    if (order == 2) model <- lm(Signal ~ 0 + Conc + I(Conc ^ 2), data = curveN)
    if (order == 3) model <- lm(Signal ~ 0 + Conc + I(Conc ^ 2) +
                                  I(Conc ^ 3), data = curveN)
  }

  if (plot) plotCCurve(curve = curve, model = model, badpoint = badpoint)
  return(model)
}
