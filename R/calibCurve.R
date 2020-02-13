#' Calculates regression curve for external standard calibration.
#'
#' The function calculates the regression curve of external standard
#' calibration to later use it to convert signals into concentration
#' values. It also plots the obtained calibration curve if desired.
#'
#' A simple linear method (i.e \code{lm()}) is applied to obtain the
#' regression curve. Creates a data frame to be used as complete
#' self-contained transport data set
#'
#' @param curve     Data frame of numeric vectors named 'Conc' and 'Signal'
#'                  containing the concentrations and the signals,
#'                  respectively.
#' @param order     Regression curve order. 1 for linear (default) and 2 for
#'                  quadratic. More options will be available soon.
#' @param badpoint  Numeric vector with the points to be ignored in the
#'                  regresion. This allows the easy elimination of outliers
#'                  without losing the stored measurement information.
#' @param intercept Logical. If \code{TRUE}, the default, the intercept is
#'                  calculated normally instead of being forced to 0.
#' @param plot      Logical. If \code{TRUE}, the default, the calibration data
#'                  is plotted.
#' @return Model of the calibration curve.
#' @examples
#'   data(curvelithium)
#'   model1 <- calibCurve(curve = curvelithium, order = 1)
#'   model2 <- calibCurve(curve = curvelithium, order = 2)
#'   summary(model1)
#'   summary(model2)
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
