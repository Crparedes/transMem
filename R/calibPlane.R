#' Calculates regression plane for external standard calibration.
#'
#' The function calculates the regression plane of external standard
#' calibration to later use it to convert signals into concentration values.
#' It differs from \code{\link{calibCurve}} in the number of explanatory
#' variables which 2 in this case. This function is useful when some
#' interference effect is being considered (e.g signal magnification of
#' lithium atomic absortion due to the precesnce of sodium in the sample).
#' The calibration plane is plotted if desired.
#'
#' A simple linear method (i.e \code{lm()}) is applied to obtain the
#' regression curve. Model assumptions (e.g normal distribution of
#' residuals) must be verified.
#'
#' @param plane     data frame of numeric vectors named 'Conc', 'Conc.S' and
#'                  'Signal'. The vectors must contain the concentrations of
#'                  the interest metal (the one whose concentration in the
#'                  samples is to be known), the secondary metal (interferent)
#'                  and the signals.
#' @param order     Not implemented yet... regression plane order. 1 for linear
#'                  (default), more options will be aviliable soon.
#' @inheritParams calibCurve
#' @return Model of the calibration plane
#' @examples
#'   #calibData <- #CREATE DATASET
#'   #calibPlane(plane = calibData)
#'
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export
#'

calibPlane <- function(plane, badpoint = NULL, plot = TRUE){

  if (!missing(badpoint)) curveN <- curve[- badpoint, ] else curveN <- curve

  model <- lm(Signal ~ Conc + Conc.S, data = plane)

  if (plot) {
    # plot(LithiumPC.Model, which = 1)
    x <- plane$Conc
    y <- plane$Conc.S
    z <- plane$Signal

    # predict values on regular xy grid
    grid.lines = 26
    x.pred <- seq(min(x), max(x), length.out = grid.lines)
    y.pred <- seq(min(y), max(y), length.out = grid.lines)
    xy <- expand.grid(Conc = x.pred, Conc.S = y.pred)
    z.pred <- matrix(predict(model, newdata = xy),
                     nrow = grid.lines, ncol = grid.lines)

    # fitted points for droplines to surface
    fitpoints <- predict(model)
    plot3D::scatter3D(x, y, z, pch = 18, cex = 2,
                      theta = -30, phi = 40, ticktype = "detailed",
                      xlab = "Lithium", ylab = "Sodium", zlab = "Absorbance",
                      surf = list(x = x.pred, y = y.pred, z = z.pred,
                                  facets = NA, fit = fitpoints))
  }

  model.inv <- lm(Conc ~ Signal + Conc.S, data = plane)
  model <- list(model = model, inter = model.inv)
  return(model)
}
