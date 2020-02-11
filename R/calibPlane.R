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
#' regression equation. Model assumptions (e.g normal distribution of
#' residuals) must be verified by the user.
#'
#' @param plane  Data frame of numeric vectors named 'Conc', 'Conc.S' and
#'               'Signal'. The vectors must contain the concentrations of
#'               the interest metal (the one whose concentration in the
#'               samples is to be known), the secondary metal (interferent)
#'               and the signals, respectively.
#' @param lines  Number of lines to use in the mesh of the plane in the plot.
#' @param xlab   Label for Z axis (response).
#' @param ylab   Label for Z axis (response).
#' @param zlab   Label for Z axis (response).
#' @param pch    Plotting symbols available in R.
#' @param cex    The size of pch symbols.
#' @param theta  Azimuthal angle at which the plane is visualized.
#' @param phi    Altitude angle  at which the plane is visualized.
#' @inheritParams calibCurve
#' @return Model of the calibration plane
#' @examples
#'   data(planelithium)
#'   planeModel <- calibPlane(plane = planelithium)
#'   summary(planeModel$model)
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @importFrom plot3D scatter3D
#' @export
#'

calibPlane <- function(plane, badpoint = NULL, plot = TRUE,
                       lines = 13, theta = -30, phi = 40, xlab = "Species 1",
                       ylab = "Species 2", zlab = "Signal", pch = 18, cex = 2){

  if (!missing(badpoint)) curveN <- curve[- badpoint, ] else curveN <- curve

  model <- lm(Signal ~ Conc + Conc.S, data = plane)

  if (plot) {
    # plot(LithiumPC.Model, which = 1)
    x <- plane$Conc
    y <- plane$Conc.S
    z <- plane$Signal

    # predict values on regular xy grid
    grid.lines = 13
    x.pred <- seq(min(x), max(x), length.out = lines)
    y.pred <- seq(min(y), max(y), length.out = lines)
    xy <- expand.grid(Conc = x.pred, Conc.S = y.pred)
    z.pred <- matrix(predict(model, newdata = xy), nrow = lines, ncol = lines)

    # fitted points for droplines to surface
    fitpoints <- predict(model)
    scatter3D(x, y, z, pch = 18, cex = 2,
              theta = -30, phi = 40, ticktype = "detailed",
              xlab = xlab, ylab = ylab, zlab = zlab,
              surf = list(x = x.pred, y = y.pred, z = z.pred,
                          facets = NA, fit = fitpoints))
  }

  model.inv <- lm(Conc ~ Signal + Conc.S, data = plane)
  model <- list(model = model, inter = model.inv)
  return(model)
}
