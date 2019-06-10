#' Calculates regression plane
#'
#' @param plane     data frame of numeric vectors named 'Conc', 'Conc.S' and 'Signal' containing the
#'                  concentrations of the interest metal, the secondary metal (interferent)
#'                  and the signals, respectively
#' @param order     Not implemented yet... regression curve order, 1 for linear (default),
#'                  more options will be aviliable soon
#' @param badpoint  numeric vector with the points to be ignored in the regresión
#' @return Model of the calibration plane
#' @export
#'

calibPlane <- function(plane, badpoint = NULL){
  name <- deparse(substitute(plane))

  if (!missing(badpoint)) curveN <- curve[- badpoint, ] else curveN <- curve

  model <- lm(Signal ~ Conc + Conc.S, data = plane)

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

  model.inv <- lm(Conc ~ Signal + Conc.S, data = plane)

  model <- list(model = model, inter = model.inv)

  return(model)
}
