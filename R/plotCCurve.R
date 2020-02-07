# No documentation needed as the function is not exported

plotCCurve <- function(curve, model = NULL, badpoint = NULL) {
  hpc <- rep(19, nrow(curve))

  if (!missing(badpoint)) hpc[badpoint] <- 21

  plot(curve$Signal ~ curve$Conc, pch = hpc)

  if (!missing(model)) {
    if (length(model$coefficients) == 2) {
      abline(model, col = 4)
    }
    if (length(model$coefficients) == 3) {
      curve(model$coefficients[1] + model$coefficients[2] * x +
              model$coefficients[3] * x^2, add = TRUE, col = 4)
    }
    if (length(model$coefficients) == 4) {
      curve(model$coefficients[1] + model$coefficients[2] * x +
              model$coefficients[3] * x^2 + model$coefficients[3] * x^3,
            add = TRUE, col = 4)
    }
  }
}
