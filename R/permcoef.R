#' calculates...
#'
#' @param trans
#' @param vol
#' @param area secondary metal considered
#' @param initial logical, can a linear profile of secondary metal be assumed?
#'
#' @return data frame
#'
#' @export


permcoef <- function(trans, conc_0, vol, area, units = c('cm^3', 'cm^2', 'h')){
  conc <- trans[which(trans$Phase == "Feed"), ]
  y <- log(conc[, 3] / conc[1, 3])
  t <- trans[which(trans$Phase == "Feed"), 1]
  if (units[3] == 'h') t <- t * 3600
  if (units[1] == 'cm^3') vol <- vol / 1000000
  if (units[2] == 'cm^2') area <- area / 10000
  plot(y ~ t)
  x <- (area / vol) * t
  model <- lm(y ~ 0 + x)
  abline(lm(y ~ 0 + t), col = 4)
  cat("Permeability coefficient: ", -1*summary(model)$coefficients[1], "+/-", summary(model)$coefficients[2], '\n')
  return(summary(model)$coefficients[1])
}
