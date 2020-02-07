# Functions for internal use

# Lists check
dfcheck <- function(df, param, fun){
  for (i in param) {
    if (length(eval(parse(text = paste0("df$", i)))) == 0) {
      stop(paste0("Provided data frame must have vectors ",
                  cat(paste0(param, ", ")),
                  " see ??", fun, " for details"))
    }
  }
}

# Colapse multi transport
transColapse <- function(trans){
  mtrans <- trans[[1]]
  frac <- matrix(ncol = length(trans), nrow = length(trans[[1]]$Time))
  for (i in 1:length(trans)) {
    frac[, i] <- trans[[i]]$Fraction
  }
  mtrans$SD <- apply(frac, 1, sd)
  mtrans$Fraction <- apply(frac, 1, mean)
  return(mtrans)
}


# Add secondary metal
AddSecondary <- function(secondary, p){
  secondary$Phase <- paste0(secondary$Phase, ".")
  if (sec.trend == 'linear') {
    p <- p + scale_shape_identity() +
      geom_smooth(method = "lm", data = secondary, se = FALSE, size = 0.5,
                  aes(x = Time, y = Fraction, group = Phase, color = Phase))
  }
  if (sec.trend == 'spline') {
    p <- p + scale_shape_identity() +
      ggformula::geom_spline(data = secondary, spar = 0.7, size = 0.5,
                             aes(x = Time, y = Fraction, group = Phase,
                                 color = Phase))
  }
  if (sec.trend == 'logaritmic') { #Still under implementation
    p <- p + scale_shape_identity() +
      stat_smooth(data = secondary, method = "lm", formula = y ~ log(x),
                  size = 0.5, se = FALSE,
                  aes(x = Time, y = Fraction, group = Phase, color = Phase))
  }
  if (sec.trend == 'loess') {
    p <- p + scale_shape_identity() +
      stat_smooth(data = secondary, method = "loess", span = span,
                  size = 0.5, se = FALSE,
                  aes(x = Time, y = Fraction, group = Phase, color = Phase))
  }
  p <- p + geom_point(data = secondary, size = size,
                      aes(x = Time, y = Fraction,
                          group = Phase, shape = 17, color = Phase))
  return(p)
}

xlimTrendWR <- function(x, trans){
  return(c(0, trans[[x]]$Time[length(trans[[x]]$Time)]))
}

AddParTrend <- function(trend, pos, phase, e) {
  if (phase == 'strip') {
    return(
      function(x) {
        ((coefficients(trend[[pos]]$strip)[1] * x^e)
        / (1/coefficients(trend[[pos]]$strip)[2] + x^e))
      }
    )
  } else {
    return(
      function(x) {
        ((1 - (coefficients(trend[[pos]]$feed)[1] * x^e)
         / (1/coefficients(trend[[pos]]$feed)[2] + x^e)))
      }
    )
  }
}

AddParedes <- list(
  function(x) (coefficients(trend[[1]]$strip)[1] * x^e)
  / (1/coefficients(trend[[1]]$strip)[2] + x^e),
  function(x) (1 - (coefficients(trend[[1]]$feed)[1] * x^e)
               / (1/coefficients(trend[[1]]$feed)[2] + x^e)),
  function(x) (coefficients(trend[[2]]$strip)[1] * x^e)
  / (1/coefficients(trend[[2]]$strip)[2] + x^e),
  function(x) (1 - (coefficients(trend[[2]]$feed)[1] * x^e)
               / (1/coefficients(trend[[2]]$feed)[2] + x^e)),
  function(x) (coefficients(trend[[3]]$strip)[1] * x^e)
  / (1/coefficients(trend[[3]]$strip)[2] + x^e),
  function(x) (1 - (coefficients(trend[[3]]$feed)[1] * x^e)
               / (1/coefficients(trend[[3]]$feed)[2] + x^e)),
  function(x) (coefficients(mtrend$strip)[1] * x^eccen)
  / (1/coefficients(mtrend$strip)[2] + x^eccen),
  function(x) (1 - (coefficients(mtrend$feed)[1] * x^eccen)
               / (1/coefficients(mtrend$feed)[2] + x^eccen))
)














# Function not exported to the user as it is still under development.
# It is still possibly to use it by transmem:::extrMolRat()
extrMolRat <- function(mass = NULL, ratio = NULL, ex1 = NULL, ex2 = NULL,
                       mw1 = 246, mw2 = 348, rho1 = NULL, rho2 = NULL) {
  if (missing(mass) && missing(ratio)) {
    report <- cbind(TotalMass = ex1 + ex2,
                    MolarRatio <- (ex1 / mw1) / (ex2 / mw2))
  } else {
    MassRatio <- mw1 * ratio / mw2
    report    <- cbind(Extr.1 = (mass * MassRatio) / (1 + MassRatio),
                       Extr.2 = mass / (1 + MassRatio))
  }
  return(report)
}


