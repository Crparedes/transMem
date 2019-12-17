# Functions for internal use

# Lists check
dfcheck <- function(df, param, fun){
  for (i in param) {
    if (length(eval(parse(text = paste0("df$", i)))) == 0) {
      stop(paste0("Provided data frame must have vectors ", cat(paste0(param, ", ")),
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
                             aes(x = Time, y = Fraction, group = Phase, color = Phase))
  }
  if (sec.trend == 'logaritmic') { #Still under implementation
    p <- p + scale_shape_identity() +
      stat_smooth(data = secondary, method = "lm", formula = y ~ log(x), size = 0.5, se = FALSE,
                  aes(x = Time, y = Fraction, group = Phase, color = Phase))
  }
  if (sec.trend == 'loess') {
    p <- p + scale_shape_identity() +
      stat_smooth(data = secondary, method = "loess", span = span, size = 0.5, se = FALSE,
                  aes(x = Time, y = Fraction, group = Phase, color = Phase))
  }
  p <- p + geom_point(data = secondary, size = size,
                      aes(x = Time, y = Fraction,
                          group = Phase, shape = 17, color = Phase))
  return(p)
}
