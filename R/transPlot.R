#' Plots individual transport profiles
#'
#' Given the fractions of the interest species and (optionally) up to two seccondary species,
#' the function plots transport profiles including (if given) non-linear regression models
#' obtained using \link{\code{transNLS}}.
#'
#' @param trans     main species fractions in time for both phases. Must be a data frame generated using
#'                  \link{\code{conc2frac}}. This is the only non-optional parameter
#' @param trend     non-linear regression model of the main transport profile
#' @param secondary secondary species fraction data. Must be a data frame generated using
#'                  \link{\code{conc2frac}}
#' @param tertiary  tertiaty species fraction data. Must be a data frame generated using
#'                  \link{\code{conc2frac}}
#' @param sec.trend type of trend line to be used for secondary and tertiary species data.
#'                  default to \code{'spline'} but \code{'linear'}, \code{'loess'} and
#'                  \code{'logarithmic'} are also available
#' @param span      amount of smoothing for loess trend curves. Only used if \code{sec.trend = 'loess'}.
#'                  Is a value between 0 and 1. Default is 0.75
#' @param legend    logical. Should a legend be included? Default to \code{FALSE}
#' @param xlim      numeric vector of limits to be considered for X-axis
#' @param xbreaks   numeric vector of x-axis breaks
#' @param ylim      numeric vector of limits to be considered for X-axis
#' @param ybreaks   numeric vector of x-axis breaks
#' @param size      size used for points in the plot
#' @param bw        logical default to \code{FALSE}. If \code{TRUE}, a black and white
#'                  plot is produced
#' @param srs       only used if \code{bw = TRUE}. It sets the relative size of the void space
#'
#'
#' @return plot
#' @import ggplot2 ggformula
#'
#' @export
#'

transPlot <- function(trans, trend = NULL, secondary = FALSE, tertiary = NULL,
                      sec.trend = 'spline', lin.secon = FALSE, span = 0.75,
                      legend = FALSE,
                      xlim = NULL, xbreaks = NULL, ylim = NULL, ybreaks = NULL,
                      size = 2.8, bw = FALSE, srs = 0.8){

  p <- ggplot(data = trans,
                       aes(x = Time, y = Fraction, group = Phase)) +
    theme_bw() + #ggsci::scale_color_npg() +
    geom_point(size = size, shape = 15, aes(color = Phase)) +
    labs(y = expression(Phi), x = 'Time (h)') +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   axis.text.x = element_text(color = "black"),
                   axis.text.y = element_text(color = "black"))

  if (!missing(trend)) {
    if (trend$model == 'paredes') {
      e <- trend$eccen
      if (bw) colbw <- c("black", "black") else colbw <- c("red", "black")
      p <- p + stat_function(fun = function(x) (coefficients(trend$strip)[1] * x^e)
                                      / (1/coefficients(trend$strip)[2] + x^e),
                                      color = colbw[1],#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                      xlim = c(0, trans$Time[length(trans$Time)])) +
        stat_function(fun = function(x) (1 - (coefficients(trend$feed)[1] * x^e)
                                         / (1/coefficients(trend$feed)[2] + x^e)),
                               color = colbw[2],# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                               xlim = c(0, trans$Time[length(trans$Time)]))
    }
  }
  if (!missing(lin.secon)) {
    warning("lin.secon is deprecated. Use sec.trend = 'linear' instead.")
    sec.trend = 'linear'
  }

  if (bw) {
    p <- p + geom_point(data = trans[which(trans$Phase == 'Strip'), ], col = 'white', size = size*srs,
                        aes(x = Time, y = Fraction), shape = 15)
  }

  if (!missing(secondary)) {
    if (class(secondary) == "data.frame") {
      secondary$Phase <- paste0(secondary$Phase, ".")
      if (sec.trend == 'linear') {
        p <- p + scale_shape_identity() +
          geom_smooth(method = "lm", data = secondary, se = FALSE, size = 0.5,
                                 aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      if (sec.trend == 'spline') {
        p <- p + scale_shape_identity() +
          geom_spline(data = secondary, spar = 0.7, size = 0.5,
                                 aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      if (sec.trend == 'logarithmic') { #Still under implementation
        p <- p + scale_shape_identity() +
          stat_smooth(data = secondary, method = "lm", formula = y ~ log(x), size = 0.5, se = FALSE,
                      aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      if (sec.trend == 'loess') {
        p <- p + scale_shape_identity() +
          stat_smooth(data = secondary, method = "loess", span = span, size = 0.5, se = FALSE,
                      aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      if (bw) {
        p <- p + geom_point(data = secondary, size = size,
                            aes(x = Time, y = Fraction), shape = 17, color = 'black')
        p <- p + geom_point(data = secondary[which(secondary$Phase == 'Strip.'), ], size = size * srs,
                            aes(x = Time, y = Fraction), shape = 17, color = 'white')
      } else {
        p <- p + geom_point(data = secondary, size = 3,
                            aes(x = Time, y = Fraction,
                                group = Phase, shape = 17, color = Phase))
      }
    } else {
      # PENDIENTE!! -> Admitir varios metales segundarios
    }
  }

  if (!missing(ternary)) {
    if (class(ternary) == "data.frame") {
      ternary$Phase <- paste0(ternary$Phase, ".")
      if (sec.trend == 'linear') {
        p <- p + scale_shape_identity() +
          geom_smooth(method = "lm", data = ternary, se = FALSE, size = 0.5,
                      aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      if (sec.trend == 'spline') {
        p <- p + scale_shape_identity() +
          geom_spline(data = ternary, spar = 0.7, size = 0.5,
                      aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      if (sec.trend == 'logarithmic') { #Still under implementation
        p <- p + scale_shape_identity() +
          stat_smooth(data = ternary, method = "lm", formula = y ~ log(x), size = 0.5, se = FALSE,
                      aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      if (sec.trend == 'loess') {
        p <- p + scale_shape_identity() +
          stat_smooth(data = ternary, method = "loess", span = span, size = 0.5, se = FALSE,
                      aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      if (bw) {
        p <- p + geom_point(data = ternary, size = size,
                            aes(x = Time, y = Fraction), shape = 16, color = 'black')
        p <- p + geom_point(data = ternary[which(ternary$Phase == 'Strip.'), ], size = size * srs,
                            aes(x = Time, y = Fraction), shape = 16, color = 'white')
      } else {
        p <- p + geom_point(data = ternary, size = 3,
                            aes(x = Time, y = Fraction,
                                group = Phase, shape = 16, color = Phase))
      }
    } else {
      # PENDIENTE!! -> Admitir varios metales segundarios
    }
  }

  if (!missing(xlim) && !missing(xbreaks)) {
    p <- p  + scale_x_continuous(breaks = xbreaks, limits = xlim)
  } else {
    if (!missing(xlim)) {
      p <- p  + scale_x_continuous(limits = xlim)
    }
    if (!missing(xbreaks)) {
      p <- p  + scale_x_continuous(breaks = xbreaks)
    }
  }

  if (!missing(ylim) && !missing(ybreaks)) {
    p <- p  + scale_y_continuous(breaks = ybreaks, limits = ylim)
  } else {
    if (!missing(ylim)) {
      p <- p  + scale_y_continuous(limits = ylim)
    }
    if (!missing(ybreaks)) {
      p <- p  + scale_y_continuous(breaks = ybreaks)
    }
  }

  if (!legend) {
    p <- p + theme(legend.position = 'none')
  }

  if (bw) {
    if (missing(secondary)) {
      p <- p + scale_color_manual(values = rep("black", 2))
    } else {
      p <- p + scale_color_manual(values = rep("black", 4))
    }
  } else {
    if (missing(secondary)) {
      p <- p + scale_color_manual(values = c("black", "red"))
    } else {
      p <- p + scale_color_manual(values = c("black", "gray48", "red", "indianred1"))
    }
  }

  print(p)
  return(p)
}
