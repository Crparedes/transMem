#' Fits non-linear trend curve to transport profiles
#'
#' Two empirical non-linear regression curves may be fitted. The data must be provided in
#'
#' @param trans     list of data frames generated using \code{conc2frac}. This is the only non-optional
#'                  parameter
#' @param trend     list of nls of ...
#' @param secondary list of secondary metal considered
#' @param lin.secon logical, can a linear profile of secondary metal be assumed?
#' @param xlim      numeric vector of limits to be considered for X-axis
#' @param xbreaks   numeric vector of x-axis breaks
#' @param ylim      numeric vector of limits to be considered for X-axis
#' @param ybreaks   numeric vector of x-axis breaks
#' @param explicit
#'
#'
#' @return plot
#' @import ggplot2
#' @export
#'

transPlotWR <- function(trans, trend = NULL, secondary = NULL, tertiary = NULL, legend = FALSE,
                        xlim = NULL, xbreaks = NULL, ylim = NULL, ybreaks = NULL,
                        lin.secon = FALSE, sec.trend = 'spline', span = 0.75,
                        explicit = FALSE, size = 3, eccen = 1, plot = TRUE, bw = FALSE, srs = 0.8){
  if (!missing(lin.secon)) {
    warning("lin.secon is deprecated. Use sec.trend = 'linear' instead.")
    sec.trend = 'linear'
  }

  if (explicit) {
    p <- ggplot(data = trans[[1]],
                         aes(x = Time, y = Fraction, group = Phase)) +
      theme_bw() + #ggsci::scale_color_npg() +
      geom_point(size = size, shape = 15, aes(color = Phase)) +
      labs(y = expression(Phi), x = 'Time (h)') +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text.x = element_text(color = "black"),
                     axis.text.y = element_text(color = "black"))

    for (i in 2:length(trans)) {
      p <- p + geom_point(data = trans[[i]], size = size, shape = 15, aes(color = Phase))
    }

    if (!missing(trend)) {
      if (length(trans) == 2) {
        if (trend[[1]]$model == 'paredes') {
          e <- trend[[1]]$eccen
          p <- p + stat_function(fun = function(x) (coefficients(trend[[1]]$strip)[1] * x^e)
                                          / (1/coefficients(trend[[1]]$strip)[2] + x^e),
                                          color = "red",#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                          xlim = c(0, trans[[1]]$Time[length(trans[[1]]$Time)])) +
            stat_function(fun = function(x) (1 - (coefficients(trend[[1]]$feed)[1] * x^e)
                                             / (1/coefficients(trend[[1]]$feed)[2] + x^e)),
                                   color = "black",# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                                   xlim = c(0, trans[[1]]$Time[length(trans[[1]]$Time)])) +
            stat_function(fun = function(x) (coefficients(trend[[2]]$strip)[1] * x^e)
                                   / (1/coefficients(trend[[2]]$strip)[2] + x^e),
                                   color = "red",#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                   xlim = c(0, trans[[2]]$Time[length(trans[[2]]$Time)])) +
            stat_function(fun = function(x) (1 - (coefficients(trend[[2]]$feed)[1] * x^e)
                                                      / (1/coefficients(trend[[2]]$feed)[2] + x^e)),
                                   color = "black",# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                                   xlim = c(0, trans[[2]]$Time[length(trans[[2]]$Time)]))
        }
      }
      if (length(trans) == 3) {
        if (trend[[1]]$model == 'paredes') {
          e <- trend[[1]]$eccen
          p <- p + stat_function(fun = function(x) (coefficients(trend[[1]]$strip)[1] * x^e)
                                          / (1/coefficients(trend[[1]]$strip)[2] + x^e),
                                          color = "red",#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                          xlim = c(0, trans[[1]]$Time[length(trans[[1]]$Time)])) +
            stat_function(fun = function(x) (1 - (coefficients(trend[[1]]$feed)[1] * x^e)
                                                      / (1/coefficients(trend[[1]]$feed)[2] + x^e)),
                                   color = "black",# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                                   xlim = c(0, trans[[1]]$Time[length(trans[[1]]$Time)])) +
            stat_function(fun = function(x) (coefficients(trend[[2]]$strip)[1] * x^e)
                                   / (1/coefficients(trend[[2]]$strip)[2] + x^e),
                                   color = "red",#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                   xlim = c(0, trans[[2]]$Time[length(trans[[2]]$Time)])) +
            stat_function(fun = function(x) (1 - (coefficients(trend[[2]]$feed)[1] * x^e)
                                                      / (1/coefficients(trend[[2]]$feed)[2] + x^e)),
                                   color = "black",# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                                   xlim = c(0, trans[[2]]$Time[length(trans[[2]]$Time)])) +
            stat_function(fun = function(x) (coefficients(trend[[3]]$strip)[1] * x^e)
                                   / (1/coefficients(trend[[3]]$strip)[2] + x^e),
                                   color = "red",#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                   xlim = c(0, trans[[3]]$Time[length(trans[[3]]$Time)])) +
            stat_function(fun = function(x) (1 - (coefficients(trend[[3]]$feed)[1] * x^e)
                                                      / (1/coefficients(trend[[3]]$feed)[2] + x^e)),
                                   color = "black",# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                                   xlim = c(0, trans[[3]]$Time[length(trans[[3]]$Time)]))
          }
        }
      }

      if (!missing(secondary)) {
        for (i in 1:length(secondary)) {
          secondary[[i]]$Phase <- paste0(secondary[[i]]$Phase, ".")
          if (sec.trend == 'linear') {
            p <- p + scale_shape_identity() +
              geom_smooth(method = "lm", data = secondary[[i]], se = FALSE, size = 0.5,
                          aes(x = Time, y = Fraction, group = Phase, color = Phase))
          }
          if (sec.trend == 'spline') {
            p <- p + scale_shape_identity() +
              ggformula::geom_spline(data = secondary[[i]], spar = 0.7, size = 0.5,
                                     aes(x = Time, y = Fraction, group = Phase, color = Phase))
          }
          if (sec.trend == 'logarithmic') { #Still under implementation
            p <- p + scale_shape_identity() +
              stat_smooth(data = secondary[[i]], method = "lm", formula = y ~ log(x), size = 0.5, se = FALSE,
                          aes(x = Time, y = Fraction, group = Phase, color = Phase))
          }
          if (sec.trend == 'loess') {
            p <- p + scale_shape_identity() +
              stat_smooth(data = secondary[[i]], method = "loess", span = span, size = 0.5, se = FALSE,
                          aes(x = Time, y = Fraction, group = Phase, color = Phase))
          }
          if (bw) {
            p <- p + geom_point(data = secondary[[i]], size = 3,
                                aes(x = Time, y = Fraction,
                                    group = Phase, shape = 17, color = Phase))
          } else {
            p <- p + geom_point(data = secondary[[i]], size = 3,
                                aes(x = Time, y = Fraction,
                                    group = Phase, shape = 17, color = Phase))
          }
        }
      }
    } else {
      mtrans <- transColapse(trans = trans)
      mtrend <- transTrend(trans = mtrans, model = 'paredes', eccen = eccen)

      if (bw) colbw <- c("black", "black") else colbw <- c("red", "black")

      p <- ggplot(data = mtrans, aes(x = Time, y = Fraction, group = Phase)) +
        theme_bw() + geom_point(size = size, shape = 15, aes(color = Phase)) +
        labs(y = expression(Phi), x = 'Time (h)') +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.text.x = element_text(color = "black"),
              axis.text.y = element_text(color = "black")) +
        stat_function(fun = function(x) (coefficients(mtrend$strip)[1] * x^eccen)
                                   / (1/coefficients(mtrend$strip)[2] + x^eccen),
                                   color = colbw[1],#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                   xlim = c(0, mtrans$Time[length(mtrans$Time)])) +
        stat_function(fun = function(x) (1 - (coefficients(mtrend$feed)[1] * x^eccen)
                                               / (1/coefficients(mtrend$feed)[2] + x^eccen)),
                            color = colbw[2],# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                            xlim = c(0, mtrans$Time[length(mtrans$Time)]))
      p <- p + geom_errorbar(aes(x = Time, ymin = Fraction - SD, ymax = Fraction + SD, color = Phase), width = 0.1)

      if (bw) {
        p <- p + geom_point(data = mtrans[which(mtrans$Phase == 'Strip'), ], col = 'white', size = size*srs,
                            aes(x = Time, y = Fraction), shape = 15)
      }

      if (!missing(secondary)) {
        msecon <- transColapse(trans = secondary)
        msecon$Phase <- paste0(msecon$Phase, ".")
        if (sec.trend == 'linear') {
          p <- p + scale_shape_identity() +
            geom_smooth(method = "lm", data = msecon, se = FALSE, size = 0.5,
                        aes(x = Time, y = Fraction, group = Phase, color = Phase))
        }
        if (sec.trend == 'spline') {
          p <- p + scale_shape_identity() +
            ggformula::geom_spline(data = msecon, spar = 0.7, size = 0.5,
                                   aes(x = Time, y = Fraction, group = Phase, color = Phase))
        }
        if (sec.trend == 'logarithmic') { #Still under implementation
          p <- p + scale_shape_identity() +
            stat_smooth(data = msecon, method = "lm", formula = y ~ log(x), size = 0.5, se = FALSE,
                        aes(x = Time, y = Fraction, group = Phase, color = Phase))
        }
        if (sec.trend == 'loess') {
          p <- p + scale_shape_identity() +
            stat_smooth(data = msecon, method = "loess", span = span, size = 0.5, se = FALSE,
                        aes(x = Time, y = Fraction, group = Phase, color = Phase))
        }

        p <- p + geom_errorbar(data = msecon, aes(x = Time, ymin = Fraction - SD,
                                                  ymax = Fraction + SD, color = Phase), width = 0.1)
        if (bw) {
          p <- p + geom_point(data = msecon, size = size,
                              aes(x = Time, y = Fraction), shape = 17, color = 'black')
          p <- p + geom_point(data = msecon[which(msecon$Phase == 'Strip.'), ], size = size * srs,
                              aes(x = Time, y = Fraction), shape = 17, color = 'white')
        } else {
          p <- p + geom_point(data = msecon, size = 3,
                              aes(x = Time, y = Fraction,
                                  group = Phase, shape = 17, color = Phase))
        }
      }
      if (!missing(ternary)) {
        mterna <- transColapse(trans = ternary)
        mterna$Phase <- paste0(mterna$Phase, ".")
        if (sec.trend == 'linear') {
          p <- p + scale_shape_identity() +
            geom_smooth(method = "lm", data = mterna, se = FALSE, size = 0.5,
                        aes(x = Time, y = Fraction, group = Phase, color = Phase))
        }
        if (sec.trend == 'spline') {
          p <- p + scale_shape_identity() +
            ggformula::geom_spline(data = mterna, spar = 0.7, size = 0.5,
                                   aes(x = Time, y = Fraction, group = Phase, color = Phase))
        }
        if (sec.trend == 'logarithmic') { #Still under implementation
          p <- p + scale_shape_identity() +
            stat_smooth(data = mterna, method = "lm", formula = y ~ log(x), size = 0.5, se = FALSE,
                        aes(x = Time, y = Fraction, group = Phase, color = Phase))
        }
        if (sec.trend == 'loess') {
          p <- p + scale_shape_identity() +
            stat_smooth(data = mterna, method = "loess", span = span, size = 0.5, se = FALSE,
                        aes(x = Time, y = Fraction, group = Phase, color = Phase))
        }
        p <- p + geom_errorbar(data = mterna, aes(x = Time, ymin = Fraction - SD,
                                                  ymax = Fraction + SD, color = Phase), width = 0.1)
        if (bw) {
          p <- p + geom_point(data = mterna, size = size,
                              aes(x = Time, y = Fraction), shape = 16, color = 'black')
          p <- p + geom_point(data = mterna[which(mterna$Phase == 'Strip.'), ], size = size * srs,
                              aes(x = Time, y = Fraction), shape = 16, color = 'white')
        } else {
          p <- p + geom_point(data = mterna, size = 3,
                              aes(x = Time, y = Fraction,
                                  group = Phase, shape = 16, color = Phase))
        }
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
      p <- p + scale_color_manual(values = rep("black", 6))
    }
  } else {
    if (missing(secondary)) {
      p <- p + scale_color_manual(values = c("black", "red"))
    } else {
      p <- p + scale_color_manual(values = c("black", "gray48", "red", "indianred1"))
    }
  }


  if (plot) print(p)
  return(p)
}
