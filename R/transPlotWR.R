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
#'
#'
#' @return plot
#' @export
#'

transPlotWR <- function(trans, trend = NULL, secondary = NULL, legend = FALSE,
                        xlim = NULL, xbreaks = NULL, ylim = NULL, ybreaks = NULL,
                        lin.secon = FALSE, sec.trend = 'spline', span = 0.75){

  p <- ggplot2::ggplot(data = trans[[1]],
                       ggplot2::aes(x = Time, y = Fraction, group = Phase)) +
    ggplot2::theme_bw() + #ggsci::scale_color_npg() +
    ggplot2::geom_point(size = 3, shape = 15, ggplot2::aes(color = Phase)) +
    ggplot2::labs(y = expression(Phi), x = 'Time (h)') +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(color = "black"),
                   axis.text.y = ggplot2::element_text(color = "black"))

  for (i in 2:length(trans)) {
    p <- p + ggplot2::geom_point(data = trans[[i]], size = 3, shape = 15, ggplot2::aes(color = Phase))
  }

  if (!missing(trend)) {
    if (length(trans) == 2) {
      if (trend[[1]]$model == 'paredes') {
        e <- trend[[1]]$eccen
        p <- p + ggplot2::stat_function(fun = function(x) (coefficients(trend[[1]]$strip)[1] * x^e)
                                        / (1/coefficients(trend[[1]]$strip)[2] + x^e),
                                        color = "red",#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                        xlim = c(0, trans[[1]]$Time[length(trans[[1]]$Time)])) +
          ggplot2::stat_function(fun = function(x) (1 - (coefficients(trend[[1]]$feed)[1] * x^e)
                                           / (1/coefficients(trend[[1]]$feed)[2] + x^e)),
                                 color = "black",# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                                 xlim = c(0, trans[[1]]$Time[length(trans[[1]]$Time)])) +
          ggplot2::stat_function(fun = function(x) (coefficients(trend[[2]]$strip)[1] * x^e)
                                 / (1/coefficients(trend[[2]]$strip)[2] + x^e),
                                 color = "red",#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                 xlim = c(0, trans[[2]]$Time[length(trans[[2]]$Time)])) +
          ggplot2::stat_function(fun = function(x) (1 - (coefficients(trend[[2]]$feed)[1] * x^e)
                                                    / (1/coefficients(trend[[2]]$feed)[2] + x^e)),
                                 color = "black",# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                                 xlim = c(0, trans[[2]]$Time[length(trans[[2]]$Time)]))
      }
    }
    if (length(trans) == 3) {
      if (trend[[1]]$model == 'paredes') {
        e <- trend[[1]]$eccen
        p <- p + ggplot2::stat_function(fun = function(x) (coefficients(trend[[1]]$strip)[1] * x^e)
                                        / (1/coefficients(trend[[1]]$strip)[2] + x^e),
                                        color = "red",#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                        xlim = c(0, trans[[1]]$Time[length(trans[[1]]$Time)])) +
          ggplot2::stat_function(fun = function(x) (1 - (coefficients(trend[[1]]$feed)[1] * x^e)
                                                    / (1/coefficients(trend[[1]]$feed)[2] + x^e)),
                                 color = "black",# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                                 xlim = c(0, trans[[1]]$Time[length(trans[[1]]$Time)])) +
          ggplot2::stat_function(fun = function(x) (coefficients(trend[[2]]$strip)[1] * x^e)
                                 / (1/coefficients(trend[[2]]$strip)[2] + x^e),
                                 color = "red",#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                 xlim = c(0, trans[[2]]$Time[length(trans[[2]]$Time)])) +
          ggplot2::stat_function(fun = function(x) (1 - (coefficients(trend[[2]]$feed)[1] * x^e)
                                                    / (1/coefficients(trend[[2]]$feed)[2] + x^e)),
                                 color = "black",# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                                 xlim = c(0, trans[[2]]$Time[length(trans[[2]]$Time)])) +
          ggplot2::stat_function(fun = function(x) (coefficients(trend[[3]]$strip)[1] * x^e)
                                 / (1/coefficients(trend[[3]]$strip)[2] + x^e),
                                 color = "red",#ggsci::pal_npg("nrc", alpha = 0.7)(2)[2],
                                 xlim = c(0, trans[[3]]$Time[length(trans[[3]]$Time)])) +
          ggplot2::stat_function(fun = function(x) (1 - (coefficients(trend[[3]]$feed)[1] * x^e)
                                                    / (1/coefficients(trend[[3]]$feed)[2] + x^e)),
                                 color = "black",# ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                                 xlim = c(0, trans[[3]]$Time[length(trans[[3]]$Time)]))
      }
    }
  }
  if (!missing(lin.secon)) {
    warning("lin.secon is deprecated. Use sec.trend = 'linear' instead.")
    sec.trend = 'linear'
  }
  if (!missing(secondary)) {
    for (i in 1:length(secondary)) {
      secondary[[i]]$Phase <- paste0(secondary[[i]]$Phase, ".")
      if (sec.trend == 'linear') {
        p <- p + ggplot2::scale_shape_identity() +
          ggplot2::geom_smooth(method = "lm", data = secondary[[i]], se = FALSE, size = 0.5,
                               ggplot2::aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      if (sec.trend == 'spline') {
        p <- p + ggplot2::scale_shape_identity() +
          ggformula::geom_spline(data = secondary[[i]], spar = 0.7, size = 0.5,
                                 ggplot2::aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      if (sec.trend == 'logaritmic') { #Still under implementation
        p <- p + ggplot2::scale_shape_identity() +
          stat_smooth(data = secondary[[i]], method = "lm", formula = y ~ log(x), size = 0.5, se = FALSE,
                      ggplot2::aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      if (sec.trend == 'loess') {
        p <- p + ggplot2::scale_shape_identity() +
          stat_smooth(data = secondary[[i]], method = "loess", span = span, size = 0.5, se = FALSE,
                      ggplot2::aes(x = Time, y = Fraction, group = Phase, color = Phase))
      }
      p <- p + ggplot2::geom_point(data = secondary[[i]], size = 3,
                                   ggplot2::aes(x = Time, y = Fraction,
                                                group = Phase, shape = 17, color = Phase))
    }
  }

  if (!missing(xlim) && !missing(xbreaks)) {
    p <- p  + ggplot2::scale_x_continuous(breaks = xbreaks, limits = xlim)
  } else {
    if (!missing(xlim)) {
      p <- p  + ggplot2::scale_x_continuous(limits = xlim)
    }
    if (!missing(xbreaks)) {
      p <- p  + ggplot2::scale_x_continuous(breaks = xbreaks)
    }
  }

  if (!missing(ylim) && !missing(ybreaks)) {
    p <- p  + ggplot2::scale_y_continuous(breaks = ybreaks, limits = ylim)
  } else {
    if (!missing(ylim)) {
      p <- p  + ggplot2::scale_y_continuous(limits = ylim)
    }
    if (!missing(ybreaks)) {
      p <- p  + ggplot2::scale_y_continuous(breaks = ybreaks)
    }
  }

  if (!legend) {
    p <- p + ggplot2::theme(legend.position = 'none')
  }

  if (missing(secondary)) {
    p <- p + ggplot2::scale_color_manual(values = c("black", "red"))
  } else {
    p <- p + ggplot2::scale_color_manual(values = c("black", "gray48", "red", "indianred1"))
  }

  print(p)
  return(p)
}
