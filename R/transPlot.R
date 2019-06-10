#' Fits non-linear trend curve to transport profiles
#'
#' Two empirical non-linear regression curves may be fitted. The data must be provided in
#'
#' @param trans     data frame generated using \code{conc2frac}. This is the only non-optional
#'                  parameter
#' @param trend     nls of ...
#' @param secondary secondary metal considered
#' @param xlim      numeric vector of limits to be considered for X-axis
#' @param xbreaks   numeric vector of x-axis breaks
#' @param ylim      numeric vector of limits to be considered for X-axis
#' @param ybreaks   numeric vector of x-axis breaks
#'
#' @return
#' @export
#'

transPlot <- function(trans, trend = NULL, secondary = NULL,
                      xlim = NULL, xbreaks = NULL, ylim = NULL, ybreaks = NULL){
  name <- deparse(substitute(trans))

  if (grepl('Transport', name, ignore.case = TRUE)) {
    name <- gsub('Transport', 'plot', name, ignore.case = TRUE)
  } else {
    name <- paste0('plot', name)
  }

  p <- ggplot2::ggplot(data = trans, aes(x = Time, y = Fraction, group = Phase)) +
    ggplot2::geom_point(size = 3, aes(color = Phase)) +
    ggplot2::labs(y = 'Transported fraction', x = 'Time')

  if (!missing(trend)) {
    if (trend$model == 'paredes') {
      e <- trend$eccen
      p <- p + ggplot2::stat_function(fun = function(x) (coefficients(trend$strip)[1] * x^e)
                                      / (1/coefficients(trend$strip)[2] + x^e), color = 'darkgrey') +
        ggplot2::stat_function(fun = function(x) (1 - (coefficients(trend$feed)[1] * x^e)
                                         / (1/coefficients(trend$feed)[2] + x^e)), color = 'darkgrey')
    }
  }

  if (!missing(secondary)) {
    library(ggformula)
    p <- p + ggplot2::scale_shape_identity() +
      geom_spline(data = secondary, aes(x = Time, y = Fraction, group = Phase), color = 'darkgrey') +
      ggplot2::geom_point(data = secondary, size = 4, aes(x = Time, y = Fraction,
                          group = Phase, shape = 'X', color = Phase))
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


  print(p)
  return(p)
}
