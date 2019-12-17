#' Overlays several single-phase transport profiles
#'
#' Two empirical non-linear regression curves may be fitted. The data must be provided in
#'
#' @param trans     list of data frames generated using \code{conc2frac}. This is the only non-optional
#'                  parameter
#' @param trend     list of nls of ...
#' @param xlim      numeric vector of limits to be considered for X-axis
#' @param xbreaks   numeric vector of x-axis breaks
#' @param ylim      numeric vector of limits to be considered for X-axis
#' @param ybreaks   numeric vector of x-axis breaks
#'
#'
#' @return plot
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @export
#'

multiPlotSP <- function(trans, phase = 'strip', trend = NULL, legend = FALSE,
                        xlim = NULL, xbreaks = NULL, ylim = NULL, ybreaks = NULL, size = 3,
                        nat.color = TRUE, plot = TRUE, shape = 15){
  if (!any(phase == c('strip', 'Strip', 'feed', 'Feed'))) {
    stop("Only 'feed' or 'strip' are allowed to phase parameter")
  }
  if (phase == 'strip') phase <- 'Strip'
  if (phase == 'feed') phase <- 'Feed'

  for (i in 1:length(trans)) {
    trans[[i]] <- trans[[i]][which(trans[[i]]$Phase == phase), ]
  }

  hues = seq(15, 375, length = length(trans) + 1)
  if(nat.color) {
    cols = hcl(h = hues, l = 65, c = 100)[1:length(trans)]
  } else {
    cols = rep("black", length(trans))
  }


  p <- ggplot(data = trans[[1]], aes(x = Time, y = Fraction)) +
    theme_bw() + #ggsci::scale_color_npg() +
    geom_point(size = size, shape = shape, color = cols[1]) +
    labs(y = expression(Phi), x = 'Time (h)') +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   axis.text.x = element_text(color = "black"),
                   axis.text.y = element_text(color = "black"))

  for (i in 2:length(trans)) {
    p <- p + geom_point(data = trans[[i]], size = size, shape = shape, color = cols[i])
  }

  if (!missing(trend)) {
    for (i in 1:length(trans)) {
      if (trend[[1]]$model == 'paredes') {
        e <- trend[[1]]$eccen
        if (phase == 'Strip'){
          p <- p + stat_function(fun = function(x, i) (coefficients(trend[[i]]$strip)[1] * x^e)
                                 / (1/coefficients(trend[[i]]$strip)[2] + x^e),
                                 color = cols[i], args = list(i = i),
                                 xlim = c(0, trans[[i]]$Time[length(trans[[i]]$Time)]))
        } else {
          p <- p + stat_function(fun = function(x, i) (1 - (coefficients(trend[[i]]$feed)[1] * x^e)
                                                    / (1/coefficients(trend[[i]]$feed)[2] + x^e)),
                                 color = cols[i], args = list(i = i),
                                 xlim = c(0, trans[[i]]$Time[length(trans[[i]]$Time)]))
        }
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

  if(nat.color) {
    x <- y <- vector()
    for (i in 1:length(trans)) {
      x <- c(x, trans[[i]]$Time[1])
      y <- c(y, trans[[i]]$Fraction[1])
    }
    p <- p + geom_point(data = data.frame(col = as.factor(1:length(trans)), x = x, y = y),
                        aes(x = x, y = y, group = col, color = col))
  }

  if (plot) print(p)
  return(p)
}
