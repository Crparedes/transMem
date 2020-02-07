#' Plots transport profiles for processes involving several cycles
#'
#' Given the data (data frames) of a transport process that was carried
#' in several cycles (e.g. membrane reuse or metal concentration studies),
#' plots the transport profiles like in a continous experiment indicating
#' the end of each cycle
#'
#' If a concentration experiment has been made through the cycles, it is
#' reccomended the y-axis to be in concentration scale insthead of fractions.
#' To get the transport data frame in concentration units use
#' \code{conc2frac(..., normalize = FALSE)}. For more details see
#' \code{\link{conc2frac}}.
#'
#' Most \code{transmem} graphical representations are made using the package
#' \code{ggplot2} so the function returns a ggplot2 object that can be
#' assigned to a variable for further modification.
#'
#' @references
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York. ISBN 978-3-319-24277-4,
#' \url{https://ggplot2.tidyverse.org}.
#'
#'
#' @param transList List containing the (ordered) transport data of each cycle.
#'                  Each data frame must be generated using
#'                  \code{\link{conc2frac}}.
#' @inheritParams transPlot
#'
#' @return Plot of the transport process carried along several cycles
#' @import ggplot2 ggformula
#'
#' @export
#'

cyclesPlot <- function(transList, xlim = NULL, xbreaks = NULL, ylim = NULL,
                       ybreaks = NULL, size = 1.8, legend = FALSE,
                       xlab = 'Time (h)', ylab = expression(Phi)){
  #Missing global variables issue correction
  Time <- Fraction <- Phase <- Group <- NULL

  cuts <- 0
  for (i in 1:length(transList)) {
    cuts <- c(cuts, transList[[i]]$Time[length(transList[[i]]$Time)])
  }

  cTrans <- cbind(transList[[1]], Group = rep(c(1, 101),
                    each = length(transList[[1]]$Time)/2))
  for (i in 2:length(transList)) {
    cTrans <- rbind(cTrans, cbind(transList[[i]], Group = rep(c(i, 100 + i),
                      each = length(transList[[i]]$Time)/2)))
  }

  p <- ggplot(data = cTrans, aes(x = Time, y = Fraction, shape = Phase,
                                 group = Group)) +
         geom_vline(xintercept = c(cuts), linetype = 'dashed',
                    color = 'gray') +
         geom_point(size = size) + theme_bw() +
         geom_smooth(method = 'loess', color = 'black', lwd = 0.5, span = 1) +
         #geom_errorbar(aes(ymin = Conc - 5, ymax = Conc + 5), width = 0.4) +
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.text.x = element_text(color = "black"),
               axis.text.y = element_text(color = "black")) +
         scale_color_manual(values = c("black", "red")) +
         labs(y = ylab, x = xlab) +
         theme(text = element_text(size = 9))

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

  print(p)
  return(p)
}
