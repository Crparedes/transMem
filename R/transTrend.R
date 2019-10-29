#' Fits non-linear trend curve to transport profiles
#'
#' The data must be provided in ... ...
#'
#' @section Details:
#' Two empirical non-linear regression curves may be fitted....
#' Rodriguez-de-San-Miguel et.al. model, the fractions (\eqn{\Phi}) in feed or
#' strip phases as a function of time (\eqn{t}) are fitted to \deqn{\Phi=Ae^{-t/d}+y_0}
#' where \eqn{A}, \eqn{d} and \eqn{y_0} are the parameters to be determined. In this model,
#' the \eqn{d} parameter determines the steepness of variation, \eqn{y_0} reflects
#' the limiting value to which the profiles tend to at long pertraction times and \eqn{A}
#' is not supposed to play an important role in the transport description.
#' In the Paredes model the equations adjusted to the strip and feed phase are sligthly
#' different. The transported fraction to the strip phase is
#' \deqn{\Phi_s=\frac{\alpha_s t^\gamma}{\beta_s^{-1}+t^\gamma}} while the
#' fraction depleted from the feed phase is
#' \deqn{\Phi_f=1-\frac{\alpha_f t^\gamma}{\beta_f^{-1}+t^\gamma}}. In those equations \eqn{\alpha}
#' relates the maximun fraction transported at long pertraction times, \eqn{\beta}
#' relates the steepness of variation and \eqn{\gamma} is an excentricity factor to improve the adjustment.
#' The adjustable parameters are \eqn{\alpha} and \eqn{\beta}. The subscripts \eqn{s} and \eqn{f}
#' relates to strip and feed phases, respectively. If no significant accumulation is presented in the
#' membrane, the parameters should be quite similar for both phases and a consensus value can be obtained
#' by various ways. While the aritmetic mean could be appropiate, in order to improve the confidence limit
#' inherent to the non linear regression fitting, the values can be combinated by using meta-analisys tools.
#'
#' ¿¿EXCENTRICITY FACTOR CAN BE OBTAINED USING NLS??
#'
#' @param trans    data frame generated using \code{conc2frac}
#' @param model
#' @param eccen    eccentricity factor for model. onli used if \code{model} is set to \code{'paredes'}
#'
#' @return
#' @export
#'

transTrend <- function(trans, model = 'paredes', eccen = 2){
  name <- deparse(substitute(trans))

  if (grepl('Transport', name, ignore.case = TRUE)) {
    name <- gsub('Transport', 'nls', name, ignore.case = TRUE)
  } else {
    name <- paste0('nls', name)
  }

  Strip <- trans$Frac[which(trans$Phase == 'Strip')]
  Feed  <- trans$Frac[which(trans$Phase == 'Feed')]
  Time  <- unique(trans$Time)

  if (model == 'paredes') {
    nlsFeed   <- nls(Feed ~ 1 - (a * Time^eccen) / (1 / b + Time^eccen), start = list(a = 1, b = 0.5))
    nlsStrip  <- nls(Strip ~ (a * Time^eccen) / (1 / b + Time^eccen), start = list(a = 1, b = 0.5))
    nlsModels <- list(feed = nlsFeed, strip = nlsStrip, eccen = eccen, model = 'paredes')
    alpha = (summary(nlsFeed)$coefficients[1, 1] / summary(nlsFeed)$coefficients[1, 2]^2 +
               summary(nlsStrip)$coefficients[1, 1] / summary(nlsStrip)$coefficients[1, 2]^2) / (
                 summary(nlsFeed)$coefficients[1, 2]^-2 + summary(nlsStrip)$coefficients[1, 2]^-2)

    SE_alpha = sqrt(1 / (summary(nlsFeed)$coefficients[1, 2]^-2 + summary(nlsStrip)$coefficients[1, 2]^-2))

    beta  = (summary(nlsFeed)$coefficients[2, 1] / summary(nlsFeed)$coefficients[2, 2]^2 +
               summary(nlsStrip)$coefficients[2, 1] / summary(nlsStrip)$coefficients[2, 2]^2) / (
                 summary(nlsFeed)$coefficients[2, 2]^-2 + summary(nlsStrip)$coefficients[2, 2]^-2)

    SE_beta = sqrt(1 / (summary(nlsFeed)$coefficients[2, 2]^-2 + summary(nlsStrip)$coefficients[2, 2]^-2))

    nlsModels$Result <- c(alpha, SE_alpha, beta, SE_beta)
    names(nlsModels$Result) <- c('alpha', 'SE_alpha', 'beta', 'SE_beta')
  }

  if (model == 'rodriguez') {
    # DOI: 10.1016/j.jhazmat.2014.03.052
    nlsFeed  <- nls(Feed ~ A * exp(- Time / d) + Yo, start = list(A = 0.5, d = 0.5, Yo = 0))
    nlsStrip <- nls(Strip ~ A * exp(- Time / d) + Yo, start = list(A = -0.5, d = 0.5, Yo = 1))
    nlsModels <- list(feed = nlsFeed, strip = nlsStrip, model = 'rodriguez')
    nlsModels$Result <- c(1 / (summary(nlsFeed)$coefficients[3, 1] * summary(nlsFeed)$coefficients[2, 1]),
                          summary(nlsStrip)$coefficients[3, 1] / summary(nlsStrip)$coefficients[2, 1])
    names(nlsModels$Result) <- c('G_feed', 'G_strip')
  }

  return(nlsModels)
}
