#' Fits non-linear trend curve to transport profiles
#'
#' Two empirical non-linear regression curves may be fitted. The data must be provided in
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
    # DOI: http://sci-hub.tw/10.1016/j.jhazmat.2014.03.052
    nlsFeed  <- nls(Feed ~ A * exp(- Time / d) + Yo, start = list(A = 0.5, d = 0.5, Yo = 0))
    nlsStrip <- nls(Strip ~ A * exp(- Time / d) + Yo, start = list(A = -0.5, d = 0.5, Yo = 1))
    nlsModels <- list(feed = nlsFeed, strip = nlsStrip, model = 'rodriguez')
    nlsModels$Result <- c(1 / (summary(nlsFeed)$coefficients[3, 1] * summary(nlsFeed)$coefficients[2, 1]),
                          summary(nlsStrip)$coefficients[3, 1] / summary(nlsStrip)$coefficients[2, 1])
    names(nlsModels$Result) <- c('G_feed', 'G_strip')
  }

  return(nlsModels)
}
