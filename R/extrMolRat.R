#' Calculates the
#'
#' Depending on which parameters are given,
#'
#' @param mass  total mass of extractants when
#' @param ratio desired molar ratio of extractant 1/extractant 2 when
#' @param ex1   measured mass or volume of extractant 1. If parameter has volume units, density must be provided
#' @param ex2   measured mass or volume of extractant 2. If parameter has volume units, density must be provided
#' @param mw1   molecular weight of extractant 1. Default is 246 corresponding to LIX-54
#' @param mw2   molecular weight of extractant 2. Default is 348 corresponding to Cyanex 923
#' @param rho1  density of extractant 1
#' @param rho2  density of extractant 2
#' @return Total mass and molar ratio of given ammount of extractants mixed or the amounts of extractants to take
#'        when desired molar ratio and total mass is provided.
#' @export
#'

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
