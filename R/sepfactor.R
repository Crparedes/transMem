#' Calculates separation factors between two transported species
#'
#' Given the transport data frames of two species, the function calculates
#' the separation factors of the main species A against a secondary species B
#' for each sample taken. If the dataset of secondary species is
#' smaller than that of the main species (e.g. if secondary species were
#' determined in only half the aliquots), the transport profile is completed
#' using \code{\link{fixSecondary}} function and a message will be printed.
#'
#' Separation factor for batch systems at any time different from zero is
#' defined as \deqn{\frac{C_a/C_b}{C_a^0/C_b^0}} where \eqn{C_a} and \eqn{C_b}
#' are the concentrations of A and B in the strip solution...
#' ...
#' -for continous systems the sep...
#'
#' @param main     data frame generated using \code{conc2frac}. This is the only non-optional
#'                  parameter
#' @param main0   data frame
#' @param secon   secondary metal considered
#' @param secon0  logical, can a linear profile of secondary metal be assumed?
#' @param order   asdf
#'
#' @return data frame
#' @import ggplot2
#'
#' @export


sepfactor <- function (main, secon, order = 2, continous = FALSE) {

  time_A <- main[which(main$Phase == "Strip"), 1]
  time_B <- secon[which(secon$Phase == "Strip"), 1]
  B_f <- secon[which(secon$Phase == "Feed"), 3] * secon0
  B_s <- secon[which(secon$Phase == "Strip"), 3] * secon0
  A_s <- main[which(main$Phase == "Strip"), 3] * main0
  A_f <- main[which(main$Phase == "Feed"), 3] * main0

  if (any(B_s < (B_f[1] - B_f))) B_s <- B_f[1] - B_f

  if (length(main[, 1]) > length(secon[, 1])) {
    B_s <- fixSecondary(conc = B_s, time = time_B, compTime = time_A, order = order)
    B_f <- fixSecondary(conc = B_f, time = time_B, compTime = time_A, order = order)
  }

  Sf <- data.frame(time = time_A, SF = (A_s / A_f) / (B_s / B_f))
  return(Sf)
}
