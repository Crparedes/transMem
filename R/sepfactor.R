#' calculates...
#'
#' @param main     data frame generated using \code{conc2frac}. This is the only non-optional
#'                  parameter
#' @param main0
#' @param secon secondary metal considered
#' @param secon0 logical, can a linear profile of secondary metal be assumed?
#'
#' @return data frame
#' @import ggplot2
#'
#' @export


sepfactor <- function(main, main0, secon, secon0, order = 2){

  time_A <- main[which(main$Phase == "Strip"), 1]
  time_B <- secon[which(secon$Phase == "Strip"), 1]
  B_f <- secon[which(secon$Phase == "Feed"), 3] * secon0
  B_s <- secon[which(secon$Phase == "Strip"), 3] * secon0
  A_s <- main[which(main$Phase == "Strip"), 3] * main0
  A_f <- main[which(main$Phase == "Feed"), 3] * main0

  if (any(B_s < (B_f[1] - B_f))) B_s <- B_f[1] - B_f

  if (length(main[, 1]) > length(secon[, 1])) {
    B_s <- fixSecondary(metalConc = B_s, time = time_B, compTime = time_A, order = order)
    B_f <- fixSecondary(metalConc = B_f, time = time_B, compTime = time_A, order = order)
  }

  Sf <- data.frame(time = time_A, SF = (A_s / A_f) / (B_s / B_f))
  return(Sf)
}
