# Functions for internal use

# Lists check
dfcheck <- function(df, param, fun){
  for (i in param) {
    if (length(eval(parse(text = paste0("df$", i)))) == 0) {
      stop(paste0("Provided data frame must have vectors ", cat(paste0(param, ", ")),
                  " see ??", fun, " for details"))
    }
  }
}
