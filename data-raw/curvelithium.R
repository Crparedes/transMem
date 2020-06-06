#### curvelithium Data. Original file: 19-10-18_Li-Preconcentration_Mem18.R
StockLi.5_7   <- 130.3 * 0.187872 * 0.99 / 0.1205105 * 1.2673 / 50.0164
curvelithium <- data.frame(Conc = c(0.0000, 0.0597, 0.2989, 0.9115, 1.2053, 1.6816, 2.1670, 2.6474) *
                             StockLi.5_7 / c(6.0000, 6.1144, 6.0524, 6.0355, 6.1449, 6.0859, 6.0838, 6.0795),
                           Signal = c(0.000, 0.033, 0.161, 0.483, 0.619, 0.848, 1.050, 1.251))

#save(curvelithium, file = "/home/cris/Dropbox/transmem/data/curvelithium.RData")
usethis::use_data(curvelithium)
