#### concentrationcycles Data. Original file: 19-10-18_Li-Preconcentration_Mem18.R
StockLi.5_7   <- 130.3 * 0.187872 * 0.99 / 0.1205105 * 1.2673 / 50.0164
curvelithium <- data.frame(Conc = c(0.0000, 0.0597, 0.2989, 0.9115, 1.2053, 1.6816, 2.1670, 2.6474) *
                             StockLi.5_7 / c(6.0000, 6.1144, 6.0524, 6.0355, 6.1449, 6.0859, 6.0838, 6.0795),
                           Signal = c(0.000, 0.033, 0.161, 0.483, 0.619, 0.848, 1.050, 1.251))

CalModel <- calibCurve(curve = curvelithium, order = 2, plot = TRUE)
AliTimes <- list(c(0, 1.5, 3, 4.75, 6),
                 c(6.01, 7.5, 9, 10.5, 12),
                 c(12.01, 13.5, 15.25, 16.5, 18),
                 c(18.01, 19.5, 21, 22.5, 24),
                 c(24.01, 25.5, 27, 28.5, 30))
dilutions <- list(Strip.1 = rep(1, 5),
                  Strip.2 = c(8517/3701, 8296/3734, 7863/3439, 8278/3662, 7967/3554),
                  Strip.3 = c(12305/3293, 11113/3265, 11160/3267, 11150/3277, 11142/3281),
                  Strip.4 = c(12838/2797, 12759/2807, 12747/2775, 12740/2780, 12666/2760),
                  Strip.5 = c(12582/2758, 13710/2775, 13681/2753, 13662/2769, 13718/2786))
AliAbs <- list(Feed.1 = c(1.170, 0.601, 0.358, 0.208, 0.155),
               Feed.2 = c(1.140, 0.750, 0.554, 0.427, 0.341),
               Feed.3 = c(1.160, 0.871, 0.672, 0.581, 0.499),
               Feed.4 = c(1.157, 0.940, 0.788, 0.688, 0.610),
               Feed.5 = c(1.156, 0.983, 0.868, 0.782, 0.727),
               Strip.1 = c(0.005, 0.574, 0.798, 0.909, 0.930),
               Strip.2 = c(0.483, 0.687, 0.760, 0.829, 0.850),
               Strip.3 = c(0.552, 0.690, 0.745, 0.773, 0.802),
               Strip.4 = c(0.614, 0.687, 0.714, 0.735, 0.751),
               Strip.5 = c(0.752, 0.749, 0.771, 0.790, 0.805))
concentrationcycles <- list()
for (i in 1:5) {
  feed  <- signal2conc(signal = AliAbs[[i]], model = CalModel)
  strip <- signal2conc(signal = AliAbs[[5+i]], model = CalModel, dilution = dilutions[[i]])
  concentrationcycles[[i]] <- conc2frac(feed = feed, strip = strip, time = AliTimes[[i]], normalize = FALSE)
}
cyclesPlot(trans = concentrationcycles, ylab = expression(paste('Conc. (mg k', g^{-1}, ')')))

#save(concentrationcycles, file = "/home/cris/Dropbox/transmem/data/concentrationcycles.RData")
usethis::use_data(concentrationcycles)
