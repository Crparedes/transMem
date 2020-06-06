#### reusecycles Data. Original file: 19-10-25-Li-Reuse_Mem19.R
StockLi.5_8   <- 130.3 * 0.187872 * 0.99 / 0.1205105 * 0.2646 / 10.3841
cyclesaliquots <- list(feed.1 = c(1.115, 0.622, 0.342, 0.194, 0.116),
                       strip.1= c(0.000, 0.636, 0.898, 1.021, 1.078),
                       feed.2 = c(1.138, 0.751, 0.482, 0.321, 0.217),
                       strip.2 = c(0.006, 0.578, 0.852, 0.983, 1.059),
                       feed.3 = c(1.142, 0.713, 0.518, 0.361, 0.264),
                       strip.3 = c(0.006, 0.602, 0.802, 0.938, 1.015),
                       feed.4 = c(1.167, 0.809, 0.573, 0.402, 0.300),
                       strip.4 = c(0.006, 0.499, 0.757, 0.908, 0.990),
                       feed.5 = c(1.136, 0.850, 0.609, 0.400, 0.335),
                       strip.5 = c(0.008, 0.541, 0.793, 0.900, 1.046),
                       feed.6 = c(1.215, 0.921, 0.693, 0.535, 0.417),
                       strip.6 = c(0.003, 0.451, 0.706, 0.856, 0.963),
                       feed.7 = c(1.209, 0.923, 0.726, 0.582, 0.470),
                       strip.7 = c(0.004, 0.440, 0.687, 0.830, 0.935),
                       feed.8 = c(1.234, 0.979, 0.789, 0.658, 0.553),
                       strip.8 = c(0.005, 0.402, 0.630, 0.764, 0.863),
                       feed.9 = c(1.249, 1.014, 0.814, 0.682, 0.587),
                       strip.9 = c(0.008, 0.377, 0.621, 0.758, 0.846),
                       feed.10 = c(1.248, 1.028, 0.874, 0.738, 0.656),
                       strip.10 = c(0.007, 0.355, 0.565, 0.714, 0.792))
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.0000, 0.0685, 0.2891, 0.9453, 1.1944, 1.6853, 2.2121, 2.7284) *
                           StockLi.5_8 / c(6.0000, 6.6725, 6.3378, 6.6590, 6.6845, 6.6557, 6.8208, 6.6317),
                         Signal = c(0.000, 0.036, 0.162, 0.477, 0.586, 0.797, 0.971, 1.169)),
  Lithium.2 = data.frame(Conc = c(0.0000, 0.0597, 0.2989, 0.9115, 1.2053, 1.6816, 2.1670, 2.6474) *
                           StockLi.5_8 / c(6.0000, 6.1144, 6.0524, 6.0355, 6.1449, 6.0859, 6.0838, 6.0795),
                         Signal = c(0.000, 0.037, 0.168, 0.497, 0.618, 0.839, 1.032, 1.241))
)
CalModels <- list()
for (i in 1:2) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = 2, plot = TRUE)
names(CalModels) <- names(CalCurves)
AliTimes <- c(0, 1.5, 3, 4.75, 6, 6.01, 7.5, 9, 10.5, 12, 12.01, 13.5, 15.25,
              16.5, 18, 18.01, 19.5, 21, 22.5, 24, 24.01, 25.5, 27, 28.5, 30,
              30.01, 31.5, 33, 34.5, 36, 36.01, 37.5, 39, 40.5, 42, 42.01,
              43.5, 45, 46.5, 48, 48.01, 49.5, 51, 52.5, 54, 54.01, 55.5, 57, 60)
AliAbs <- list(Cycle.1.F.b = c(1.115, 0.622, 0.342, 0.194, 0.116),
               Cycle.1.S.b = c(0.000, 0.636, 0.898, 1.021, 1.078),
               Cycle.2.F.b = c(1.138, 0.751, 0.482, 0.321, 0.217),
               Cycle.2.S.b = c(0.006, 0.578, 0.852, 0.983, 1.059),
               Cycle.3.F.b = c(1.142, 0.713, 0.518, 0.361, 0.264),
               Cycle.3.S.b = c(0.006, 0.602, 0.802, 0.938, 1.015),
               Cycle.4.F.b = c(1.167, 0.809, 0.573, 0.402, 0.300),
               Cycle.4.S.b = c(0.006, 0.499, 0.757, 0.908, 0.990),
               Cycle.5.F.b = c(1.136, 0.850, 0.609, 0.400, 0.335),
               Cycle.5.S.b = c(0.008, 0.541, 0.793, 0.900, 1.046),
               Cycle.6.F.b = c(1.215, 0.921, 0.693, 0.535, 0.417),
               Cycle.6.S.b = c(0.003, 0.451, 0.706, 0.856, 0.963),
               Cycle.7.F.b = c(1.209, 0.923, 0.726, 0.582, 0.470),
               Cycle.7.S.b = c(0.004, 0.440, 0.687, 0.830, 0.935),
               Cycle.8.F.b = c(1.234, 0.979, 0.789, 0.658, 0.553),
               Cycle.8.S.b = c(0.005, 0.402, 0.630, 0.764, 0.863),
               Cycle.9.F.b = c(1.249, 1.014, 0.814, 0.682, 0.587),
               Cycle.9.S.b = c(0.008, 0.377, 0.621, 0.758, 0.846),
               Cycle.10.F.b = c(1.248, 1.028, 0.874, 0.738, 0.656),
               Cycle.10.S.b = c(0.007, 0.355, 0.565, 0.714, 0.792))
AliConc <- vector(mode = "list", length = length(AliAbs))
names(AliConc) <- names(AliAbs)
for (i in 1:length(AliAbs)) {
  if(i %in% c(1:12)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Lithium.1)
  if(i %in% c(13:20)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Lithium.2)
}
TransFrac <- vector(mode = "list", length = length(AliAbs) / 2)
names(TransFrac) <- names(AliAbs)[seq(1, 20, 2)]
for (i in 1:10) {
  TransFrac[[i]] <- conc2frac(feed = AliConc[[2 * i - 1]], strip = AliConc[[2 * i]],
                              time = c(0, 1.5, 3, 4.5, 6))
}

reusecycles = list(TransFrac[[1]], TransFrac[[2]], TransFrac[[3]], TransFrac[[4]], TransFrac[[5]],
                   TransFrac[[6]], TransFrac[[7]], TransFrac[[8]], TransFrac[[9]], TransFrac[[10]])
permcoef(trans = reusecycles[[1]], conc_0 = AliConc[[1]][1], vol = 85, area = pi*1.25^2)

usethis::use_data(reusecycles)
#save(reusecycles, file = "/home/cris/Dropbox/transmem/data/reusecycles.RData")
