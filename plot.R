setwd("~/eckNN_aaai2020")

load("sym_sample_entropy.RData")
plot(5:20, apply(knn_bw, 2, function(x) sqrt(mean(x^2))), col = 8, ylab = "Average RMSE", xlab = "Dimensionality", type = 'o', pch = 20, lty = 1)
lines(5:20, apply(classical_1, 2, function(x) sqrt(mean(x^2))), col = 2, type = "o", pch = 20)
lines(5:20, apply(classical_5, 2, function(x) sqrt(mean(x^2))), col = 3, type = "o", pch = 20)
lines(5:20, apply(classical_10, 2, function(x) sqrt(mean(x^2))), col = 4, type = "o", pch = 20)
lines(5:20, apply(classical_15, 2, function(x) sqrt(mean(x^2))), col = 5, type = "o", pch = 20)
lines(5:20, apply(classical_25, 2, function(x) sqrt(mean(x^2))), col = 6, type = "o", pch = 20)
lines(5:20, apply(ec, 2, function(x) sqrt(mean(x^2))), col = 1, type = "o", pch = 20)
title("Symmetric Case (Entropy)")
legend(5, 30, c("EC-kNN (k = 25)", "kNN (k = 1)",  "kNN (k = 5)", "kNN (k = 10)", "kNN (k = 15)", "kNN (k = 25)", "kNN bw"),
       pch = 20, lty = 1, col = c(1, 2, 3, 4, 5, 6, 8))


load("asym_sample_entropy.RData")
plot(5:20, apply(knn_bw, 2, function(x) sqrt(mean(x^2))), col = 8, ylab = "RMSE", xlab = "Dimensionality", type = 'o', pch = 20, lty = 1)
lines(5:20, apply(classical_1, 2, function(x) sqrt(mean(x^2))), col = 2, type = "o", pch = 20)
lines(5:20, apply(classical_5, 2, function(x) sqrt(mean(x^2))), col = 3, type = "o", pch = 20)
lines(5:20, apply(classical_10, 2, function(x) sqrt(mean(x^2))), col = 4, type = "o", pch = 20)
lines(5:20, apply(classical_15, 2, function(x) sqrt(mean(x^2))), col = 5, type = "o", pch = 20)
lines(5:20, apply(classical_25, 2, function(x) sqrt(mean(x^2))), col = 6, type = "o", pch = 20)
lines(5:20, apply(ec, 2, function(x) sqrt(mean(x^2))), col = 1, type = "o", pch = 20)
title("Asymmetric Case (Entropy)")


load("mixture_sample_entropy.RData")
plot(5:20, apply(knn_bw, 2, function(x) sqrt(mean(x^2))), col = 8, ylab = "RMSE", xlab = "Dimensionality", type = 'o', pch = 20, lty = 1)
lines(5:20, apply(classical_1, 2, function(x) sqrt(mean(x^2))), col = 2, type = "o", pch = 20)
lines(5:20, apply(classical_5, 2, function(x) sqrt(mean(x^2))), col = 3, type = "o", pch = 20)
lines(5:20, apply(classical_10, 2, function(x) sqrt(mean(x^2))), col = 4, type = "o", pch = 20)
lines(5:20, apply(classical_15, 2, function(x) sqrt(mean(x^2))), col = 5, type = "o", pch = 20)
lines(5:20, apply(classical_25, 2, function(x) sqrt(mean(x^2))), col = 6, type = "o", pch = 20)
lines(5:20, apply(ec, 2, function(x) sqrt(mean(x^2))), col = 1, type = "o", pch = 20)
title("Mixture Case (Entropy)")


load("sym_sample_mi.RData")
plot(5:20, apply(LNC, 2, function(x) sqrt(mean(x^2))), col = 3, ylab = "RMSE", xlab = "Dimensionality", type = 'o', pch = 2, lty = 1, log = "y")
plot(5:20, apply(knn_bw, 2, function(x) sqrt(mean(x^2))), col = 8, ylab = "RMSE", xlab = "Dimensionality", type = 'o', pch = 20, lty = 1, ylim = c(0, 13))
lines(5:20, apply(classical_1, 2, function(x) sqrt(mean(x^2))), col = 2, type = "o", pch = 20)
lines(5:20, apply(classical_5, 2, function(x) sqrt(mean(x^2))), col = 3, type = "o", pch = 20)
lines(5:20, apply(classical_10, 2, function(x) sqrt(mean(x^2))), col = 4, type = "o", pch = 20)
lines(5:20, apply(classical_15, 2, function(x) sqrt(mean(x^2))), col = 5, type = "o", pch = 20)
lines(5:20, apply(classical_25, 2, function(x) sqrt(mean(x^2))), col = 6, type = "o", pch = 20)
lines(5:20, apply(KSG, 2, function(x) sqrt(mean(x^2))), col = 7, type = "o", pch = 20)
lines(5:20, apply(LNC, 2, function(x) sqrt(mean(x^2))), col = 3, type = "o", pch = 2)
lines(5:20, apply(ec, 2, function(x) sqrt(mean(x^2))), col = 1, type = "o", pch = 20)
title("Symmetric Case (Mutual Information)")
legend(5, 12, c("EC-kNN (k = 25)", "kNN (k = 1)",  "kNN (k = 5)", "kNN (k = 10)", "kNN (k = 15)", "kNN (k = 25)", "KSG",  "kNN bw", "LNC"),
       pch = c(rep(20, 8), 2), lty = 1, col = c(1, 2, 3, 4, 5, 6, 7, 8, 8))


load("asym_sample_mi.RData")
plot(5:20, apply(LNC, 2, function(x) sqrt(mean(x^2))), col = 3, ylab = "RMSE", xlab = "Dimensionality", type = 'o', pch = 2, lty = 1, log = "y")
plot(5:20, apply(knn_bw, 2, function(x) sqrt(mean(x^2))), col = 8, ylab = "RMSE", xlab = "Dimensionality", type = 'o', pch = 20, lty = 1, ylim = c(0, 20))
lines(5:20, apply(classical_1, 2, function(x) sqrt(mean(x^2))), col = 2, type = "o", pch = 20)
lines(5:20, apply(classical_5, 2, function(x) sqrt(mean(x^2))), col = 3, type = "o", pch = 20)
lines(5:20, apply(classical_10, 2, function(x) sqrt(mean(x^2))), col = 4, type = "o", pch = 20)
lines(5:20, apply(classical_15, 2, function(x) sqrt(mean(x^2))), col = 5, type = "o", pch = 20)
lines(5:20, apply(classical_25, 2, function(x) sqrt(mean(x^2))), col = 6, type = "o", pch = 20)
lines(5:20, apply(KSG, 2, function(x) sqrt(mean(x^2))), col = 7, type = "o", pch = 20)
lines(5:20, apply(LNC, 2, function(x) sqrt(mean(x^2))), col = 3, type = "o", pch = 2)
lines(5:20, apply(ec, 2, function(x) sqrt(mean(x^2))), col = 1, type = "o", pch = 20)
title("Asymmetric Case (Mutual Information)")


load("mixture_sample_mi.RData")
plot(5:20, apply(LNC, 2, function(x) sqrt(mean(x^2))), col = 3, ylab = "RMSE", xlab = "Dimensionality", type = 'o', pch = 2, lty = 1, log = "y")
plot(5:20, apply(knn_bw, 2, function(x) sqrt(mean(x^2, na.rm = T))), col = 8, ylab = "RMSE", xlab = "Dimensionality", type = 'o', pch = 20, lty = 1, ylim = c(0, 24))
lines(5:20, apply(classical_1, 2, function(x) sqrt(mean(x^2))), col = 2, type = "o", pch = 20)
lines(5:20, apply(classical_5, 2, function(x) sqrt(mean(x^2))), col = 3, type = "o", pch = 20)
lines(5:20, apply(classical_10, 2, function(x) sqrt(mean(x^2))), col = 4, type = "o", pch = 20)
lines(5:20, apply(classical_15, 2, function(x) sqrt(mean(x^2))), col = 5, type = "o", pch = 20)
lines(5:20, apply(classical_25, 2, function(x) sqrt(mean(x^2))), col = 6, type = "o", pch = 20)
lines(5:20, apply(KSG, 2, function(x) sqrt(mean(x^2))), col = 7, type = "o", pch = 20)
lines(5:20, apply(LNC, 2, function(x) sqrt(mean(x^2))), col = 3, type = "o", pch = 2)
lines(5:20, apply(ec, 2, function(x) sqrt(mean(x^2))), col = 1, type = "o", pch = 20)
title("Mixture Case (Mutual Information)")



