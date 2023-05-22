
setwd("~/Desktop/Florian-Rajen/Cross-validation/Simulations")
source("methods.R")

set.seed(1)
M <- 1e4

n <- 150
cp <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150)
value <- c(1:15)
signal <- numeric(n)
for (k in 1:(length(cp) - 1))
  signal[(cp[k] + 1):(cp[k + 1])] <- value[k]
sigma <- 0.3

# Y <- rnorm(n, signal, 7)
# pdf("signalBlocks.pdf", height = 6, width = 12)
# par(mar = c(2.5, 2.5, 0.5, 0.5))
# plot(Y, pch = 16, col = "grey50", xlab = "", ylab = "", cex.axis = 1.5)
# lines(signal, type = "s", lwd = 3)
# dev.off()

seed <- 21e4


namesMethod <- c("copps", "CV1", "CVmod", "CV2fold", "CV5fold", "nonAdaptiveCV5fold",
                 "pelt", "binseg", "sbs", "wbs", "fdrseg", "smuce", "hsmuce", "msfpop", "huber", "biweight")
est <- list()
mse <- list()

q <- FDRSeg::simulQuantile(0.9, n = n, type = "fdrseg")

for (i in 1:M) {
  if (i %% 10 == 1) {
    print(i)
  }
  
  set.seed(seed + i)
  Y <- rnorm(n, signal, sigma)
  
  ret <- estimating(Y = Y, q = q, maxK = 30, fit = TRUE, slow = FALSE)
  for (nameM in namesMethod) {
    est[[nameM]][i] <- ret[[nameM]]
    mse[[nameM]][i] <- mean((signal - ret$fit[[nameM]])^2)
  }
}

saveRDS(est, file = paste("results/EstStairs10.RDS", sep = ""))
saveRDS(mse, file = paste("results/MseStairs10.RDS", sep = ""))

K <- 14
library(xtable)
library(xtable)
tab <- matrix(0, length(namesMethod), 4)
for (i in seq_along(namesMethod)) {
  namesM <- namesMethod[i]
  tab[i, ] <- c(sum(est[[namesM]] < K), sum(est[[namesM]] == K), sum(est[[namesM]] > K), sum(mse[[namesM]]) / 100)
}
tab <- tab / M * 100

# methods <- c("COPSS", "COPPS L1", "COPPS new", "2-fold CV", "5-fold CV", "10-fold CV", "20-fold CV", "LOOCV", "5-fold CV L2",
#              "PELT", "BinSeg", "SBS", "WBS", "FDRseg", "Smuce")
tab <- cbind(namesMethod, as.data.frame(tab))
colnames(tab) <- c("Method", "$\\hat{K} < K$","$\\hat{K} = K$", "$\\hat{K} > K$", "MISE")
tab <- xtable(tab, digits = rep(4, 6), auto = TRUE, display = c("g","s","g","g","g", "g"),
              align = c("l", "l|", "c", "c", "c", "c"),
              caption = "test")
saveRDS(tab, file = paste("results/Stairs10.RDS", sep = ""))
print(tab, include.rownames = FALSE, file = paste("results/Stairs10.tex", sep = ""),
      append = FALSE, sanitize.text.function=function(x){x})  

