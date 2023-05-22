
setwd("~/Desktop/Florian-Rajen/Cross-validation/Simulations")
source("methods.R")

set.seed(1)
M <- 1e4

delta <- 20
Delta <- 3.5 * delta
lambda <- 5
signal <- c(rep(delta - 15 + 10.98 - 14.64 - 3.66, 204), rep(delta - 15 + 7.32 + 3.66, 266), rep(delta - 15, 308),
            rep(delta, 100), rep(0, lambda), rep(Delta, 11), rep(0, 90),
            rep(-15, 430), rep(-7.32, 224), rep(8.42, 42), rep(-2.93, 60), rep(4.76, 308))
n <- length(signal)

Y <- rnorm(n, signal, 7)
pdf("signalUnderestimationL2.pdf", height = 6, width = 12)
par(mar = c(2.5, 2.5, 0.5, 0.5))
plot(Y, pch = 16, col = "grey50", xlab = "", ylab = "", cex.axis = 1.5)
lines(signal, type = "s", lwd = 3)
dev.off()

sigma <- 7
seed <- 8e4


namesMethod <- c("copps", "CV1", "CVmod", "CV2fold", "CV5fold", "nonAdaptiveCV5fold",
                 "pelt", "binseg", "sbs", "wbs", "fdrseg", "smuce", "hsmuce", "msfpop", "huber", "biweight")
est <- list()
mse <- list()

q <- readRDS("qFDRSeg2048.RDS")

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

saveRDS(est, file = paste("results/EstUnderestimationL2.RDS", sep = ""))
saveRDS(mse, file = paste("results/MseUnderestimationL2.RDS", sep = ""))

K <- 11
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
saveRDS(tab, file = paste("results/UnderestimationL2.RDS", sep = ""))
print(tab, include.rownames = FALSE, file = paste("results/UnderestimationL2.tex", sep = ""),
      append = FALSE, sanitize.text.function=function(x){x})  

