
setwd("~/Desktop/Florian-Rajen/Cross-validation/Simulations")
source("methods.R")

set.seed(1)
M <- 1e4

n <- 2048
original <- c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1)
even <- rep(0, 11)
odd <- rep(1, 11)
add <- original
cp <- c(0, 204, 266, 308, 472, 512, 820, 902, 1332, 1556, 1598, 1658, 2048) + c(0, add, 0)
value <- c(0, 14.64, -3.66, 7.32, -7.32, 10.98, -4.39, 3.29, 19.03, 7.68, 15.37, 0)
signal <- numeric(n)
for (k in 1:(length(cp) - 1))
  signal[(cp[k] + 1):(cp[k + 1])] <- value[k]
sigma <- 7

Y <- rnorm(n, signal, 7)
pdf("signalBlocks.pdf", height = 6, width = 12)
par(mar = c(2.5, 2.5, 0.5, 0.5))
plot(Y, pch = 16, col = "grey50", xlab = "", ylab = "", cex.axis = 1.5)
lines(signal, type = "s", lwd = 3)
dev.off()

seed <- 1e4


namesMethod <- c("copps", "CV1", "CVmod", "CV2fold", "CV5fold", "nonAdaptiveCV5fold",
                 "pelt", "binseg", "sbs", "wbs", "fdrseg", "smuce", "hsmuce", "msfpop", "huber", "biweight")
est <- list()
mse <- list()

# q <- FDRSeg::simulQuantile(0.9, n = n, type = "fdrseg")
# saveRDS(q, "qFDRSeg2048.RDS")
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

saveRDS(est, file = paste("results/EstBlocks2028original.RDS", sep = ""))
saveRDS(mse, file = paste("results/MseBlocks2028original.RDS", sep = ""))

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
saveRDS(tab, file = paste("results/Blocks2028original.RDS", sep = ""))
print(tab, include.rownames = FALSE, file = paste("results/Blocks2028original.tex", sep = ""),
      append = FALSE, sanitize.text.function=function(x){x})  

