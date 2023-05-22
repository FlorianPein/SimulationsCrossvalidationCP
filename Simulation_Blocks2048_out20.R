
setwd("~/Desktop/Florian-Rajen/Cross-validation/Simulations")
source("methods.R")

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

seed <- 6e4


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
  indices <- sample.int(n, 10, replace = FALSE)
  Y[indices] <- Y[indices] + rpois(10, 20)
  
  ret <- estimating(Y = Y, q = q, maxK = 30, fit = TRUE, slow = FALSE)
  for (nameM in namesMethod) {
    est[[nameM]][i] <- ret[[nameM]]
    mse[[nameM]][i] <- mean((signal - ret$fit[[nameM]])^2)
  }
}

saveRDS(est, file = paste("results/EstBlocks2028out20.RDS", sep = ""))
saveRDS(mse, file = paste("results/MseBlocks2028out20.RDS", sep = ""))

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
saveRDS(tab, file = paste("results/Blocks2028out20.RDS", sep = ""))
print(tab, include.rownames = FALSE, file = paste("results/Blocks2028out20.tex", sep = ""),
      append = FALSE, sanitize.text.function=function(x){x})  

