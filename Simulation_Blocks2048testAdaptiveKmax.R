
setwd("~/Desktop/Florian-Rajen/Cross-validation/Simulations")

estimating <- function(Y) {
  n <- length(Y)
  
  ret <- list()
  
  retM <- crossvalidationCP::VfoldCV(Y, V = 5L, Kmax = 30L, adaptiveKmax = FALSE, estimator = crossvalidationCP::leastSquares, output = "fit")
  cps <- retM$fit$cps
  fitM <- numeric(n)
  for (l in seq_along(cps)[-1]) {
    fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
  }
  ret$fit$nonAdaptive <- fitM
  ret$nonAdaptive <- length(cps) - 2
  
  for (Kmax in 5:14) {
    retM <- crossvalidationCP::VfoldCV(Y, V = 5L, Kmax = Kmax, adaptiveKmax = TRUE, estimator = crossvalidationCP::leastSquares, output = "fit")
    cps <- retM$fit$cps
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
    }
    ret$fit[[paste0("Kmax", Kmax)]] <- fitM
    ret[[paste0("Kmax", Kmax)]] <- length(cps) - 2
  }

  ret
}

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

seed <- 1e6

est <- list()
mse <- list()


for (i in 1:M) {
  # if (i %% 10 == 1) {
    print(i)
  # }
  
  set.seed(seed + i)
  Y <- rnorm(n, signal, sigma)
  
  ret <- estimating(Y = Y)
  for (nameM in names(ret)[-1]) {
    est[[nameM]][i] <- ret[[nameM]]
    mse[[nameM]][i] <- mean((signal - ret$fit[[nameM]])^2)
  }
}

namesMethod <- names(ret)[-1]

saveRDS(est, file = paste("results/EstBlocks2028testAdaptiveKmax.RDS", sep = ""))
saveRDS(mse, file = paste("results/MseBlocks2028testAdaptiveKmax.RDS", sep = ""))

K <- 11
library(xtable)
library(xtable)
tab <- matrix(0, length(namesMethod), 4)
for (i in seq_along(namesMethod)) {
  namesM <- namesMethod[i]
  tab[i, ] <- c(sum(est[[namesM]] < K), sum(est[[namesM]] == K), sum(est[[namesM]] > K), sum(mse[[namesM]]) / 100)
}
tab <- tab / M * 100

tab <- cbind(namesMethod, as.data.frame(tab))
colnames(tab) <- c("Method", "$\\hat{K} < K$","$\\hat{K} = K$", "$\\hat{K} > K$", "MISE")
tab <- xtable(tab, digits = rep(4, 6), auto = TRUE, display = c("g","s","g","g","g", "g"),
              align = c("l", "l|", "c", "c", "c", "c"),
              caption = "test")
saveRDS(tab, file = paste("results/Blocks2028testAdaptiveKmax.RDS", sep = ""))
print(tab, include.rownames = FALSE, file = paste("results/Blocks2028testAdaptiveKmax.tex", sep = ""),
      append = FALSE, sanitize.text.function=function(x){x})  

