
setwd("~/Desktop/Florian-Rajen/Cross-validation/Simulations")
source("methods.R")
maxK <- 30

M <- 1e4
name <- "arlot_s1_pc3"
n <- 100
signal <- rep(1, 100)
signal[21:40] <- 0
signal[61:80] <- 0
sigma <- rep(0.2, 100) * 3
sigma[34:100] <- 0.05 * 3
seed <- 1e6 + 2e4

Y <- matrix(0, M, n)
for (i in 1:M) {
  if (i %% 10 == 1) {
    print(i)
  }
  
  set.seed(seed + i)
  Y[i, ] <- rnorm(n, signal, sigma)
}
R.matlab::writeMat(paste0("matData/", name, "/Data.mat"), y = Y)

#####
# run run.m in Matlab before continuing
#####

namesMethod <- c("arlotV2", "arlotV5")
abbr <- c("V2", "V5")
est <- list()
mse <- list()

est$arlotV2 <- as.numeric(R.matlab::readMat(paste0("matData/", name, "/numberV2.mat"))$numberjumpsV2)
est$arlotV5 <- as.numeric(R.matlab::readMat(paste0("matData/", name, "/numberV5.mat"))$numberjumpsV5)


for (i in seq_along(abbr)) {
  values <- R.matlab::readMat(paste0("matData/", name, "/values", abbr[i], ".mat"))[[paste0("values", abbr[i])]]
  jumps <- R.matlab::readMat(paste0("matData/", name, "/jumps", abbr[i], ".mat"))[[paste0("jumps", abbr[i])]]
  
  mse[[namesMethod[i]]] <- numeric(M)
  
  for (j in 1:M) {
    cps <- c(0, jumps[j, jumps[j, ] != 0], n)
    fit <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fit[(cps[l - 1] + 1):cps[l]] <- values[j, l - 1]
    }
    mse[[namesMethod[i]]][j] <- mean((signal - fit)^2)
  }
}

namesMethod2 <- c("copps", "CV1", "CVmod", "CV2fold", "CV5fold", "CV10fold", "CV20fold", "nonAdaptiveCV5fold",
                  "pelt", "binseg", "sbs", "wbs", "fdrseg", "smuce", "hsmuce", "msfpop", "huber", "biweight")
namesMethod <- c(namesMethod, namesMethod2)

q <- FDRSeg::simulQuantile(0.9, n = n, type = "fdrseg")

for (i in 1:M) {
  if (i %% 10 == 1) {
    print(i)
  }
  
  set.seed(seed + i)
  Y <- rnorm(n, signal, sigma)
  
  ret <- estimating(Y = Y, q = q, maxK = maxK, fit = TRUE)
  for (nameM in namesMethod2) {
    est[[nameM]][i] <- ret[[nameM]]
    mse[[nameM]][i] <- mean((signal - ret$fit[[nameM]])^2)
  }
}

saveRDS(est, file = paste("results/Est_", name, ".RDS", sep = ""))
saveRDS(mse, file = paste("results/Mse_", name, ".RDS", sep = ""))


K <- 4
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
saveRDS(tab, file = paste("results/", name, ".RDS", sep = ""))
print(tab, include.rownames = FALSE, file = paste("results/", name, ".tex", sep = ""),
      append = FALSE, sanitize.text.function=function(x){x})  
