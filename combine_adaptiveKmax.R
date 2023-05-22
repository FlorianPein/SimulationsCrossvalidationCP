
setwd("~/Desktop/Florian-Rajen/Cross-validation/Simulations")

tab1 <- readRDS(file = paste("results/Blocks2028original.RDS", sep = ""))
tab2 <- readRDS(file = paste("results/Stairs10.RDS", sep = ""))

tab <- cbind(tab1[, -1], tab2[, -1])
methods <- c("$\\operatorname{COPSS}$", "$\\operatorname{CV}_{(1)}$", "$\\operatorname{CV}_{\\operatorname{mod}}$",
             "$2$-fold $\\operatorname{CV}_{(1)}$", "$5$-fold $\\operatorname{CV}_{(1)}$", "$5$-fold $\\operatorname{CV}_{(1)}^*$",
             "$\\operatorname{PELT}$", "$\\operatorname{BS}$", "$\\operatorname{SBS}$", "$\\operatorname{WBS}$",
             "$\\operatorname{FDRSeg}$", "$\\operatorname{SMUCE}$", "$\\operatorname{HSMUCE}$", "$\\operatorname{MsFPOP}$",
             "$\\operatorname{Huber}$", "$\\operatorname{Biweight}$")
tab <- cbind(methods, as.data.frame(tab))
colnames(tab) <- c("Method", rep(c("$\\hat{K} < K$","$\\hat{K} = K$", "$\\hat{K} > K$", "$\\operatorname{MISE}$"), 2))
tab <- xtable::xtable(tab, digits = rep(4, 2 + 4 * 2), auto = TRUE, display = c("g", "s", rep(c("g","g","g","g"), 2)),
              align = c("l", "|l|", rep(c("c", "c", "c", "c|"), 2)),
              caption = "Blocks ...")
print(tab, include.rownames = FALSE, file = paste("results/combined_BlocksStairs.tex", sep = ""),
      append = FALSE, sanitize.text.function=function(x){x})  
