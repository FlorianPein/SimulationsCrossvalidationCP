setwd("~/Desktop/Florian-Rajen/Cross-validation/Simulations")

tab1 <- readRDS(file = paste("results/Blocks2028t5.RDS", sep = ""))
tab2 <- readRDS(file = paste("results/Blocks2028expm1.RDS", sep = ""))

tab <- cbind(tab1[, -1], tab2[, -1])
tab <- tab[c(5, 1:3, 7, 10, 11, 14, 16), ]
methods <- c("$5$-fold $\\operatorname{CV}_{(1)}$", "$\\operatorname{COPSS}$",
             "$\\operatorname{CV}_{(1)}$", "$\\operatorname{CV}_{\\operatorname{mod}}$",
             "$\\operatorname{PELT}$", "$\\operatorname{WBS}$", "$\\operatorname{FDRSeg}$",
             "$\\operatorname{Ms.FPOP}$", "$\\operatorname{Biweight}$")
tab <- cbind(methods, as.data.frame(tab))
colnames(tab) <- c("Method", rep(c("$\\hat{K} < K$","$\\hat{K} = K$", "$\\hat{K} > K$", "$\\operatorname{MISE}$"), 2))
tab <- xtable::xtable(tab, digits = rep(4, 2 + 4 * 2), auto = TRUE, display = c("g", "s", rep(c("g","g","g","g"), 2)),
              align = c("l", "|l|", rep(c("c", "c", "c", "c|"), 2)),
              caption = "Heavy Tailed ...")
print(tab, include.rownames = FALSE, file = paste("results/combined_HeavyTailed.tex", sep = ""),
      append = FALSE, sanitize.text.function=function(x){x})  