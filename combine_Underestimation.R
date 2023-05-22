
setwd("~/Desktop/Florian-Rajen/Cross-validation/Simulations")

tab2 <- readRDS(file = paste("results/under_D_2.RDS", sep = ""))
tab3 <- readRDS(file = paste("results/under_D_3.RDS", sep = ""))
tab5 <- readRDS(file = paste("results/under_D_5.RDS", sep = ""))

tab <- cbind(tab5[, -1], tab3[, -1])
tab <- tab[c(8:11, 5:7, 3:4, 13, 16, 17, 20, 22), ]
methods <- c("$2$-fold $\\operatorname{CV}_{(1)}$", "$5$-fold $\\operatorname{CV}_{(1)}$",
             "$10$-fold $\\operatorname{CV}_{(1)}$", "$20$-fold $\\operatorname{CV}_{(1)}$",
             "$\\operatorname{COPSS}$", "$\\operatorname{CV}_{(1)}$", "$\\operatorname{CV}_{\\operatorname{mod}}$",
             "$\\operatorname{LooVF}_2$", "$\\operatorname{LooVF}_5$",
             "$\\operatorname{PELT}$", "$\\operatorname{WBS}$", "$\\operatorname{FDRSeg}$",
             "$\\operatorname{Ms.FPOP}$", "$\\operatorname{Biweight}$")
tab <- cbind(methods, as.data.frame(tab))
colnames(tab) <- c("Method", rep(c("$\\hat{K} < K$","$\\hat{K} = K$", "$\\hat{K} > K$", "$\\operatorname{MISE}$"), 2))
tab <- xtable::xtable(tab, digits = rep(4, 2 + 4 * 2), auto = TRUE, display = c("g", "s", rep(c("g","g","g","g"), 2)),
              align = c("l", "|l|", rep(c("c", "c", "c", "c|"), 2)),
              caption = "Under I ...")
print(tab, include.rownames = FALSE, file = paste("results/Under1.tex", sep = ""),
      append = FALSE, sanitize.text.function=function(x){x})  



tab <- cbind(tab2[, -1])
tab <- tab[c(8:11, 5:7, 3:4, 13, 16, 17, 20, 22), ]
methods <- c("$2$-fold $\\operatorname{CV}_{(1)}$", "$5$-fold $\\operatorname{CV}_{(1)}$",
             "$10$-fold $\\operatorname{CV}_{(1)}$", "$20$-fold $\\operatorname{CV}_{(1)}$",
             "$\\operatorname{COPSS}$", "$\\operatorname{CV}_{(1)}$", "$\\operatorname{CV}_{\\operatorname{mod}}$",
             "$\\operatorname{LooVF}_2$", "$\\operatorname{LooVF}_5$",
             "$\\operatorname{PELT}$", "$\\operatorname{WBS}$", "$\\operatorname{FDRSeg}$",
             "$\\operatorname{Ms.FPOP}$", "$\\operatorname{Biweight}$")
tab <- cbind(methods, as.data.frame(tab))
colnames(tab) <- c("Method", rep(c("$\\hat{K} < K$","$\\hat{K} = K$", "$\\hat{K} > K$", "$\\operatorname{MISE}$"), 1))
tab <- xtable::xtable(tab, digits = rep(4, 2 + 4 * 1), auto = TRUE, display = c("g", "s", rep(c("g","g","g","g"), 1)),
              align = c("l", "|l|", rep(c("c", "c", "c", "c|"), 1)),
              caption = "Under II ...")
print(tab, include.rownames = FALSE, file = paste("results/Under2.tex", sep = ""),
      append = FALSE, sanitize.text.function=function(x){x})  


