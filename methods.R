
estimating <- function(Y, q = FDRSeg::simulQuantile(0.9, n = length(Y), type = "fdrseg"), maxK = 5, fit = FALSE, slow = TRUE) {
  retWBS <- wbs::changepoints(wbs::wbs(Y), Kmax = maxK)
  sd <- retWBS$sigma
  n <- length(Y)
  
  if (fit) {
    if(slow) {
      ret <- list(copps = NA, CV1 = NA, CVmod = NA, CV2fold = NA, CV5fold = NA, CV10fold = NA, CV20fold = NA, nonAdaptiveCV5fold = NA,
                  pelt = NA, binseg = NA, sbs = NA, wbs = NA, fdrseg = NA, smuce = NA, hsmuce = NA, msfpop = NA, huber = NA, biweight = NA,
                  fit = list())
    } else {
      ret <- list(copps = NA, CV1 = NA, CVmod = NA, CV2fold = NA, CV5fold = NA, nonAdaptiveCV5fold = NA,
                  pelt = NA, binseg = NA, sbs = NA, wbs = NA, fdrseg = NA, smuce = NA, hsmuce = NA, msfpop = NA, huber = NA, biweight = NA,
                  fit = list())
    }

    
    retM <- crossvalidationCP::COPPS(Y, param = maxK, estimator = crossvalidationCP::leastSquares, output = "fit")
    cps <- retM$fit$cps
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
    }
    ret$copps <- length(cps) - 2
    ret$fit$copps <- fitM
    
    retM <- crossvalidationCP::CV1(Y, param = maxK, estimator = crossvalidationCP::leastSquares, output = "fit")
    cps <- retM$fit$cps
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
    }
    ret$CV1 <- length(cps) - 2
    ret$fit$CV1 <- fitM
    
    retM <- crossvalidationCP::CVmod(Y, param = maxK, estimator = crossvalidationCP::leastSquares, output = "fit")
    cps <- retM$fit$cps
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
    }
    ret$CVmod <- length(cps) - 2
    ret$fit$CVmod <- fitM
    
    retM <- crossvalidationCP::VfoldCV(Y, V = 2L, estimator = crossvalidationCP::leastSquares, output = "fit")
    cps <- retM$fit$cps
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
    }
    ret$CV2fold <- length(cps) - 2
    ret$fit$CV2fold <- fitM
    
    retM <- crossvalidationCP::VfoldCV(Y, V = 5L, estimator = crossvalidationCP::leastSquares, output = "fit")
    cps <- retM$fit$cps
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
    }
    ret$CV5fold <- length(cps) - 2
    ret$fit$CV5fold <- fitM
    
    if (slow) {
      retM <- crossvalidationCP::VfoldCV(Y, V = 10L, estimator = crossvalidationCP::leastSquares, output = "fit")
      cps <- retM$fit$cps
      fitM <- numeric(n)
      for (l in seq_along(cps)[-1]) {
        fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
      }
      ret$CV10fold <- length(cps) - 2
      ret$fit$CV10fold <- fitM
      
      
      retM <- crossvalidationCP::VfoldCV(Y, V = 20L, estimator = crossvalidationCP::leastSquares, output = "fit")
      cps <- retM$fit$cps
      fitM <- numeric(n)
      for (l in seq_along(cps)[-1]) {
        fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
      }
      ret$CV20fold <- length(cps) - 2
      ret$fit$CV20fold <- fitM
    }

    retM <- crossvalidationCP::VfoldCV(Y, V = 5L, Kmax = maxK, adaptiveKmax = FALSE, estimator = crossvalidationCP::leastSquares, output = "fit")
    cps <- retM$fit$cps
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
    }
    ret$fit$nonAdaptiveCV5fold <- fitM
    ret$nonAdaptiveCV5fold <- length(cps) - 2
    
    retM <- changepoint::cpt.mean(data = Y / sd, penalty = "SIC", method = "PELT")
    cps <- c(0, retM@cpts)
    values <- retM@param.est$mean * sd
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- values[l - 1]
    }
    ret$pelt <- length(cps) - 2
    ret$fit$pelt <- fitM
    
    retM <- changepoint::cpt.mean(data = Y / sd, penalty = "SIC", method = "BinSeg", Q = maxK)
    cps <- c(0, retM@cpts)
    values <- retM@param.est$mean * sd
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- values[l - 1]
    }
    ret$binseg <- length(cps) - 2
    ret$fit$binseg <- fitM
    
    retM <- wbs::changepoints(wbs::sbs(Y))
    cps <- c(0, sort(retM$cpt.th[[1]]), n)
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
    }
    ret$sbs <- length(cps) - 2
    ret$fit$sbs <- fitM
    
    retM <- retWBS
    cps <- c(0, sort(retM$cpt.th[[1]]), n)
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
    }
    ret$wbs <- length(cps) - 2
    ret$fit$wbs <- fitM
    
    retM <- FDRSeg::fdrseg(Y, q = q)
    cps <- c(retM$left - 1, n)
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- retM$value[l - 1]
    }
    ret$fdrseg <- length(cps) - 2
    ret$fit$fdrseg <- fitM
    
    retM <- stepR::stepFit(Y, alpha = 0.05)
    ret$smuce <- length(retM$leftIndex) - 1
    ret$fit$smuce <- fitted(retM)
    
    retM <- stepR::stepFit(Y, alpha = 0.05, family = "hsmuce")
    ret$hsmuce <- length(retM$leftIndex) - 1
    ret$fit$hsmuce <- fitted(retM)
    
    retM <- MsFPOP::MsFPOP(y = Y / sd, alpha = 9 + 2.25 * log(n), beta = 2.25)
    cps <- c(0, retM$changepoints)
    fitM <- numeric(n)
    for (l in seq_along(cps)[-1]) {
      fitM[(cps[l - 1] + 1):cps[l]] <- mean(Y[(cps[l - 1] + 1):cps[l]])
    }
    ret$msfpop <- length(cps) - 2
    ret$fit$msfpop <- fitM
    
    retM <- robseg::Rob_seg.std(Y / sd,  "Huber", lambda = 1.4 * log(n), lthreshold = 1.345)
    ret$huber <- retM$K - 1
    ret$fit$huber <- retM$smt * sd

    retM <- robseg::Rob_seg.std(Y / sd,  "Outlier", lambda = 2 * log(n), lthreshold = 3)
    ret$biweight <- retM$K - 1
    ret$fit$biweight <- retM$smt * sd
    
  } else {
    if(slow) {
      ret <- list(copps = crossvalidationCP::COPPS(Y, param = maxK, estimator = crossvalidationCP::leastSquares),
                  CV1 = crossvalidationCP::CV1(Y, param = maxK, estimator = crossvalidationCP::leastSquares),
                  CVmod = crossvalidationCP::CVmod(Y, param = maxK, estimator = crossvalidationCP::leastSquares),
                  CV2fold = crossvalidationCP::VfoldCV(Y, V = 2L, estimator = crossvalidationCP::leastSquares),
                  CV5fold = crossvalidationCP::VfoldCV(Y, V = 5L, estimator = crossvalidationCP::leastSquares),
                  CV10fold = crossvalidationCP::VfoldCV(Y, V = 10L, estimator = crossvalidationCP::leastSquares),
                  CV20fold = crossvalidationCP::VfoldCV(Y, V = 20L, estimator = crossvalidationCP::leastSquares),
                  nonAdaptiveCV5fold = crossvalidationCP::VfoldCV(Y, V = 5L, Kmax = maxK, adaptiveKmax = FALSE, estimator = crossvalidationCP::leastSquares),
                  pelt = length(changepoint::cpt.mean(data = Y / sd, penalty = "SIC", method = "PELT")@cpts) - 1,
                  binseg = length(changepoint::cpt.mean(data = Y / sd, penalty = "SIC", method = "BinSeg", Q = maxK)@cpts) - 1,
                  sbs = wbs::changepoints(wbs::sbs(Y))$no.cpt.th,
                  wbs = retWBS$no.cpt.th,
                  fdrseg = length(FDRSeg::fdrseg(Y, q = q)$left) - 1,
                  smuce = length(stepR::stepFit(Y, alpha = 0.05)$leftIndex) - 1,
                  hsmuce = length(stepR::stepFit(Y, alpha = 0.05)$leftIndex) - 1,
                  msfpop = length(MsFPOP::MsFPOP(y = Y, alpha = 9 + 2.25 * log(n), beta = 2.25)) - 1,
                  huber = Rob_seg.std(Y / sd,  "Huber", lambda = 1.4 * log(n), lthreshold = 1.345)$K - 1,
                  biweight = Rob_seg.std(Y / sd,  "Outlier", lambda = 2 * log(n), lthreshold = 3)$K - 1)
    } else {
      ret <- list(copps = crossvalidationCP::COPPS(Y, param = maxK, estimator = crossvalidationCP::leastSquares),
                  CV1 = crossvalidationCP::CV1(Y, param = maxK, estimator = crossvalidationCP::leastSquares),
                  CVmod = crossvalidationCP::CVmod(Y, param = maxK, estimator = crossvalidationCP::leastSquares),
                  CV2fold = crossvalidationCP::VfoldCV(Y, V = 2L, estimator = crossvalidationCP::leastSquares),
                  CV5fold = crossvalidationCP::VfoldCV(Y, V = 5L, estimator = crossvalidationCP::leastSquares),
                  nonAdaptiveCV5fold = crossvalidationCP::VfoldCV(Y, V = 5L, Kmax = maxK, adaptiveKmax = FALSE, estimator = crossvalidationCP::leastSquares),
                  pelt = length(changepoint::cpt.mean(data = Y / sd, penalty = "SIC", method = "PELT")@cpts) - 1,
                  binseg = length(changepoint::cpt.mean(data = Y / sd, penalty = "SIC", method = "BinSeg", Q = maxK)@cpts) - 1,
                  sbs = wbs::changepoints(wbs::sbs(Y))$no.cpt.th,
                  wbs = retWBS$no.cpt.th,
                  fdrseg = length(FDRSeg::fdrseg(Y, q = q)$left) - 1,
                  smuce = length(stepR::stepFit(Y, alpha = 0.05)$leftIndex) - 1,
                  hsmuce = length(stepR::stepFit(Y, alpha = 0.05)$leftIndex) - 1,
                  msfpop = length(MsFPOP::MsFPOP(y = Y, alpha = 9 + 2.25 * log(n), beta = 2.25)) - 1,
                  huber = Rob_seg.std(Y / sd,  "Huber", lambda = 1.4 * log(n), lthreshold = 1.345)$K - 1,
                  biweight = Rob_seg.std(Y / sd,  "Outlier", lambda = 2 * log(n), lthreshold = 3)$K - 1)
    }
  }
  ret
}
