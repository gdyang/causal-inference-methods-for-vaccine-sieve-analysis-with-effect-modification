getTruth <- function(n = 1e5, t0 = 6, prob = 0.25, equal = TRUE, msm.formula = "trt*w2", msm.family){
  w1 = rnorm(n, 0, 1); w2 = rbinom(n,4,0.5)
  adjustVars <- data.frame(w1 = w1, w2 = w2)
  
  stackedTrt <- rep(0:1, each = n*6 )
  stackedWeights <- rep(1, length(stackedTrt))
  stackedAdjustVars <- rbind(adjustVars[rep(seq_len(n),  6),], adjustVars[rep(seq_len(n), 6),])
  stackedFtype <- c(rep(1:6, each = n), rep(1:6, each = n))
  stackedData <- data.frame(#weights = stackedWeights,
    stackedAdjustVars,trt = stackedTrt,
    ftype = stackedFtype)
  stackedData$ci <- apply(stackedData, 1, function(x) getTrueCumInc(w1 = x[1], w2 = x[2], trt = x[3], ftype = x[4]))
  fit <- glm(as.formula(paste0("ci ~", msm.formula)), family = msm.family,
             weights = stackedData$weights, 
             data = stackedData)
  return(fit$coefficients)
}