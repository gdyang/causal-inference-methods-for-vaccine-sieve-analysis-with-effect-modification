rm(list = ls())
library(devtools)
suppressWarnings(load_all("~/survtmle"))
library(parallel)
total_hazard <- function(w1, w2, trt){
  plogis(-1.5 + 0.4*((w2 == 0) + (w2 == 1) + (w2 == 2)) - 0.2*(w2 == 3)- 0.2*w1*trt - 1*trt)
}



makeData <- function(n, prob = 0.25, equal = TRUE){
  w1 = rnorm(n, 0, 1); w2 = rbinom(n,4,0.5)
  if (equal == T){
    trt <-  rbinom(n, 1, 0.5)
  }
  else {trt <- rbinom(n, 1, prob)}
  ftype <- rbinom(n, 5, plogis(0.2*trt)) +1
  ctime <- 1 + rgeom(n, plogis(-3 + 0.2*(w2 == 2)- 0.2*(w2 == 3))) # non-random censoring
  ftime <- 1 + rgeom(n, total_hazard(w1, w2, trt))
  time <- pmin(ctime, ftime)
  ftype[ctime < ftime] <- 0
  return(list(adjustVars = data.frame(w1= w1, w2 = w2), trt = trt, ftime = time, ftype = ftype))
}


getTrueCumInc <- function(w1, w2, trt, ftype, t0 = 6){
  total_hazard <- total_hazard(w1, w2, trt)
  haz <- dbinom(ftype-1, 5, plogis(0.2*trt))*total_hazard
	ci <- haz
	for(t in 2:t0){
		ci <- ci + haz * (1 -  total_hazard)^(t-1)
	}
	return(ci)
}

getTruth <- function(n = 1e5, t0 = 6, prob = 0.25, equal = TRUE, msm.formula = "trt*w2", msm.family){
  w1 = rnorm(n, 0, 1); w2 = rbinom(n,4,0.5)
  adjustVars <- data.frame(w1 = w1, w2 = w2)
	#ci_by_ftype_control <- sapply(1:6, getTrueCumInc, trt = 0, w1 = w1, 
	#                    w2 = w2, t0 = 6,
	#                    simplify = FALSE)
	#ci_by_ftype_trt <- sapply(1:6, getTrueCumInc, trt = 1, w1 = w1, 
	#                              w2 = w2, t0 = 6,
	#                              simplify = FALSE)
	
	 #weight_by_ftype <- sapply(0:3, function(x){
	# 	rep(dbinom(x, 3, prob), n)
	# }, simplify = FALSE)
	 
	#stackedOutcome <-c(Reduce("c", ci_by_ftype_control), Reduce("c", ci_by_ftype_trt))
	#if (equal == T){
	
	#}else{
	#stackedWeights <- Reduce("c", weight_by_trt)}
	
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


SL.gam3 <- function(..., deg.gam = 3, cts.num = 5){
  SL.gam(..., deg.gam = 3, cts.num = 5)
}

SL.glm.correct.ftime <- function (Y, X, newX, family, obsWeights, ...) 
{
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  fit.glm <- glm(Y ~ trt*factor(w2)*w1, data = X, family = family, weights = obsWeights)
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}

SL.glm.correct.ctime <- function (Y, X, newX, family, obsWeights, ...) 
{
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  fit.glm <- glm(Y ~ factor(w2)*t, data = X, family = family, weights = obsWeights)
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}



do.one <- function(X, n, equal = T, truth, msm.formula = "trt*w2", msm.family){
  set.seed(X)
	dat <- makeData(n = n, equal = equal)
	
	
	SL.ftime.lib <- list(#c("SL.mean", "All"),
	                     c("SL.glm.interaction", "All"),
	                     #c("SL.earth", "All"),
	                     c("SL.glm.correct.ftime", "All"))
	
	### ctime SL Library
	SL.ctime.lib <- list(#c("SL.mean", "All"),
	                     c("SL.glm.interaction", "All"),
	                     #c("SL.gam3", "All"),
	                     c("SL.earth", "All"),
	                     c("SL.glm.correct.ctime", "All"))
	
	fit <- tryCatch({suppressWarnings(
	  
	  mean_tmle(ftime = dat$ftime, ftype = dat$ftype,
	                           trt = dat$trt, adjustVars = dat$adjustVars,
	                           glm.trt = "1", t0 = 6,	
	           # glm.ftime = "trt*factor(w2)*w1",
	           #glm.ctime = "factor(w2)*t",
	                           SL.ftime = SL.ftime.lib,
	                           SL.ctime = SL.ctime.lib,
	                           msm.formula = msm.formula,
	                           msm.family = msm.family)
	                           
	                           )
		}, error = function(e){ rep(NA, 2*length(truth)) })

	if(class(fit) == "survtmle"){
		est <- fit$est
		se <- sqrt(diag(fit$var))
		cil <- est - 1.96 * se
		ciu <- est + 1.96 * se
		coverage <- truth < ciu & truth > cil
		bias <- est - truth
		return(c(bias, se, coverage))
	}else{
		return(fit)
	}
}





msm.formula <-  "ftype*trt + trt*w1"
msm.family <- "poisson"
truth <- getTruth(msm.formula = msm.formula, msm.family = poisson() )
truth

cl <- makeCluster(20, type = "SOCK")
clusterExport(cl, "do.one")
clusterExport(cl, "SL.gam3")
clusterExport(cl, "SL.glm.correct.ftime")
clusterExport(cl, "SL.glm.correct.ctime")
clusterExport(cl, "makeData")
clusterExport(cl, "total_hazard")
clusterEvalQ(cl, devtools::load_all("~/survtmle"))
clusterEvalQ(cl, library(SuperLearner))


size = 1000

rslt_1000 <- parSapplyLB(cl = cl, X = seq_len(size), FUN = do.one, n = 1000,
                        truth = truth, msm.formula = msm.formula, msm.family = msm.family)
saveRDS(rslt_1000,"~/survtmle_sim/sim_case1/re_n1000_SL.rds")
rowMeans(rslt_1000)
rslt_2000 <- parSapplyLB(cl = cl, X = seq_len(size), FUN = do.one, n = 2000,
                         truth = truth, msm.formula = msm.formula, msm.family = msm.family)
rowMeans(rslt_2000)
saveRDS(rslt_2000,"~/survtmle_sim/sim_case1/re_n2000_SL.rds")

rslt_5000 <- parSapplyLB(cl = cl, X = seq_len(size), FUN = do.one, n = 5000,
                         truth = truth, msm.formula = msm.formula, msm.family = msm.family)
saveRDS(rslt_5000,"~/survtmle_sim/sim_case1/re_n5000_SL.rds")

rowMeans(rslt_5000)
stopCluster(cl = cl)
