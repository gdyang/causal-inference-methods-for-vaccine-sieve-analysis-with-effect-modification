## five highest correlations
screen.corRank.top5 <- function(rank = 5, ...){
  screen.corRank(..., rank = rank)
}

## ten highest correlations
screen.corRank.top10 <- function(rank = 10, ...){
  screen.corRank(..., rank = rank)
}

## select only trt and site
screen.TrtSite <- function (Y, X, family, obsWeights, id, ...) {
  sitenames <- grep("site", colnames(X), value = T)
  whichVariable <- rep(FALSE, ncol(X))
  whichVariable[which(colnames(X) %in% c("trt", sitenames))] <- T
  return(whichVariable)
}

## select only trt, site and time
screen.TrtSiteTime<- function (Y, X, family, obsWeights, id, ...) {
  sitenames <- grep("site", colnames(X), value = T)
  whichVariable <- rep(FALSE, ncol(X))
  whichVariable[which(colnames(X) %in% c("trt", "t", sitenames))] <- T
  return(whichVariable)
}

## select only trt
screen.Trt <- function (Y, X, family, obsWeights, id, ...) {
  whichVariable <- rep(FALSE, ncol(X))
  whichVariable[which(colnames(X) == c("trt") )] <- T
  return(whichVariable)
}

### random forest with ntree = 1000 and mtry = 5
SL.randomForest1 <- function(ntree = 1000, mtry = 5, ...){
  library(randomForest)
  SL.randomForest(..., mtry = mtry, ntree = ntree)
}

### xgboost with max_depth = 2 and ntrees = 1000
SL.xgboost1 <- function(..., ntrees = 1000, max_depth = 2, shrinkage=0.1, minobspernode=10) {
  library(xgboost)
  SL.xgboost(..., ntrees = ntrees, max_depth = max_depth, shrinkage=shrinkage, minobspernode=minobspernode)
}

### GAM with df = 3

SL.gam3 <- function(..., deg.gam = 3){
  library(gam)
  SL.gam(..., deg.gam = 3)
}


### Get inference

getInference <- function(est, var, sig.level = 0.05){

  cutoff<- qnorm(sig.level/2, lower.tail=F)
 
  # standard error (square root of the variance)
  se<- sqrt(diag(var))
  
  # test statistic and pvalue
  tstat <- est/se
  
  # 95% confidence interval
  CI.lo <- est - cutoff*se
  CI.hi <-  est + cutoff*se
  # p-value
  pval <- 2*pnorm(abs(tstat), lower.tail=F)
  
  data.frame(tstat, se, CI.lo, CI.hi, pval)
}
