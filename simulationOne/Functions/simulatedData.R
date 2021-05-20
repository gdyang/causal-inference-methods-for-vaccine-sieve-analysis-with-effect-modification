total_hazard <- function(w1, w2, trt){
  plogis(-2 + 0.4*((w2 == 0) + (w2 == 1) + (w2 == 2)) - 0.2*(w2 == 3)- 0.5*w1*trt - 0.5*trt)
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
