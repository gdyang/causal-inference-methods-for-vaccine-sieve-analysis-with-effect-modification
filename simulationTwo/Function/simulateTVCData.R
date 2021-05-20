


makeData <- function(n, trttime = c(0,2), t0 = 4, J = 3){
  varlist <- vector("list", length =  length(trttime))
  names(varlist) <- paste0("t", trttime)
  trtlist <- vector("list", length =  length(trttime))
  names(trtlist) <- paste0("t", trttime)
  
  u_N_j <- data.frame(matrix(runif(n*t0, 0 , 1), nrow = n))
  N_j <- data.frame(matrix(rep(0, n*t0), nrow = n))
  which_j <- runif(n, 0, 1)
  C <- data.frame(matrix(rep(0, n*t0), nrow = n))
  
  varlist$t0 <- data.frame(id = seq_len(n), w1 =  rbinom(n, 1, 0.3), w2 =  rbinom(n, 1, 0.7))
  trtlist$t0 <- data.frame(id = seq_len(n), trt = rbinom(n, 1, plogis(1 - 0.5*varlist$t0$w1 - varlist$t0$w2) ))
  
  w1_new <- rbinom(n, 1, ifelse(varlist$t0$w1 ==1, 0.6, 0.4))
  w2_new <- rbinom(n, 1, ifelse(varlist$t0$w2 ==1, 0.7, 0.3))
  varlist[["t2"]] <- data.frame(id = seq_len(n), w1 = w1_new, w2 = w2_new)
  trt_new <- rbinom(n, 1,  plogis(-5- 0.5*w1_new + 0.2*w2_new + 5*trtlist$t0$trt))
  trtlist[["t2"]] <- data.frame(id = seq_len(n), trt = trt_new)
  
  table(trt_new + trtlist$t0$trt)
  
  #trttime_no_base <- trttime[which(trttime !=0)]
  for (t_i in seq_len(t0)) {
    # fix hazard
    w1_t  <- varlist[[paste0("t", max(trttime[trttime < t_i]))]]$w1
    w2_t  <-  varlist[[paste0("t", max(trttime[trttime < t_i]))]]$w2
    trt_t <- trtlist[[paste0("t", max(trttime[trttime < t_i]))]]$trt
    
    
    N_j[,t_i] <- as.numeric(3*plogis(-2 + 0.5* w1_t + 0.6*w2_t - trt_t) >  u_N_j[, t_i])
    
    # make the censoring simple
    C[, t_i] <- rbinom(n, 1,  1 - plogis(2  + 0.5*w1_t))
    
  }
  
  
  ftime <- apply( N_j, 1, function(z) {
    if (sum(z) == 0){
      return(t0+2)
    }else {return(min(which(z == 1)))}
  })
  
  ctime <- apply(C, 1, function(x){
    if (sum(x) == 0){
      return(t0+1)
    }else {return(min(which(x == 1)))}
  })
  
  
  ftype <- cut(which_j, breaks=c(0, seq_len(J)/J), right = T, labels = FALSE)
  
  
  time <- pmin(ctime, ftime)
  ftype[ctime <= ftime] <- 0
  
  for (t_i in trttime) {
    missing_ind <- (time <= t_i)
    varlist[[paste0("t", t_i)]][missing_ind, -1] <- NA
    trtlist[[paste0("t", t_i)]][missing_ind, -1]  <- NA
  }
  
  
  return(list(adjustVars = varlist, trt = trtlist, ftime = time, ftype = ftype))
}



cf_data <- function(w1, w2, cf, ftime, ftype, t0 = 4, J =J){
  temp <- data.frame(w1, w2, cf, expand.grid(ftime = seq_len(t0), ftype = seq_len(J)), Y = 0)
  temp$Y[which(temp$ftype == ftype & temp$ftime >=  ftime)] <- 1
  return(temp)
}



true_hazard <- function(w1, w2, trt){
  plogis(-2 + 0.5* w1 + 0.6*w2 - trt)
}

getTrueCumInc <- function(w1, w2, trt, trt_t2, t0 = 4){
  haz1_base <- plogis(-2 + 0.5* w1 + 0.6*w2 - trt)
  if (t0 == 1){
    ci <- haz1_base
  }
  
  if (t0 == 2){
    ci <- haz1_base  + haz1_base * (1 - 3*haz1_base)
  }
  
  if (t0 == 3){
    w_t2 <- expand.grid(w1_t2 = c(0,1), w2_t2 = c(0,1))
    
    hazard_bar_3 <- sum(mapply(true_hazard, w1 = w_t2$w1_t2, w2 = w_t2$w2_t2, trt = trt_t2)* 
                          mapply(w1_t2_dens, w1 = w1, w1_t2 = w_t2$w1_t2)*mapply(w2_t2_dens, w2 = w2, w2_t2 = w_t2$w2_t2))
    ci <- haz1_base + haz1_base*(1 - 3*haz1_base) +  hazard_bar_3*(1 - 3*haz1_base)^2
  }
  
  
  if (t0 == 4){
    w_t2 <- expand.grid(w1_t2 = c(0,1), w2_t2 = c(0,1))
    hazard_bar_3 <- sum(mapply(true_hazard, w1 = w_t2$w1_t2, w2 = w_t2$w2_t2, trt = trt_t2)* 
                          mapply(w1_t2_dens, w1 = w1, w1_t2 = w_t2$w1_t2)*mapply(w2_t2_dens, w2 = w2, w2_t2 = w_t2$w2_t2))
    hazard_bar_3_4 <- sum(mapply(true_hazard, w1 = w_t2$w1_t2, w2 = w_t2$w2_t2, trt = trt_t2)^2* 
                            mapply(w1_t2_dens, w1 = w1, w1_t2 = w_t2$w1_t2)*mapply(w2_t2_dens, w2 = w2, w2_t2 = w_t2$w2_t2))
    ci <- haz1_base + haz1_base*(1 - 3*haz1_base) +  hazard_bar_3*(1-3*haz1_base)^2 + 
      (hazard_bar_3 - 3*hazard_bar_3_4)*(1-3*haz1_base)^2
    
  }
  
  return(ci)
  
}

