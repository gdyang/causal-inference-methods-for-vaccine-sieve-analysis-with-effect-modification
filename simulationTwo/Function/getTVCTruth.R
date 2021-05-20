
truth_MSM <- function(n, trttime = c(0,2), t0 = 4, J = 3, 
                      msm.formula = ".",  trtOfInterest){
  varlist <- vector("list", length =  length(trttime))
  names(varlist) <- paste0("t", trttime)
  trtlist <- vector("list", length =  length(trttime))
  names(trtlist) <- paste0("t", trttime)
  
  u_N_j <- data.frame(matrix(runif(n*t0, 0 , 1), nrow = n))
  which_j <- runif(n, 0, 1)
  
  
  cf_list <- vector("list", length = (ncol(trtOfInterest) - 1))
  
  
  varlist$t0 <- data.frame(id = seq_len(n), w1 =  rbinom(n, 1, 0.3), w2 =  rbinom(n, 1, 0.7))
  
  w1_new <- rbinom(n, 1, ifelse(varlist$t0$w1 ==1, 0.6, 0.4))
  w2_new <- rbinom(n, 1, ifelse(varlist$t0$w2 ==1, 0.7, 0.3))
  varlist[["t2"]] <- data.frame(id = seq_len(n), w1 = w1_new, w2 = w2_new)
  
  for (cf_i in 1:length(cf_list)){
    #trttime_no_base <- trttime[which(trttime !=0)]
    trtlist$t0 <-  data.frame(id = seq_len(n), trt = trtOfInterest[1, cf_i+1])
    trtlist$t2 <-  data.frame(id = seq_len(n), trt = trtOfInterest[2, cf_i+1])
    N_j <- data.frame(matrix(rep(0, n*t0), nrow = n))
    for (t_i in seq_len(t0)) {
      w1_t  <- varlist[[paste0("t", max(trttime[trttime < t_i]))]]$w1
      w2_t  <-  varlist[[paste0("t", max(trttime[trttime < t_i]))]]$w2
      trt_t <- trtlist[[paste0("t", max(trttime[trttime < t_i]))]]$trt
      
      
      N_j[,t_i] <- as.numeric(3*plogis(-2 + 0.5* w1_t + 0.6*w2_t - trt_t) >  u_N_j[, t_i])
      
    }
    
    N_j <- t(apply(N_j, 1, cummax))
    
    
    ftime <- apply( N_j, 1, function(z) {
      if (sum(z) == 0){
        return(t0+1)
      }else {return(min(which(z == 1)))}
    })
    
    
    ftype <- cut(which_j, breaks=c(0, seq_len(J)/J), right = T, labels = FALSE)
    
    cf_list[[cf_i]] <- do.call(rbind, mapply(cf_data, w1 = varlist$t0$w1, w2 =  varlist$t0$w2, 
                                             cf = cf_i, ftype = ftype , ftime = ftime, J =J, SIMPLIFY = F))
    
  }
  
  cf_data <- do.call(rbind,  cf_list)
  cf_data$regimen1 = as.numeric(cf_data$cf == 1)
  cf_data$regimen2 = as.numeric(cf_data$cf == 2)
  
  return(cf_data)
}



truth_MSM_ci <- function(n, trttime = c(0,2), t0 = 4, J = 3, 
                         msm.formula = ".",  trtOfInterest){
  varlist <- vector("list", length =  length(trttime))
  names(varlist) <- paste0("t", trttime)
  trtlist <- vector("list", length =  length(trttime))
  names(trtlist) <- paste0("t", trttime)
  
  
  
  cf_list <- vector("list", length = (ncol(trtOfInterest) - 1))
  
  
  varlist$t0 <- data.frame(id = seq_len(n), w1 =  rbinom(n, 1, 0.3), w2 =  rbinom(n, 1, 0.7))
  
  w1_new <- rbinom(n, 1, ifelse(varlist$t0$w1 ==1, 0.6, 0.4))
  w2_new <- rbinom(n, 1, ifelse(varlist$t0$w2 ==1, 0.7, 0.3))
  varlist[["t2"]] <- data.frame(id = seq_len(n), w1 = w1_new, w2 = w2_new)
  
  for (cf_i in 1:length(cf_list)){
    temp.list <- vector("list", length = t0)
    
    for ( t_i  in 1: t0){
      temp  <- do.call(rbind, mapply(getTrueCumInc, w1 = varlist$t0$w1, 
                                     w2 = varlist$t0$w2, trt = trtOfInterest[1, cf_i+1], 
                                     trt_t2 =trtOfInterest[2, cf_i+1], t0 = t_i, SIMPLIFY = F))
      temp2 <- data.frame(ci = temp, ftime = t_i, w1 = varlist$t0$w1, w2 =   varlist$t0$w2, cf = cf_i)
      temp.list[[t_i]] <- data.frame(temp2[rep(seq(n), J),], ftype = rep(c(seq_len(J)), each = n))
    }
    cf_list[[cf_i]]  <- do.call(rbind, temp.list)
    
  }
  
  cf_data <- do.call(rbind,  cf_list)
  cf_data$regimen1 = as.numeric(cf_data$cf == 1)
  cf_data$regimen2 = as.numeric(cf_data$cf == 2)
  
  return(cf_data)
}

