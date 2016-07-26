ewmaVolatilityContribution2 = function(R, weights, lambda = 0.94, includeMisc = T, curWeightsOnly = T)
{
#  rm(V1)
#   R=subs[-nrow(subs), names(alpha.allocs.subs)]
#   weights = lag(alpha.allocs.subs)
  # lambda = 0.92
  miscColR = grep("Misc", names(R))
  miscColW = grep("Misc", names(weights))
  
  if(!curWeightsOnly)
  {
      maxDate = min(end(R), end(weights))
      R = R[paste0("/", maxDate),]
      weights = weights[paste0("/", maxDate),]
  }
  
  weights = na.fill(weights, 0)
  
  if(!includeMisc)
  {
    if(length(miscColR)>0)
    {
        R=R[,-miscColR]
    }
    if(length(miscColW)>0)
    {
        weights = weights[,-miscColW]
        weights = as.xts(t(apply(weights, 1, FUN = function(x)return(x/sum(x)))))
    }
  }
  index(R) = as.Date(index(R))
  index(weights) = as.Date(index(weights))
  
  R = R[, names(weights)]
  
  for(i in 1:ncol(R))
  {
    infRow = which(R[,i]==Inf)
    if(length(infRow)>0)
    {
      R[infRow,i] = 0
    }
    
  }
  
  Mu = colMeans(R, na.rm = T)
  
  
  if(length(miscColR)>0)
  {
    R = R[which(rowSums(is.na(R[,-miscColR]))<ncol(R[,-miscColR])),]
  }
  if(length(miscColR)==0){
    R = R[which(rowSums(is.na(R)) < ncol(R)),]
}
  
  rtn_full = R
  
  rtn = na.fill(R, 0)
  
  nT = dim(rtn)[1]
  k = dim(rtn)[2]
  
  x_full = scale(rtn_full, center = TRUE, scale = FALSE)
  Sigt = na.fill(cov(x_full, use = "p"), fill=0)
  par = lambda
  
  h1 = 1 - lambda
  
  if(!curWeightsOnly)
  {
    if(nrow(rtn)>nrow(weights)){
      firstWeightRow = which(index(rtn)==start(weights))
    }
    if(nrow(rtn)<=nrow(weights)){
      firstWeightRow = 1
      w = as.vector(weights[index(rtn[1,]),])
      V1 = w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w), ncol(Sigt))
    }
    
    for (t in 2:nT) {
      xx = na.fill(as.numeric(rtn[t - 1, ]), fill=0)
      for (i in 1:k) {
        Sigt[i, ] = h1 * xx * xx[i] + lambda * Sigt[i,]
      }
      if(t >= firstWeightRow)
      {
        w = as.vector(weights[index(rtn[t,]),])
        if(!exists("V1"))
        {
          V1 = w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w), ncol(Sigt))
        }
        else
        {
          V1 = rbind(V1, w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w),ncol(Sigt)))
        }
      }
    }
    if(nrow(rtn)>nrow(weights)){
      sigma.t = xts(V1, order.by = index(weights))
    }
    else{
      sigma.t = xts(V1, order.by = index(rtn))
    }
    names(sigma.t) = colnames(Sigt)
  }
  if(curWeightsOnly)
  {
    w = as.vector(weights[nrow(weights),])
    V1 = w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w), ncol(Sigt))
    for (t in 2:nT) 
    {
      xx = na.fill(as.numeric(rtn[t - 1, ]), fill=0)
      for (i in 1:k) 
      {
        Sigt[i, ] = h1 * xx * xx[i] + lambda * Sigt[i,]
      }
      V1 = rbind(V1, w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w),ncol(Sigt)))
    }
    sigma.t = xts(V1, order.by = index(R))
    names(sigma.t) = colnames(Sigt)
    #sigma.t = sigma.t[,-which(sigma.t[nrow(sigma.t),]==0)]
  }
  return(sigma.t*sqrt(252))
}


