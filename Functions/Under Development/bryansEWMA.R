bryansEWMAVolatilityContribution = function(R, weights, lambda = 0.93, cov.mat0 = NULL)
{
  rtn_full = R
  for(i in 1:ncol(rtn_full))
  {
    infRow = which(rtn_full[,i]==Inf)
    if(length(infRow)>0)
      rtn_full[infRow,i] = 0
    
  }
  
  rtn = na.fill(rtn_full, 0)
  
  if(is.null(cov.mat0))
  {
    Sigt = cov(scale(rtn_full, center = T, scale = F), use = "p")
  }
  else
  {
    Sigt = cov.mat0
  }
  
  
  if(is.matrix(weights))
  {
    if(nrow(rtn)>nrow(weights))
    {
      firstWeightRow = which(index(rtn)==start(weights))
    }
    
    if(nrow(rtn)<=nrow(weights))
    {
      firstWeightRow = 1
      w = as.vector(weights[1,])
      Sigma_ALL = w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w), ncol(Sigt))
    }
    
    
    for(t in 2:(nrow(rtn)+1))
    {
      xx = na.fill(as.numeric(rtn[t-1, ]), fill = 0)
      for(i in 1:ncol(rtn))
      {
        Sigt[i, ] = ((1 - lambda) * xx * xx[i]) + (lambda * Sigt[i,])
      }
      
      if(t >= firstWeightRow)
      {
        if(!exists("Sigma_ALL"))
        {
          w = as.vector(weights[1,])
          Sigma_ALL = w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w), ncol(Sigt))
        }
        else
        {
          w = as.vector(weights[t-firstWeightRow + 1, ])
          Sigma_ALL = rbind(Sigma_ALL, w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w),ncol(Sigt)))
        }
      }
    }
    Sigma_ALL = xts(Sigma_ALL, order.by = index(weights))
    names(Sigma_ALL) = colnames(Sigt)
  }
  if(!is.matrix(weights))
  {
    w = weights
    Sigma_ALL = w*(w %*% na.fill(Sigt, 0))/rep(sqrt(t(w) %*% na.fill(Sigt, 0) %*% w), ncol(Sigt))
    for(t in 2:(nrow(rtn)+1)) 
    {
      xx = na.fill(as.numeric(rtn[t - 1, ]), fill=0)
      for(i in 1:ncol(rtn))
      {
        Sigt[i, ] = h1 * xx * xx[i] + lambda * Sigt[i,]
      }
      Sigma_ALL = rbind(Sigma_ALL, w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w),ncol(Sigt)))
    }
    Sigma_ALL = xts(Sigma_ALL, order.by = index(R))
    names(Sigma_ALL) = colnames(Sigt)
    Sigma_ALL = Sigma_ALL[,-which(Sigma_ALL[nrow(Sigma_ALL),]==0)]
  }
  return(Sigt)
}