bryansEWMACov = function(R, lambda = 0.93, cov.mat = NULL)
{
  rtn_full = R
  for(i in 1:ncol(rtn_full))
  {
    infRow = which(rtn_full[,i]==Inf)
    if(length(infRow)>0)
      rtn_full[infRow,i] = 0
    
  }
  
  rtn = na.fill(rtn_full, 0)
  
  if(is.null(cov.mat))
  {
    Sigt = cov(scale(rtn_full, center = T, scale = F), use = "p")
  }
  else
  {
    Sigt = cov.mat
  }
  
  for(t in 2:(nrow(rtn)+1))
  {
    xx = na.fill(as.numeric(rtn[t-1, ]), fill = 0)
    for(i in 1:ncol(rtn))
    {
      Sigt[i, ] = ((1 - lambda) * xx * xx[i]) + (lambda * Sigt[i,])
    }
  }
  return(Sigt)
}