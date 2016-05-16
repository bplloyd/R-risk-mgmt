ewmaRiskContribution = function(riskFunc = "VaR", R, weights, p=0.95, lambda = 0.92)
{
  require(xts)
  require(PerformanceAnalytics)
  R = R[which(rowSums(is.na(R))<ncol(R)),]
  Mu = colMeans(R, na.rm = T)
  
  if (!is.matrix(R)) {
    rtn_full = as.matrix(R)
  }
  riskFunc = match.fun(riskFunc)
  rtn_full = R
  rtn = na.omit(R)
  nT = dim(rtn)[1]
  k = dim(rtn)[2]
  
  x_full =scale(rtn_full, center = TRUE, scale = FALSE)
  Sigt = cov(x_full, use = "p")
  
  par = lambda
  h1 = 1 - lambda
  x = na.omit(x_full)
  
  
  if(is.vector(weights))
      weights_t = weights
  if(is.matrix(weights))
      weights_t = weights[1,]
  
  V1 = riskFunc(weights = weights_t, p = p, method = "gaussian", portfolio_method = "component", sigma = Sigt, mu = Mu)$contribution
  
  for (t in 2:nT) {
    xx = as.numeric(x[t - 1, ])
    for (i in 1:k) {
      Sigt[i, ] = h1 * xx * xx[i] + lambda * Sigt[i,]
    }
    weights_t = weights[t,]
    V1 = rbind(V1,  riskFunc(weights = weights_t, p = p, method = "gaussian", portfolio_method = "component", sigma = Sigt, mu = Mu)$contribution)
  }
  colnames(V1) = names(R)
  return(xts(V1, order.by = index(rtn)))
}