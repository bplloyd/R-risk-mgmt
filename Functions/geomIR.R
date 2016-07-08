geomIR = function(Ra, Rb, beta=NULL)
{
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(beta))
      beta = CAPM.beta(Ra, Rb)
  alpha = Ra - beta*Rb
  res = cbind(((1+mu.geom(alpha))^252-1),
               (sqrt(252)*sd.geom(alpha)),
               ((1+mu.geom(alpha))^252-1)/(sqrt(252)*sd.geom(alpha)))
  colnames(res) = c("ActivePremium", "TrackingError", "InformationRatio")
  return(res)
}

