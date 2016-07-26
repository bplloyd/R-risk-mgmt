marketRiskDecomposition_old = function(R.cov,  p = NULL, annualized = F)
{
  require(xts)
  if(!is.null(p))
      z = qnorm(p)
  else
      z = 1
  if(is.xts(R.cov))
  {
    R.vol = sqrt(R.cov[,1])
    R.beta = R.cov[,3]/R.cov[,4]
    R.vol_mkt = (R.beta^2)*R.cov[,4]/R.vol
    R.vol_spec = R.vol - R.vol_mkt
  }
  else
  {
    R.vol = sqrt(R.cov[1])
    R.beta = R.cov[3]/R.cov[4]
    R.vol_mkt = (R.beta^2)*R.cov[4]/R.vol
    R.vol_spec = R.vol - R.vol_mkt
  }
  
  result = c(TotalRisk = R.vol, MarketRisk = R.vol_mkt, SpecificRisk = R.vol_spec)*z
  if(annualized)
    result = sqrt(252)*result
  #row.names(result) = c("TotalRisk", "MarketRisk", "SpecificRisk")
  return(result)
}
