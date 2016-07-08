estimateSpBeta = function(rets, sp5)
{
  require(xts)
  require(PerformanceAnalytics)
  return(CAPM.beta(Ra = rets, Rb = sp5))
}