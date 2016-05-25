rollingRiskContribution = function(riskFunc = "VaR", R, weights, p=0.95, width = 63, method = "gaussian")
{
  require(xts)
  require(PerformanceAnalytics)
  riskFunc = match.fun(riskFunc)
  ts = createTimeSlices2(data = R, initialWindow = width, fixedWindow = T)
  return(xts(t(sapply(ts, FUN = function(t)return(riskFunc(R[t,], weights = weights, p = p, portfolio_method = "component", method = method)$contribution))), order.by = as.Date(names(ts))))
}