# RETURNS ROLLING CCTR IN AN XTS OBJECT

rollCCTR = function(data, weights = NULL, initialWindow = 126, fixedWindow = T, skip = 0)
{
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(weights))
    weights = rep(1/ncol(data), ncol(data))
  weights = weights/sum(weights)
  slices = createTimeSlices2(data, initialWindow = initialWindow, fixedWindow = fixedWindow, skip = skip)
  rolledList = lapply(slices, function(x)return(StdDev(data[x,], weights = weights, portfolio_method = "component")))
  return(extractFieldFromRoll(rolledList, "contribution"))
}

