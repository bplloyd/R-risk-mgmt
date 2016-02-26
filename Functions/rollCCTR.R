# RETURNS ROLLING CCTR IN AN XTS OBJECT

rollCCTR = function(data, weights = NULL, initialWindow = 126, fixedWindow = T, skip = 0, annualize = T)
{
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(weights))
    weights = rep(1/ncol(data), ncol(data))
  weights = weights/sum(weights)
  if(!is.null(names(weights)))
    data = data[, names(weights)]
  
  data = na.omit(data)
  slices = createTimeSlices2(data, initialWindow = initialWindow, fixedWindow = fixedWindow, skip = skip)
  rolledList = lapply(slices, function(x)return(StdDev(data[x,], weights = weights, portfolio_method = "component")))
  result = extractFieldFromRoll(rolledList, "contribution")
  if(annualize)
      result = result*sqrt(Frequency(data[,1]))
  return(result)
}

