riskBounds = function(x, FUN = "VaR", method = "modified", p = 0.99, n = 126){
  require(xts)
  require(PerformanceAnalytics)
  boundDate = end(x)
  lowerBound = xts(do.call(match.fun(FUN), args = list(R=x, p=p, width = n, method = method, portfolio_method = "single")), order.by = boundDate)
  
  if(FUN == "VaR"){
      upperBound = xts(do.call(match.fun(FUN), args = list(R=x, p=(1-p), width = n, method = method, portfolio_method = "single")), order.by = boundDate)
      result = merge.xts(lowerBound, upperBound)
      names(result)[1:2] = c(paste(FUN, p,  sep = ""), paste(FUN, 1-p, sep = ""))
  }
  else{
      result = lowerBound
      names(result)[1] = c(paste(FUN, p,  sep = ""))
  }
  return(result)
}