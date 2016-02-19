nVaRBounds = function(x, p = 0.99, n = 126){
  require(xts)
  boundDate = end(x)
  upperBound = xts(nVaR(x, p=1-p, n=n), order.by = boundDate)
  lowerBound = xts(nVaR(x, p=p, n=n), order.by = boundDate)
  result = merge.xts(lowerBound, upperBound)
  names(result)[1:2] = c(paste("VaR", p,  sep = ""), paste("VaR", 1-p, sep = ""))
  return(result)
}