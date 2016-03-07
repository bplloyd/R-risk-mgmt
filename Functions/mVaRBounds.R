mVaRBounds = function(x, p = 0.98, n = 126){
  require(xts)
  boundDate = end(x)
  upperBound = xts(mVaR(x, p=1-p, n=n), order.by = boundDate)
  lowerBound = xts(mVaR(x, p=p, n=n), order.by = boundDate)
  result = merge.xts(lowerBound, upperBound)
  names(result)[1:2] = c(paste("mVaR", p,  sep = ""), paste("mVaR", 1-p, sep = ""))
  return(result)
}