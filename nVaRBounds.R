nVaRBounds = function(x, p = 0.99, n = 126){
  result = merge.xts(x, apply.rolling2(x, n, FUN = "nVaR", p = p), apply.rolling2(x, n, FUN = "nVaR", p = 1-p))
  names(result)[1:3] = c("DailyReturn",paste("VaR", p, sep = "_"), paste("VaR", 1-p, sep = "_"))
  return(result)
}