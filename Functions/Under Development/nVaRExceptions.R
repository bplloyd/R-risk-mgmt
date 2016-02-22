nVaRExceptions = function(x, p=0.99, n=126){
  rollingVaR = nVaRBoundsRolling(x, p = p, n = n)
  comparison = merge.xts(x, lag(rollingVaR$VaR_0.99,1), lag(rollingVaR$VaR_0.01,1))
  names(comparison)[1] = 'DailyReturn'
  return(comparison[which(comparison$DailyReturn < comparison$VaR_0.99 | comparison$DailyReturn > comparison$VaR_0.01),])
}