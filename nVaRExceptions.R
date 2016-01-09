nVaRExceptions = function(x, p=0.99, n=126){
  rollingVaR = nVaRBounds(x, p = p, n = n)
  return(rollingVaR[which(rollingVaR$DailyReturn < rollingVaR$VaR_0.99 | rollingVaR$DailyReturn > rollingVaR$VaR_0.01),])
}