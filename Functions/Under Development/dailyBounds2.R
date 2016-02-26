dailyBounds2 = function(accounts, date=NULL, p = 0.99, n = 126, FUN = "VaR",  method = "modified"){
  require(PerformanceAnalytics)
  if(FUN == "VaR")
  {
      if(method == "modified")
      {
          func = mVaRBounds
      }
      else
      {
          func = nVaRBounds
      }
  }
  if(FUN == "ES")
  {
       func = function(x, p, n){ return(riskBounds(x[(nrow(x)-(n-1)):nrow(x),], FUN = "ES", p=p, method = method)) }
  }
  if(is.null(date))
  {
    date = end(accounts[-nrow(accounts),])
  }
  bounds = data.frame(lbound = numeric(), ubound = numeric())
  
  for (i in 1:ncol(accounts))
  {
      bounds = rbind(bounds, data.frame(func(accounts[paste("/", date, sep = ""),i], p=p, n=n)))
  }
  row.names(bounds)=names(accounts)
  return(bounds)
}
