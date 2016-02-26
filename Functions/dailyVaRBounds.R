dailyVaRBounds = function(accounts, date=NULL, p = 0.99, n = 126,  method = "modified"){
  require(PerformanceAnalytics)
 
  if(method == "modified")
  {
    func = mVaRBounds
  }
  else
  {
    func = nVaRBounds
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
