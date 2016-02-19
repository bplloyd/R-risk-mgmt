dailyBounds = function(accounts, date=NULL, p = 0.99, n = 126){
  if(is.null(date))
      date = end(accounts[-nrow(accounts),])
  bounds = data.frame(name = character(), lbound = numeric(), ubound = numeric())
  for (i in 1:ncol(accounts))
      bounds = rbind(bounds, data.frame(name = names(accounts)[i], nVaRBounds(accounts[paste("/",date,sep = ""),i], p=p, n=n)))
  row.names(bounds)=rep(date, ncol(accounts))
  return(bounds)
}

