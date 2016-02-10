dailyBounds2 = function(accounts, date=NULL, p = 0.99, n = 126){
  if(is.null(date))
    date = end(accounts[-nrow(accounts),])
  bounds = data.frame(lbound = numeric(), ubound = numeric())
  for (i in 1:ncol(accounts))
    bounds = rbind(bounds, data.frame(nVaRBounds(accounts[paste("/",date,sep = ""),i], p=p, n=n)))
  row.names(bounds)=names(accounts)
  return(bounds)
}
