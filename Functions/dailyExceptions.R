dailyExceptions = function(accounts, date = NULL, p = 0.99, n = 126){
  if(is.null(date))
      date = end(accounts)
  exceptionDate = index(accounts)[which(index(accounts)==date)-1]
  exceptions = cbind(t(data.frame(accounts[date,])), data.frame(dailyBounds2(accounts, date = exceptionDate, p=p, n=n)))
  #names(exceptions)[2:3] = paste(names(exceptions)[2:3], exceptionDate, sep = "_")
  names(exceptions)[1] = paste('Return', date, sep = "_")
  return(exceptions[which((exceptions[,1] < exceptions[,2]) | (exceptions[,1] > exceptions[,3])),])
}