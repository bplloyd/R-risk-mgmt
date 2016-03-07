#RETURNS DAILY VAR EXCEPTIONS

dailyExceptions = function(accounts, date = NULL, p = 0.98, n = 126, method = "modified"){
  require(xts)
  if(is.null(date))
      date = end(accounts)
  exceptionDate = index(accounts)[which(index(accounts)==date)-1]
  exceptions = cbind(t(data.frame(accounts[date,])), data.frame(dailyVaRBounds(accounts, date = exceptionDate, p=p, n=n, method = method)))
  #names(exceptions)[2:3] = paste(names(exceptions)[2:3], exceptionDate, sep = "_")
  names(exceptions)[1] = c(paste("Return", date, sep = " "))
  return(exceptions[which((exceptions[,1] < exceptions[,2]) | (exceptions[,1] > exceptions[,3])),])
}