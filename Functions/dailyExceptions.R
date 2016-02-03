dailyExceptions = function(accounts, p = 0.99, n = 126){
  exceptions = data.frame(Name = character(), Return = numeric(), LowBound = numeric(), UpBound = numeric())
  for (i in 1:ncol(accounts)){
    accountexception = nVaRExceptions(accounts[(nrow(accounts)-n):nrow(accounts),i], p = p, n = n)
    if (nrow(accountexception)!=0){
      exceptions = rbind(exceptions, cbind.data.frame(Name = names(accounts)[i], accountexception))
    }
  }
  return(exceptions)
}