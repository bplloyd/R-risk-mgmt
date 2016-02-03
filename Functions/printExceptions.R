printExceptions = function(accounts, p = 0.99, n = 126){
  print(paste("nVaR 99% Exceptions for ", end(accounts), sep=""))
  #print(paste("Name", "Date", "Return","LowBound","UpBound", sep = "  |  "))
  
  print(dailyExceptions(accounts = accounts, p=p, n=n))
}