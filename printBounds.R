printBounds = function(accounts, p = 0.99, n = 126){
  print(paste("Name", "Date", "LowBound","UpBound", sep = "  |  "))
  for (i in 1:ncol(accounts)){
    bounds = nVaRBounds(accounts[,i], p = p, n=n)
    print(paste(names(accounts)[i], 
                end(bounds), 
                bounds[end(bounds),1],
                bounds[end(bounds),2],
                sep = "  |  "))
  }
}