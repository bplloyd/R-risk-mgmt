runVaR = function(){
  subs = loadSubAdvisors()
  ufts = loadUFTs()
  pofs = loadPOFs()
  printExceptions = function(accounts){
    print(paste("Name", "Date", "Return","LowBound","UpBound", sep = "  |  "))
    for (i in 1:ncol(accounts)){
      exceptions = nVaRExceptions(accounts[,i])
      print(paste(names(accounts)[i], 
                  end(exceptions), 
                  exceptions[end(exceptions),1],
                  exceptions[end(exceptions),2],
                  exceptions[end(exceptions),3], 
                  sep = "  |  "))
    }
  }
  printExceptions(subs)
  printExceptions(ufts)
  printExceptions(pofs)
}

