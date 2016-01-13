runVaR = function(){
  subs = loadSubAdvisors()
  ufts = loadUFTs()
  pofs = loadPOFs()
  printExceptions = function(accounts){
    for (i in 1:ncol(accounts)){print(paste(names(accounts)[i], ": ", end(nVaRExceptions(accounts[,i])), sep = " "))}
  }
  printExceptions(subs)
  printExceptions(ufts)
  printExceptions(pofs)
}

