currentVaRLevels = function(accounts, n = 126, p = 0.99){
  require(xts)
  accounts = accounts[,-which(is.nan(accounts[nrow(accounts),]))]
  allBounds = vector(mode = "list", length = ncol(accounts))
  for (i in 1:length(allBounds)){
      var = nVaRBounds(na.omit(accounts[,i]), n=n, p=p)
      allBounds[[i]] = cbind(rbind(na.omit(accounts[,i]), xts(matrix(data = NaN, nrow = 1, ncol = 1), order.by = Sys.Date())),
                             rbind(lag(var,1), xts(tail(var,1), order.by = Sys.Date())))
  }
  names(allBounds)= names(accounts)
  return(allBounds)
}