currentLevels = function(accounts, FUN = "VaR", width = 126, p = 0.99, method = "modified"){
  require(xts)
  require(PerformanceAnalytics)
  accounts = accounts[,-which(is.nan(accounts[nrow(accounts),]))]
  allBounds = vector(mode = "list", length = ncol(accounts))
  for (i in 1:length(allBounds)){
    levels = getBounds(na.omit(accounts[,i]), FUN = FUN, width = width, p=p, method = method)
    allBounds[[i]] = cbind(rbind(na.omit(accounts[,i]), xts(matrix(data = NaN, nrow = 1, ncol = 1), order.by = Sys.Date())),
                           rbind(lag(levels,1), xts(tail(levels,1), order.by = Sys.Date())))
  }
  names(allBounds)= names(accounts)
  return(allBounds)
}