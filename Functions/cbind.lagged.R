cbind.lag = function(notlagged, lagged){
  require(xts)
  return(cbind(rbind(na.omit(notlagged), xts(matrix(data = NaN, nrow = 1, ncol = ncol(notlagged)), order.by = end(notlagged)+1)),
        rbind(lag(lagged,1), xts(tail(lagged,1), order.by = end(lagged)+1))))
}