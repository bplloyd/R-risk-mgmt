getFIReturns = function(inds,startDate = NULL, endDate = NULL){
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(startDate))
    startDate = start(inds)
  if(is.null(endDate))
    endDate = end(inds)
  h0 = inds[,grep("H0A", names(inds))]
  c0 = inds[,grep("C0A", names(inds))]
  bc = inds[,grep("LBUSTRUU", names(inds))]
  fi = cbind(h0, c0, bc)
  fi = fi[-which(apply(fi, 1, function(x)all(is.na(x)))),]
  fi=CalculateReturns(fi)
  return(fi[paste(startDate,endDate,sep = "/"),])
}
