loadMLReturns = function(inds,startDate = NULL, endDate = NULL){
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(startDate))
    startDate = start(inds)
  if(is.null(endDate))
    endDate = end(inds)
  h0 = names(inds[,grep("H0A", names(inds))])
  c0 = names(inds[,grep("C0A", names(inds))])
  b0 = names(inds[,grep("B0A", names(inds))])
  ml.names = c(h0, c0, b0)
  
  fi = na.omit(CalculateReturns(na.omit(inds[,ml.names[1]])))
  for(i in 2:length(ml.names))
    fi = cbind(fi, na.omit(CalculateReturns(na.omit(inds[,ml.names[i]]))))
  
  return(fi[paste(startDate,endDate,sep = "/"),])
}
