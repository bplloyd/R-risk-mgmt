getHYReturns = function(inds,startDate = NULL, endDate = NULL){
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(startDate))
    startDate = start(inds)
  if(is.null(endDate))
    endDate = end(inds)
  h0 = inds[,grep("H0A", names(inds))]
  c0 = inds[,grep("C0A", names(inds))]
  hy = cbind(h0, c0)
  hy = hy[-which(apply(hy, 1, function(x)all(is.na(x)))),]
  hy=CalculateReturns(hy)
  return(hy[paste(startDate,endDate,sep = "/"),])
}
