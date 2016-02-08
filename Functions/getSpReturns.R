getSpReturns = function(inds, startDate = NULL, endDate = NULL){
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(startDate))
      startDate = start(inds)
  if(is.null(endDate))
      endDate = end(inds)
  sp = inds[,grep("SPTR", names(inds))]
  sp = sp[-which(apply(sp, 1, function(x)all(is.na(x)))),]
  sp=CalculateReturns(sp)
  return(sp[paste(startDate,endDate,sep = "/"),])
}