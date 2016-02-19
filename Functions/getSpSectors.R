getSpSectors = function(inds=NULL){
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(inds))
      inds = loadIndices()
  
  sp = inds[,grep("SPTR", names(inds))]
  sp = sp[,-which(names(sp) %in% c("SPTR","SPTRSGX", "SPTRSVX", "SPTRTRNS"))]
  
  allNA = which(apply(sp, 1, function(x)all(is.na(x))))
  if(length(allNA)!=0)
      sp = sp[-allNA,]
  
  allNaN = which(apply(sp, 1, function(x)all(is.nan(x))))
  if(length(allNaN)!=0)
      sp = sp[-allNaN,]
  return(CalculateReturns(sp))
}