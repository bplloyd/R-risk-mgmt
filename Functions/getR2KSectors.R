getRussell = function(inds=NULL){
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(inds))
    inds = loadIndices()
  
  r1k = inds[,"RU10INTR"]
  r2k = inds[,"RTY"]
  r3k = inds[,"RU10INTR"]
  
  allNA = which(apply(r2k, 1, function(x)all(is.na(x))))
  if(length(allNA)!=0)
    r2k = r2k[-allNA,]
  
  allNaN = which(apply(r2k, 1, function(x)all(is.nan(x))))
  if(length(allNaN)!=0)
    r2k = r2k[-allNaN,]
  return(CalculateReturns(r2k))
}