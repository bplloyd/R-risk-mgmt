getR2KSectors = function(inds=NULL){
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(inds))
    inds = loadIndices()
  
  r2k = inds[,grep("RGUS", names(inds))]
  r2k = r2k[,which(substr(names(r2k), start = 6, stop = 6)=="S")]
  
  allNA = which(apply(r2k, 1, function(x)all(is.na(x))))
  if(length(allNA)!=0)
    r2k = r2k[-allNA,]
  
  allNaN = which(apply(r2k, 1, function(x)all(is.nan(x))))
  if(length(allNaN)!=0)
    r2k = r2k[-allNaN,]
  return(CalculateReturns(r2k))
}