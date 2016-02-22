getFama3 = function(inds = NULL){
  require(xts)
  require(PerformanceAnalytics)
  
  if(is.null(inds))
      inds = loadIndices()
  
  names = c("SPTR", "SPTRSVX", "SPTRSGX", "RTY")
  facts = inds[,names]
  
  allNA = which(apply(facts, 1, function(x)all(is.na(x))))
  if(length(allNA)!=0)
    facts = facts[-allNA,]
  
  allNaN = which(apply(facts, 1, function(x)all(is.nan(x))))
  if(length(allNaN)!=0)
    facts = facts[-allNaN,]
  
  facts = CalculateReturns(facts)
  facts = cbind(facts$SPTR, facts$RTY - facts$SPTR, facts$SPTRSVX - facts$SPTRSGX)
  names(facts)[2:3] = c("SMB", "VMG")
  return(facts)
}