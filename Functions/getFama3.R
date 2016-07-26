getFama3 = function(inds = NULL){
  require(xts)
  require(PerformanceAnalytics)
  
  if(is.null(inds))
      inds = loadIndices()
  
  nms = c("SPTR","RU10INTR", "SPTRSVX", "SPTRSGX", "RTY")
  facts = na.omit(CalculateReturns(na.omit(inds[,nms[1]])))
  for(i in 2:length(nms))
  {
    facts = cbind(facts,  na.omit(CalculateReturns(na.omit(inds[,nms[i]]))) )
  }
  
  facts = facts[rowSums(is.na(facts))<ncol(facts),]
  
  facts = cbind(facts$SPTR, facts$RTY - facts$RU10INTR, facts$SPTRSVX - facts$SPTRSGX)
  names(facts)[2:3] = c("SMB", "VMG")
  return(facts)
}