getMarketReturns = function(inds,startDate = NULL, endDate = NULL){
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(startDate))
    startDate = start(inds)
  if(is.null(endDate))
    endDate = end(inds)
  mkt = inds[,c("SPTR", "RTY", "H0A0")]
  mkt = mkt[-which(apply(mkt, 1, function(x)all(is.na(x)))),]
  mkt=CalculateReturns(mkt)
  
#   if(endDate != end(mkt))
#   {
#       require(Quandl)
#       qkey = getQuandlKey()
#       Quandl.api_key(qkey)
#       sp = Quandl("SPDJ/SPX", type = "xts")[,1]
#       rt = Quandl("YAHOO/INDEX_RUT", type = "xts")[,6]
#       hy = Quandl("WSJ/MLCHY",type = "xts")[,1]
#       
#       mkt2 = merge.xts(sp, rt, hy)
#       mkt2 = CalculateReturns(mkt2)[-1,]
#       
#       if(endDate %in% index(mkt2))
#       {
#           mkt = rbind(mkt, mkt2[endDate,])
#       }
#   }
  
  return(mkt[paste(startDate,endDate,sep = "/"),])
}