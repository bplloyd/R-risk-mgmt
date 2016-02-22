organizeExceptions = function(pof.exceptions, uft.exceptions, sub.exceptions)
{
#   pof.exceptions = pofs.exceptions
#   uft.exceptions = ufts.exceptions
#   sub.exceptions = subs.exceptions
  
  mstrat = pof.exceptions[which(row.names(pof.exceptions) %in% c("ALPHX", "ALPIX", "HHSIX")),]
  lse = rbind(pof.exceptions[which(row.names(pof.exceptions)=="HLSIX"),], uft.exceptions[which(row.names(uft.exceptions)=="LSE"),], sub.exceptions$LSE[which(row.names(sub.exceptions$LSE)!="MiscLSE"),])
  lsd = rbind(pof.exceptions[which(row.names(pof.exceptions)=="HFINX"),], uft.exceptions[which(row.names(uft.exceptions)=="LSD"),], sub.exceptions$LSD[which(row.names(sub.exceptions$LSD)!="MiscLSD"),])
  ed = rbind(uft.exceptions[which(row.names(uft.exceptions)=="ED"),], sub.exceptions$ED[which(row.names(sub.exceptions$ED)!="MiscED"),])
  mn = rbind(uft.exceptions[which(row.names(uft.exceptions)=="MN"),],sub.exceptions$MN[which(row.names(sub.exceptions$MN)!="MiscMN"),])
  mf = rbind(pof.exceptions[which(row.names(pof.exceptions)=="HMFIX"),], uft.exceptions[which(row.names(uft.exceptions)=="MF"),], sub.exceptions$MF[which(row.names(sub.exceptions$MF)!="MiscMF"),])
  
  exceptions = list(MULTISTRAT = mstrat, LSE=lse, LSD = lsd, ED = ed, MN = mn, MF = mf)
  names(exceptions) = c("MultiStrat", "Long/Short Equity", "Long/Short Debt", "Event Driven", "Market Neutral", "Managed Futures")
  #exceptions = exceptions[lapply(exceptions, function(x)return(nrow(x)))>0]
  attr(exceptions, "subheadings") = paste(names(exceptions), "VaR Exceptions", sep = " ")
  return(exceptions)
}



