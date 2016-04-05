organizeLevels = function(pof.levels, uft.levels, sub.levels, mkt.levels)
{
#   pof.levels = pofs.levels
#   uft.levels = ufts.levels
#   sub.levels = subs.levels
  
  mstrat = pof.levels[which(row.names(pof.levels) %in% c("ALPHX", "ALPIX", "HHSIX")),]
  lse = rbind(pof.levels[which(row.names(pof.levels)=="HLSIX"),], uft.levels[which(row.names(uft.levels)=="LSE"),], sub.levels$LSE[which(row.names(sub.levels$LSE)!="MiscLSE"),])
  lsd = rbind(pof.levels[which(row.names(pof.levels)=="HFINX"),], uft.levels[which(row.names(uft.levels)=="LSD"),], sub.levels$LSD[which(row.names(sub.levels$LSD)!="MiscLSD"),])
  ed = rbind(uft.levels[which(row.names(uft.levels)=="ED"),], sub.levels$ED[which(row.names(sub.levels$ED)!="MiscED"),])
  mn = rbind(uft.levels[which(row.names(uft.levels)=="MN"),],sub.levels$MN[which(row.names(sub.levels$MN)!="MiscMN"),])
  mf = rbind(pof.levels[which(row.names(pof.levels)=="HMFIX"),], uft.levels[which(row.names(uft.levels)=="MF"),], sub.levels$MF[which(row.names(sub.levels$MF)!="MiscMF"),])
  
  levels = list(MULTISTRAT = mstrat, LSE= lse, LSD = lsd, ED = ed, MN = mn, MF = mf, MARKET = mkt.levels)
  names(levels) = c("MultiStrat", "Long/Short Equity", "Long/Short Debt", "Event Driven", "Market Neutral", "Managed Futures", "Market Indices")
  #levels = levels[lapply(levels, function(x)return(nrow(x)))>0]
  levelType = trimws(substr(names(pof.levels)[1], start = 1, stop = 3))
  attr(levels, "subheadings") = paste(names(levels), levelType, "levels", sep = " ")
  return(levels)
}



