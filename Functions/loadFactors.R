loadFactors = function()
{
  mlfi = loadMLFI()
  hyoas = mlfi[,"ML.HYOAS - VALUE"]
 
  
  ty = Quandl(code = "USTREASURY/YIELD", type = "xts")
  ty10 = ty[,"10 YR"]
  
  ty10_2 = ty[, "10 YR"] - ty[, "2 YR"]
  
  sp = Quandl(code = "YAHOO/SP500TR", type = "xts")
  sp = sp[,"Adjusted Close"]
  names(sp) = "SPTR"
  sp.r = na.omit(CalculateReturns(na.omit(sp)))
  sp.r = rbind(sp.r, sp2[which(index(sp2)>end(sp.r))])
  
  f3 = getFama3()
 
  hyoas = na.omit(diff(na.omit(hyoas)))
  ty10 = na.omit(diff(na.omit(ty10)))
  ty10_2 = na.omit(diff(na.omit(ty10_2)))
  
  facts = cbind(sp.r, f3[,2:3],hyoas, ty10, ty10_2)
  names(facts) = c("SPTR", "SMB", "VMG","HYOAS_DIFF", "TY10_DIFF", "TY10_2_DIFF")
  return(facts)
}