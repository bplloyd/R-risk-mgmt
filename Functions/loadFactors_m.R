loadFactors_m = function(on = "months")
{
  mlfi = loadMLFI()
  hyoas = mlfi[,"ML.HYOAS - VALUE"]
  hyoas = hyoas[endpoints(hyoas, on = on),]
  
  ty = Quandl(code = "USTREASURY/YIELD", type = "xts")
  ty10 = ty[,"10 YR"]
  ty10 = ty10[endpoints(ty10, on = on),]
  
  sp = Quandl(code = "YAHOO/SP500TR", type = "xts")
  sp = sp[,"Adjusted Close"]
  names(sp) = "SPTR"
  sp.r = na.omit(CalculateReturns(na.omit(sp)))
  
  sp.r = rbind(sp.r, sp2[which(index(sp2)>end(sp.r))])
  
  sp.r = switch(on,
              days = sp.r,
              weeks = na.omit(apply.weekly(na.omit(sp.r), Return.cumulative)),
              months =  na.omit(apply.monthly(na.omit(sp.r), Return.cumulative)),
              quarters =  na.omit(apply.quarterly(na.omit(sp.r), Return.cumulative)),
              years =  na.omit(apply.yearly(na.omit(sp.r), Return.cumulative)))
  
  
  f3 = getFama3()
  f3 = switch(on,
              days = f3,
              weeks = na.omit(apply.weekly(na.omit(f3), Return.cumulative)),
              months =  na.omit(apply.monthly(na.omit(f3), Return.cumulative)),
              quarters =  na.omit(apply.quarterly(na.omit(f3), Return.cumulative)),
              years =  na.omit(apply.yearly(na.omit(f3), Return.cumulative)))
  
  
  hyoas = na.omit(diff(na.omit(hyoas)))
  ty10 = na.omit(diff(na.omit(ty10)))
  
  ty10 = ty10[which(index(ty10)>=start(hyoas))]
  index(ty10) = index(hyoas)
  
  sp.r = sp.r[which(index(sp.r)>=start(hyoas))]
  index(sp.r) = index(hyoas)
  
  f3 = f3[which(index(f3)>=start(hyoas)),]
  index(f3) = index(hyoas[which(index(hyoas)>=start(f3))])
  
  
  fi.facts = cbind(hyoas, ty10)
  
  
  facts = na.omit(cbind(sp.r, f3[,2:3], fi.facts))
  names(facts) = c("SPTR", "SMB", "VMG","HYOAS_DIFF", "TY10_DIFF")
  return(facts)
}