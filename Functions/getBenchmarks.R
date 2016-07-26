getBenchmarks = function(id)
{
  if(id == 785)
    bms = cbind(sp2, hfrx$HFRXEH)
  else if(id == 784)
    bms = cbind(hy$H0A0, hfrx$HFRXFIC)
  else if(id == 783)
    bms = cbind(sp2, hfrx$HFRXED)
  else if(id == 782)
    bms = cbind(sp2, hfrx$HFRXAR)
  else if((id == 786) | (id == 774))
  {
    bagg = na.omit(CalculateReturns(na.omit(inds$LBUSTRUU)))
    sp.bc = cbind(sp2, bagg)
    sixty40 = 0.6*sp.bc[,1] + 0.4*sp.bc[,2]
    bms = cbind(sixty40, hfrx$HFRXGL)
    names(bms)[1] = "SixtyForty"
  }
  return(bms)
}