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
    sixty40 = 0.6*sp2 + 0.4*bagg
    bms = cbind(bagg, hfrx$HFRXGL)
    names(bms)[1] = "SixtyForty"
  }
  return(bms)
}