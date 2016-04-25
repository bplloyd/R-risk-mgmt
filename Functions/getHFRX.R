getHFRX = function(inds)
{
  names = c("HFRXGL","HFRXEH","HFRXAR","HFRXFIC","HFRXED","HFRXSDV")
  hfrx = na.omit(CalculateReturns(na.omit(inds[,names[1]])))
  for(i in 2:length(names))
    hfrx = cbind(hfrx, na.omit(CalculateReturns(na.omit(inds[, names[i]]))))
  return(hfrx)
}