estimateVol = function(rets)
{
  require(xts)
  if(nrow(rets)>126)
  {
    return(sqrt(252)*sd(rets[(nrow(rets)-125):nrow(rets)], na.rm = T))
  }
  else
  {
    return(sqrt(252)*sd(rets, na.rm = T))
  }
}