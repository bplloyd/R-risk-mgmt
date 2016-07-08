mu.geom = function(R, logR=F)
{
  R = na.omit(R)
  if(!logR)
    R = log(1+R)
  return(exp(mean(R))-1)
}