sd.geom = function(R, logR = F)
{
  if(!logR)
    R = log(1+R)
  
  lmu.g = mean(R)
  return(exp(sqrt(mean((R-lmu.g)^2)))-1)
}

