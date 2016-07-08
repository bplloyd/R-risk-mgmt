specVol = function(vol, beta, marketvol)
{
  return(sqrt(vol^2 - (beta*marketvol)^2))
}