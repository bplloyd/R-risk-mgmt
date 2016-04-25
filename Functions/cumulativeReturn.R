cumulativeReturn = function (R, wealth.index = FALSE, geometric = TRUE) 
{
  require(xts)
  if(geometric)
    return((cumprod(1+na.fill(R,0))-1))
  else
    return(cumsum(na.fill(R,0)))
}