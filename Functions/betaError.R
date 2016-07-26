betaError = function(returns, pred)
{
  return(sqrt(mean((returns - pred)^2 , na.rm = T)))
}