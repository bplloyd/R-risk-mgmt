ewmaSigma = function(model, annualize = T)
{
  require(xts)
  result = xts(model@fit$sigma, order.by = model@model$modeldata$index)
  if(annualize)
      result = result*sqrt(Frequency(result))
  return(result)
}