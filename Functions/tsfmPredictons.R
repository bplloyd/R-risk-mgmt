tsFMPredictions = function(model, newdata)
{
  pred = predict(model, newdata)
  pred.xts = as.xts(pred[[1]])
  for(i in 2:length(pred))
    pred.xts = cbind(pred.xts, as.xts(pred[[i]]))
  
  names(pred.xts) = names(pred)
  return(pred.xts)
}