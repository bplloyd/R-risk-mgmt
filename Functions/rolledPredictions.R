rolledPredictions = function(rollfit, data, slices){
  preds = xts(rep(NA, length(rollfit)), order.by = index(data)[(nrow(data)-length(rollfit)+1):nrow(data)])
  for(i in 1:length(rollfit)){
    preds[i] = predict(rollfit[[i]], newdata = data[slices[[2]][[i]],])
  }
  names(preds)="prediction"
  return(preds)
}