appendPredictions = function(actual, predictions)
{
    for(i in 1:ncol(predictions))
    {
      nm = names(predictions)[i]
      actStart = start(na.omit(actual[,nm]))
      actEnd = end(na.omit(actual[,nm]))
      predictions[paste(actStart, actEnd, sep = "/"),i] = na.omit(actual[,i])
    }
  return(predictions)
}