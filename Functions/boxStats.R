boxStats = function(R)
{
  result = apply(R, MARGIN = 2, FUN = function(x){x = na.omit(x);  return(t(data.frame(min = min(x), q1 = quantile(x, 0.25), q2 = quantile(x, 0.5), q3 = quantile(x, 0.75), max= max(x))))})
  row.names(result) = c("Min", "Q1", "Q2", "Q3", "Max")
  return(result)
}

R = cbind(ufts$LSE['200907/'], sp2['200907/'], na.omit(CalculateReturns(inds$HFRXEH))['200907/'])