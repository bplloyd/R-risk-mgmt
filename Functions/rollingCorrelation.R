rollingCorrelation = function(data, window)
{
  
  data = data[which(rowSums(!is.na(data))>1),]
  slices = createTimeSlices2(data = data, initialWindow = window, fixedWindow = T)
  return(lapply(slices, function(x)return(cor(data[x,], use='pairwise.complete.obs'))))
}