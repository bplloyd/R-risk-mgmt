#RETURNS ROLLED LIST OF CORRELATION MATRICES
rollingCorrelationMatrix = function(data, window=126)
{
  data = data[which(rowSums(!is.na(data))>1),]
  slices = createTimeSlices2(data = data, initialWindow = window, fixedWindow = T)
  return(lapply(slices, function(x)return(cor(data[x,], use='pairwise.complete.obs'))))
}