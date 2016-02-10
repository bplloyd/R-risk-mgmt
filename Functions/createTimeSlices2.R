createTimeSlices2 = function(data, initialWindow, fixedWindow = TRUE, skip = 0){
  require(xts)
  y = 1:nrow(data)
  stops <- (seq(along = y))[initialWindow:length(y)]
  if (fixedWindow){
    starts <- stops - initialWindow + 1
  }
  else{
    starts <- rep(1, length(stops))
  }
  slices = lapply(mapply(seq, starts, stops, SIMPLIFY = FALSE), function(x)return(index(data)[x]))
  names(slices) = index(data[initialWindow:nrow(data),])
  
  thin <- function(x, skip = 2){
    n <- length(x)
    x[seq(1, n, by = skip)]
  }
  if (skip > 0){
    slices <- thin(slices, skip = skip + 1)
  }
  
  return(slices)
}