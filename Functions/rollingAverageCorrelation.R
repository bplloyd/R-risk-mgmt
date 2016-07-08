rollingAverageCorrelation = function(R, width = 63, mode = "window", lamda = 0.94)
{
  R = na.omit(R)
  
  if(toupper(mode)=="WINDOW")
  {

    tslices = createTimeSlices2(R, initialWindow = width, fixedWindow = T)
    res = xts(sapply(X = tslices, 
                     FUN = function(t)
                       {c = cor(R[t, ], use = "p"); return(mean(c[lower.tri(c)]))}
                     ), 
              order.by = index(R[width:nrow(R),])
              )
  }
  if(toupper(mode)=="EWMA")
  {
    ewmaCov = ewmaCovariance(R, lambda = lamda)
    ewmaCor = ewmaCorrelation(ewmaCov)
    res = xts(rowMeans(ewmaCor), order.by = index(ewmaCor))
  }
  names(res) = "AverageCorrelation"
  return(res)
}


# rollingAverageCorrelation2= function(R, width = 63)
# {
#   R = na.omit(R)
#   cor = rollapply(R, 
#             width = width, 
#             FUN = function(x) 
#               cor(x, use = "p"))
# }