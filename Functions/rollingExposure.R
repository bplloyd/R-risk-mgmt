rollingExposure = function(sub)
{
  proc = "usp_Rolling_LSNetGross"
  param = paste("@sub", sub, sep = " = ")
  result = executeSP(proc, param)
  #return(result)
  return(xts(result[,2:5], order.by = as.Date.character(result[,1])))
}