rollingExposure = function(id)
{
  proc = "usp_Get_Exposure"
  param = paste("@id", id, sep = " = ")
  result = executeSP(proc, param, schema = "HAMF")
  #return(result)
  result = xts(result[,2:ncol(result)], order.by = as.Date.character(result[,1]))['20140623/', c("L", "S")]
  result$Net = result$L + result$S
  result$Gross = abs(result$L) + abs(result$S)
  return(result)
}