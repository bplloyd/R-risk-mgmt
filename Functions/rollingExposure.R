getExposure = function(id)
{
  proc = "usp_Get_Exposure"
  param = paste("@id", id, sep = " = ")
  result = executeSP(proc, param, schema = "HAMF")
  #return(result)
  return(xts(result[,2:ncol(result)], order.by = as.Date.character(result[,1])))
}