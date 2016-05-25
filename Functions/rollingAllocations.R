rollingAllocations = function(id, startDate, endDate)
{
  require(xts)
  proc = "usp_Allocations_Rolling"
#   id = 786
#   startDate = '1-1-2016'
#   endDate = '5-13-2016'
  
  
  startDate = paste0("'", startDate, "'")
  endDate = paste0("'", endDate, "'")
  
  param_start = paste("@startDate", startDate, sep = " = ")
  param_end = paste("@endDate", endDate, sep = " = ")
  param_id = paste("@fundId", id, sep = " = ")
  
  params = paste(param_id, param_start, param_end, sep = ", ")
  
  res = executeSP(proc, paramstring = params)
  return(xts(res[,2:ncol(res)], order.by = as.Date(res$DateReported)))
  
}