getAllocations_Rolling = function(id, start, end){
  proc = "usp_Allocations_Rolling"
  
  idString = paste("@fundId", id, sep = " = ")
  startString = paste("@startDate",paste("'", as.character.Date(start), "'", sep = ""), sep = " = ")
  endString = paste("@endDate",paste("'", as.character.Date(end), "'", sep = ""), sep = " = ")
  
  params = paste(idString, startString, endString, sep = ", ")
 
  result = executeSP(proc, params)
  return(xts(result[,2:ncol(result)], order.by = as.Date.factor(result$DateReported)))
}
  