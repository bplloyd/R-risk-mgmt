getAllocations_Rolling = function(start, end){
  
  require(lubridate)
  
  proc = "usp_Allocations_Rolling"
  makeParams = function(id){
    
    idString = paste("@fundId", id, sep = " = ")
    startString = paste("@startDate",paste("'", as.character.Date(start), "'", sep = ""), sep = " = ")
    endString = paste("@endDate",paste("'", as.character.Date(end), "'", sep = ""), sep = " = ")
  
    return(paste(idString, startString, endString, sep = ", "))
  }
  
  result = lapply(c(777, 782, 783, 784, 785), FUN = function(id){res = executeSP(proc, makeParams(id));
                                                                  return(xts(na.fill(res[,2:ncol(res)],fill=0), order.by = ymd(as.Date.factor(res$DateReported))))
                                                                }
                  )
  names(result) = c("MF", "MN", "ED", "LSD", "LSE")
  return(result)
}
  