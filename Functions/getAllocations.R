getAllocations = function(asOfDate=NULL){
  proc = "usp_Allocations"
  params = list(MF=777, MN=782, ED=783, LSD=784, LSE=785)
  params = lapply(params, function(x)return(paste("@fundId", x, sep = " = ")))
  if(!is.null(asOfDate)){
      dateString = paste("@asofDate",paste("'", as.character.Date(asOfDate), "'", sep = ""), sep = " = ")
      params = lapply(params, function(x)return(paste(x, dateString, sep = ", ")))
  }
  result = lapply(params, function(x)return(executeSP(proc, x)))
  return(lapply(result, function(x)return(x[,-which(names(x)=="Fund_UID")])))
}
  