loadSubPort = function(id, date)
{
  idStr = paste0("@subId = ", id)
  dtStr = paste0("@date = '", date, "'")
  
  pString = paste(idStr, dtStr, sep = ", ")
  proc = "usp_Portfolio_Sub"
  
  return(executeSP(proc, pString))
}

# lapply(subNames, FUN = function(nm)return(loadSubPort(id=getSubID(nm),date =  "2016-04-29"))) -> sub.ports
# names(sub.ports) = subNames