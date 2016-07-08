loadSubPort = function(id, date)
{
  idStr = paste0("@subId = ", id)
  dtStr = paste0("@date = '", date, "'")
  
  pString = paste(idStr, dtStr, sep = ", ")
  proc = "usp_Portfolio_Sub"
  
  return(executeSP(proc, pString))
}
# subNames = unlist(sapply(subs.o, FUN = function(x)return(names(x))))
# lapply(subNames, FUN = function(nm)return(loadSubPort(id=getSubID(nm),date =  "2016-05-31"))) -> sub.ports
# names(sub.ports) = subNames
# 
# saveRDS(sub.ports, file = "G:\\PORTFOLIO MANGEMENT\\Bryan Lloyd\\2016 Projects\\Data\\SubPorts\\SubPorts_20160531.rds")
