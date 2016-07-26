loadPort = function(id, date)
{
  if(id < 700)
  {
    idStr = paste0("@subId = ", id)
    dtStr = paste0("@date = '", date, "'")
    
    pString = paste(idStr, dtStr, sep = ", ")
    proc = "usp_Portfolio_Sub"
  }
  if(id %in% c(777, 782, 783, 784, 785))
  {
    idStr = paste0("@id = ", id)
    dtStr = paste0("@date = '", date, "'")
    
    pString = paste(idStr, dtStr, sep = ", ")
    proc = "usp_Portfolio_UFT"
  }
  gc()
  return(executeSP(proc, pString))
}
# subNames = unlist(sapply(subs.o, FUN = function(x)return(names(x))))
# lapply(subNames, FUN = function(nm)return(loadSubPort(id=getSubID(nm),date =  "2016-05-31"))) -> sub.ports
# names(sub.ports) = subNames
# 
# saveRDS(sub.ports, file = "G:\\PORTFOLIO MANGEMENT\\Bryan Lloyd\\2016 Projects\\Data\\SubPorts\\SubPorts_20160531.rds")
