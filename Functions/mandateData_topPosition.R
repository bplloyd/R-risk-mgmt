mandateData_topPosition = function(id)
{
  #library(RODBC)
  library(data.table)
  library(xts)
  library(PerformanceAnalytics)
  #cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  res = executeSP(procname = "usp_Top_Position", paramstring = paste0("@id = ", id))
  res =xts(na.fill(res[,c("Long", "Short")], fill=0), order.by = as.Date.factor(res$DateReported))
  return(res)
}