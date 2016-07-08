topPositionUFT = function(uftId)
{
  #library(RODBC)
  library(data.table)
  library(xts)
  library(PerformanceAnalytics)
  #cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  res = executeSP(procname = "usp_Top_Position_UFT", paramstring = paste0("@id = ", uftId))
  res =xts(na.fill(res[,c("Long", "Short")], fill=0), order.by = as.Date.factor(res$DateReported))
  return(res)
}