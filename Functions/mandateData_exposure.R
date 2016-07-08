mandateData_exposure = function(id, deltaAdj = 0)
{
  #library(RODBC)
  library(data.table)
  library(xts)
  library(PerformanceAnalytics)
  #cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")

  parID = paste0("@id = ", id)
  parDA = paste0("@exposureMode = ", deltaAdj)
  res = executeSP(procname = "usp_Exposure_Rolling", paramstring = paste(parID, parDA, sep = ", "))
  res =xts(res[,2:ncol(res)], order.by = as.Date.factor(res$DateReported))
  return(res)
}