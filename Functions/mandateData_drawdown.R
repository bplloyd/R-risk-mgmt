mandateData_drawdown = function(id, fcid = NULL)
{
  #library(RODBC)
  library(data.table)
  library(xts)
  library(PerformanceAnalytics)
  #cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  if(is.null(fcid))
  {
    if(id == 786)
      fcid = 4
    else if(id == 774)
      fcid = 5
    else
      fcid = 0
  }
  parID = paste0("@id = ", id)
  parFCID = paste0("@fcid = ", fcid)
  res = executeSP(procname = "usp_Drawdown", paramstring = paste(parID, parFCID, sep = ", "))
  res =xts(res[,2:ncol(res)], order.by = as.Date.factor(res$DateReported))
  return(res)
}