sectorExposure3 = function(id, optionMode = 0)
{
  library(RODBC)
  library(data.table)
  library(xts)
  library(PerformanceAnalytics)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  
#   id = 785
#   optionMode = 0

  sql = paste0("
                SELECT
                    e.DateReported
                    , e.Sector
                    , e.Net_Exposure
                FROM
                    MandateData.Sector_Exposure AS e
                WHERE 
                    e.Account_ID = ", id)
 
  sql = paste0(sql,  " AND e.DeltaAdjusted = ", optionMode, "
                ORDER BY
                    e.DateReported")
  
  res = sqlQuery(cn, sql)
  res$DateReported = as.Date.factor(res$DateReported)
  return(res)
}