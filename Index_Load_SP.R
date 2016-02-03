library(RODBC)
cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
library(PerformanceAnalytics)
library(xts)
sql = "SELECT CAST(v.DateReported AS date) 'DateReported', v.Ticker, v.PX_LAST FROM v_Index_Px_Daily AS v"
indices = sqlQuery(cn, sql)

