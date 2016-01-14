library(RODBC)
cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
library(PerformanceAnalytics)
library(xts)
sql = "SELECT CAST(v.DateReported AS date) 'DateReported', v.Ticker, v.PX_LAST FROM v_Index_Px_Daily AS v"
index.result = sqlQuery(cn, sql) %>% melt(., id.vars = 1:2) %>% dcast(., DateReported ~ Ticker, max)

